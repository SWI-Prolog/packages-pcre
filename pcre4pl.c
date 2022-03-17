/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2022, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* #define O_DEBUG 1 */
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <pcre.h>

		 /*******************************
		 *	      RE STRUCT		*
		 *******************************/

typedef enum cap_type /* capture type */
{ CAP_DEFAULT = 0,
  CAP_STRING,
  CAP_ATOM,
  CAP_INTEGER,
  CAP_FLOAT,
  CAP_NUMBER,
  CAP_TERM,
  CAP_RANGE
} cap_type;

/* For debugging - map cap_type to string */
static const char *
cap_type_str(int i)
{ switch( i )
  { case CAP_DEFAULT: return "CAP_DEFAULT";
    case CAP_STRING:  return "CAP_STRING";
    case CAP_ATOM:    return "CAP_ATOM";
    case CAP_INTEGER: return "CAP_INTEGER";
    case CAP_FLOAT:   return "CAP_FLOAT";
    case CAP_NUMBER:  return "CAP_NUMBER";
    case CAP_TERM:    return "CAP_TERM";
    case CAP_RANGE:   return "CAP_RANGE";
    default:	      return "CAP_???";
  }
}

typedef struct cap_how /* capture "how" [name + type] */
{ atom_t	name; /* 0 if it's unnamed (referenced by number position) */
  cap_type	type;
} cap_how;

/* When processing options, only the first value for a flag is used;
   any subsequent values are ignored. The `seen` field tracks this.
   Some flags use multiple bits (`bsr` and `newline`) - to handle
   these, the set_flag() function has a `mask` parameter.
   This struct should be initialized to {0,0}. */
typedef struct re_options_flags
{ uint32_t seen;
  uint32_t flags;
} re_options_flags;

typedef struct re_data
{ atom_t	    symbol;		/* regex as blob */
  atom_t	    pattern;		/* pattern (as atom) */
  re_options_flags  options_flags;	/* compilation options */
  int		    capture_size;	/* # captured subpatterns */
  cap_how          *capture_names;	/* Names for captured data */
  cap_type	    capture_type;	/* Default return for capture */
  pcre	           *pcre;		/* the compiled expression */
  pcre_extra       *extra;		/* study result (NULL if not RE_STUDY) */
} re_data;


static void
write_re_options(IOSTREAM *s, const char* sep, int re_options);


static void
re_free(re_data *re)
{ if ( re->pattern )
    PL_unregister_atom(re->pattern);
  if ( re->pcre )
    pcre_free(re->pcre);
  if ( re->extra )
    pcre_free_study(re->extra);
  if ( re->capture_names )
  { int i;

    for(i=0; i<re->capture_size+1; i++)
    { if ( re->capture_names[i].name )
	PL_unregister_atom(re->capture_names[i].name);
    }

    free(re->capture_names);
  }

  PL_free(re);
}


static functor_t FUNCTOR_pair2;		/* -/2 */

/* Each ATOM_xxx definition has a corresponding MKATOM(xxx) in
   install_pcre4pl(). */
static atom_t ATOM_optimise;
static atom_t ATOM_optimize;
static atom_t ATOM_bsr;
static atom_t ATOM_compat;
static atom_t ATOM_javascript;
static atom_t ATOM_newline;
static atom_t ATOM_start;
static atom_t ATOM_capture_type;
static atom_t ATOM_string;
static atom_t ATOM_atom;
static atom_t ATOM_term;
static atom_t ATOM_range;
static atom_t ATOM_version;


		 /*******************************
		 *	  SYMBOL WRAPPER	*
		 *******************************/

static void
acquire_pcre(atom_t symbol)
{ re_data *re = *(re_data**)PL_blob_data(symbol, NULL, NULL);

  re->symbol = symbol;
}


static int
release_pcre(atom_t symbol)
{ re_data *re = *(re_data**)PL_blob_data(symbol, NULL, NULL);

  re_free(re);

  return TRUE;
}


static int
compare_pcres(atom_t a, atom_t b)
{ re_data *rea = *(re_data**)PL_blob_data(a, NULL, NULL);
  re_data *reb = *(re_data**)PL_blob_data(b, NULL, NULL);

  return ( rea > reb ?  1 :
	   reb < rea ? -1 : 0
	 );
}


static int
write_pcre(IOSTREAM *s, atom_t symbol, int flags)
{ (void)flags; /* unused */
  re_data *re = *(re_data**)PL_blob_data(symbol, NULL, NULL);
  Sfprintf(s, "<regex>(%p)", re);	/* For blob details: re_portray() - re_portray/2 */
  return TRUE;
}


static PL_blob_t pcre_blob =
{ PL_BLOB_MAGIC,
  0,
  "regex",
  release_pcre,
  compare_pcres,
  write_pcre,
  acquire_pcre
};


		 /*******************************
		 *	      UTF8 UTIL		*
		 *******************************/

typedef struct re_subject
{ char	     *subject;			/* Subject string */
  size_t      length;			/* Total length in bytes */
  size_t      charp;			/* Character position */
  size_t      bytep;			/* Byte position */
  unsigned    flags;			/* Allocation flags */
} re_subject;


#define ISUTF8_CB(c)  (((c)&0xc0) == 0x80) /* Is continuation byte */

static inline char *
utf8_skip_char(const char *in)
{ if ( !(in[0]&0x80) )
  { return (char*)in+1;
  } else
  { in++;
    while ( ISUTF8_CB(in[0]) )
      in++;
    return (char*)in;
  }
}

static size_t
utf8_seek(const char *subject, size_t len, size_t offset)
{ const char *s = subject;
  const char *e = subject+len;

  for(; offset > 0; offset--)
  { s = utf8_skip_char(s);
    if ( s >= e )
      return (size_t)-1;
  }

  return s - subject;
}

static size_t
char_offset(const char *subject, size_t byte_offset)
{ size_t co;
  const char *e = subject+byte_offset;

  for(co=0; subject < e; subject = utf8_skip_char(subject))
    co++;

  return co;
}

static size_t
bytep_to_charp(re_subject *subj, size_t bytep)
{ if ( subj->bytep > bytep )
  { subj->bytep = subj->charp = 0;
  }

  subj->charp += char_offset(subj->subject+subj->bytep, bytep-subj->bytep);
  subj->bytep = bytep;

  return subj->charp;
}

#define SUBJ_FLAGS (CVT_ATOM|CVT_STRING|CVT_LIST|REP_UTF8|CVT_EXCEPTION)

static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
re_get_subject(term_t t, re_subject *subj, unsigned flags)
{ memset(subj, 0, sizeof *subj); /* { NULL, 0, 0, 0, 0 }; */

  subj->flags = flags;
  return PL_get_nchars(t, &subj->length, &subj->subject, flags|SUBJ_FLAGS);
}

static void
re_free_subject(re_subject *subj)
{ (void)subj; /* unused */
}



		 /*******************************
		 *	   PROLOG EXCHANGE	*
		 *******************************/

static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
get_re(term_t t, re_data **re)
{ void *p;
  size_t len;
  PL_blob_t *type;

  if ( PL_get_blob(t, &p, &len, &type) && type == &pcre_blob )
  { re_data **rep = p;
    *re = *rep;
    return TRUE;
  }

  *re = NULL;
  return PL_type_error("regex", t);
}


#define RE_STUDY 0x0001

static int /* FALSE/TRUE or -1 for error */
effective_bool(term_t arg)
{ if ( arg )
  { int v;
    if ( PL_get_bool_ex(arg, &v) )
      return v;
    return -1; /* Error: neither FALSE nor TRUE */
  }
  return TRUE;
}

static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
/* The `mask` has 1 for where the value can be applied; `value` is the
   flag value (e.g., from re_optdefs[i].flag) and is never zero. `arg`
   is usesd to determine whether the flag is set to `value` or its
   inverse, and `invert` uses the inverse of `value`.  */
set_flag(term_t arg, re_options_flags *options_flags, uint32_t mask, uint32_t value, int invert)
{ if ( options_flags->seen&mask )
    return TRUE; /* repeated option - ignore */
  options_flags->flags &= ~mask; /* zero out where the value goes */
  options_flags->seen |= mask;
  switch( effective_bool(arg) )
  { case TRUE:
      if ( invert )
	options_flags->flags &= ~value;
      else
	options_flags->flags |= value;
      break;
    case FALSE:
      if ( invert )
	options_flags->flags |= value;
      else
	options_flags->flags &= ~value;
      break;
    default: /* -1 */
      return FALSE;
  }
  return TRUE;
}


typedef struct re_optdef
{ const char *name;
  unsigned    flag;
  unsigned    mode; /* RE_COMP, RE_EXEC, RE_NEG */
  atom_t      atom; /* Initially 0; filled in as-needed by lookup_optdef() */
} re_optdef;

#define RE_COMP 0x001
#define RE_EXEC 0x002
#define RE_NEG  0x004


static const re_optdef*
lookup_optdef(re_optdef opt_defs[], atom_t name, unsigned mode)
{ re_optdef *def;
  for(def=opt_defs; def->name; def++)
  { if ( !def->atom ) /* lazily fill in atoms in lookup table */
      def->atom = PL_new_atom(def->name);
    if ( def->atom == name && (def->mode&mode) )
      return def;
  }
  return NULL;
}


static int/* bool (FALSE/TRUE), as returned by PL_..._error() */
lookup_and_apply_optdef(re_optdef opt_defs[], atom_t name, unsigned mode,
			term_t option_term, term_t arg, int mask,
                        re_options_flags *options_flags)
{ const re_optdef *def = lookup_optdef(opt_defs, name, mode);
  if ( def )
    return set_flag(arg, options_flags, mask, def->flag, def->mode&RE_NEG);
  return PL_type_error("option", option_term);
}


static re_optdef re_optdefs[] =
{ { "anchored",	       PCRE_ANCHORED,	      RE_COMP|RE_EXEC },
  { "auto_capture",    PCRE_NO_AUTO_CAPTURE,  RE_COMP|RE_NEG },
  { "bol",	       PCRE_NOTBOL,	      RE_EXEC|RE_NEG },
  { "caseless",	       PCRE_CASELESS,	      RE_COMP },
  { "dollar_endonly",  PCRE_DOLLAR_ENDONLY,   RE_COMP },
  { "dotall",	       PCRE_DOTALL,	      RE_COMP },
  { "dupnames",	       PCRE_DUPNAMES,	      RE_COMP },
  { "empty",	       PCRE_NOTEMPTY,	      RE_EXEC|RE_NEG },
  { "empty_atstart",   PCRE_NOTEMPTY_ATSTART, RE_EXEC|RE_NEG },
  { "eol",	       PCRE_NOTEOL,	      RE_EXEC|RE_NEG },
  { "extended",	       PCRE_EXTENDED,	      RE_COMP },
  { "extra",	       PCRE_EXTRA,	      RE_COMP },
  { "firstline",       PCRE_FIRSTLINE,	      RE_COMP },
  { "greedy",	       PCRE_UNGREEDY,	      RE_COMP|RE_NEG },
  { "multiline",       PCRE_MULTILINE,	      RE_COMP },
  { "no_auto_capture", PCRE_NO_AUTO_CAPTURE,  RE_COMP }, /* backwards compatibility */
  { "ucp",	       PCRE_UCP,	      RE_COMP },
  { "ungreedy",	       PCRE_UNGREEDY,	      RE_COMP }, /* backwards compatibility */
  { NULL, 0}
};

static re_optdef re_optbsrs[] = /* TODO: verify RE_EXEC: */
{ { "anycrlf",	       PCRE_BSR_ANYCRLF,      RE_COMP|RE_EXEC },
  { "unicode",	       PCRE_BSR_UNICODE,      RE_COMP|RE_EXEC },
  { NULL }
};

#define OPTBSR_MASK (PCRE_BSR_ANYCRLF|PCRE_BSR_UNICODE)

static re_optdef re_optnewlines[] = /* TODO: verify RE_EXEC: */
{ { "any",	       PCRE_NEWLINE_ANY,      RE_COMP|RE_EXEC },
  { "anycrlf",	       PCRE_NEWLINE_ANYCRLF,  RE_COMP|RE_EXEC },
  { "crlf",	       PCRE_NEWLINE_CRLF,     RE_COMP|RE_EXEC },
  { "lf",	       PCRE_NEWLINE_LF,	      RE_COMP|RE_EXEC },
  { "cr",	       PCRE_NEWLINE_CR,	      RE_COMP|RE_EXEC },
  { NULL }
};

#define OPTNEWLINE_MASK (PCRE_NEWLINE_CR|PCRE_NEWLINE_LF|PCRE_NEWLINE_CRLF|PCRE_NEWLINE_ANY|PCRE_NEWLINE_ANYCRLF)

static int /* bool (FALSE/TRUE), as returned by PL_..._error() */
get_arg_1_if_any(term_t head, atom_t *name, size_t *arity, term_t *arg)
{ if ( PL_get_name_arity(head, name, arity) && *arity <= 1 )
  { term_t argt = PL_new_term_ref();
    if ( *arity == 1 )
    { _PL_get_arg(1, head, argt);
      *arg = argt;
    } else
    { *arg = 0;
    }
    return TRUE;
  }
  *arg = 0; /* Make compiler's flow analysis happy */
  return PL_type_error("option", head);
}


static int /* bool (FALSE/TRUE), as returned by PL_..._error() */
re_get_options(term_t options, unsigned mode, re_options_flags *options_flags,
	       int (*func)(atom_t o, term_t a, void *ctx),
	       void *ctx)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  re_options_flags bsr_option = {0,0};
  re_options_flags newline_option = {0,0};
  options_flags->seen = 0;
  options_flags->flags = PCRE_NO_UTF8_CHECK;
  newline_option.flags = PCRE_NEWLINE_ANYCRLF;

  while(PL_get_list_ex(tail, head, tail))
  { atom_t name;
    size_t arity;
    term_t arg;
    if ( !get_arg_1_if_any(head, &name, &arity, &arg) )
      return FALSE;
    if ( name == ATOM_bsr && arg && (mode&RE_COMP) )
    { atom_t aval;
      if ( !PL_get_atom_ex(arg, &aval) ||
	   !lookup_and_apply_optdef(re_optbsrs, aval, RE_COMP,
				    head, 0, OPTBSR_MASK, &bsr_option) )
	return FALSE;
    } else if ( name == ATOM_newline && arg && (mode&RE_COMP) )
    { atom_t aval;
      if (!PL_get_atom_ex(arg, &aval) ||
	  !lookup_and_apply_optdef(re_optnewlines, aval, RE_COMP,
				   head, 0, OPTNEWLINE_MASK, &newline_option) )
	return FALSE;
    } else if ( name == ATOM_compat && arg && (mode&RE_COMP) && !(options_flags->seen&PCRE_JAVASCRIPT_COMPAT) )
    { atom_t aval;
      options_flags->seen |= PCRE_JAVASCRIPT_COMPAT;
      /* No need for a mask - the only flag option is positive */
      if ( !PL_get_atom_ex(arg, &aval) )
	return FALSE;
      if ( aval == ATOM_javascript )
	options_flags->flags |= PCRE_JAVASCRIPT_COMPAT;
      else
	return PL_domain_error("compat_option", arg);
    } else
    { const re_optdef *def = lookup_optdef(re_optdefs, name, mode);
      if ( def )
      { if ( !set_flag(arg, options_flags, def->flag, def->flag, def->mode&RE_NEG) )
	  return FALSE;
      } else if ( func )
	{ if ( !(*func)(name, arg, ctx) )
	    return FALSE;
	} else
	{  return PL_type_error("option", head);
	}
    }
  }

  options_flags->flags |= bsr_option.flags | newline_option.flags;
  return PL_get_nil_ex(tail);
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

typedef enum re_config_type
{ CFG_INTEGER,
  CFG_BOOL,
  CFG_STRING
} re_config_type;


typedef struct re_config_opt
{ char		 *name;
  int		  id;
  re_config_type  type;
  atom_t	  atom; /* Initially 0; filled in as-needed by re_config() */
} re_config_opt;

static re_config_opt cfg_opts[] =
{ { "bsr",		      PCRE_CONFIG_BSR,			  CFG_INTEGER },
  { "jit",		      PCRE_CONFIG_JIT,			  CFG_BOOL },
  { "jittarget",	      PCRE_CONFIG_JITTARGET,		  CFG_STRING },
  { "link_size",	      PCRE_CONFIG_LINK_SIZE,		  CFG_INTEGER },
  { "match_limit",	      PCRE_CONFIG_MATCH_LIMIT,		  CFG_INTEGER },
  { "match_limit_recursion",  PCRE_CONFIG_MATCH_LIMIT_RECURSION,  CFG_INTEGER },
  { "newline",		      PCRE_CONFIG_NEWLINE,		  CFG_INTEGER },
  { "posix_malloc_threshold", PCRE_CONFIG_POSIX_MALLOC_THRESHOLD, CFG_INTEGER },
  { "stackrecurse",	      PCRE_CONFIG_STACKRECURSE,		  CFG_BOOL },
  { "unicode_properties",     PCRE_CONFIG_UNICODE_PROPERTIES,	  CFG_BOOL },
  { "utf8",		      PCRE_CONFIG_UTF8,			  CFG_BOOL },
#ifdef PCRE_CONFIG_PARENS_LIMIT
  { "parens_limit",	      PCRE_CONFIG_PARENS_LIMIT,		  CFG_INTEGER },
#endif
  { NULL, 0}
};


/** re_config(+Term)

    For documentation of this function, see pcre.pl
*/
static foreign_t
re_config(term_t opt)
{ atom_t name;
  size_t arity;

  if ( PL_get_name_arity(opt, &name, &arity) )
  { if ( arity == 1 )
    { re_config_opt *o;
      term_t arg = PL_new_term_ref();

      _PL_get_arg(1, opt, arg);

      for(o=cfg_opts; o->name; o++)
      { if ( !o->atom )
	  o->atom = PL_new_atom(o->name);

	if ( o->atom == name )
	{ union
	  { int i;
	    char *s;
	  } val;

	  if ( pcre_config(o->id, &val) == 0 )
	  { switch(o->type)
	    { case CFG_BOOL:
		return PL_unify_bool(arg, val.i);
	      case CFG_INTEGER:
		return PL_unify_integer(arg, val.i);
	      case CFG_STRING:
		return val.s ? PL_unify_atom_chars(arg, val.s) : FALSE;
	      default:
		assert(0);
	    }
	  } else
	  { break;
	  }
	}
      }

      if ( name == ATOM_version )
	return PL_unify_atom_chars(arg, pcre_version());

      return PL_existence_error("re_config", opt);
    }
    return PL_type_error("compound", opt);
  }

  return PL_type_error("compound", opt);
}

static int
set_capture_name_and_type(const char *s, re_data *re, int ci)
{ const char *fs = strrchr(s, '_');
  size_t len;

  assert(ci < re->capture_size+1);

  if ( (fs=strrchr(s, '_')) && fs[1] && !fs[2] )
  { len = fs-s;
    switch(fs[1])
    { case 'S': re->capture_names[ci].type = CAP_STRING;  break;
      case 'A': re->capture_names[ci].type = CAP_ATOM;    break;
      case 'I': re->capture_names[ci].type = CAP_INTEGER; break;
      case 'F': re->capture_names[ci].type = CAP_FLOAT;   break;
      case 'N': re->capture_names[ci].type = CAP_NUMBER;  break;
      case 'T': re->capture_names[ci].type = CAP_TERM;    break;
      case 'R': re->capture_names[ci].type = CAP_RANGE;   break;
      default:
      { term_t ex;
	return ( (ex=PL_new_term_ref()) &&
		 PL_put_atom_chars(ex, &fs[1]) &&
		 PL_existence_error("re_type_flag", ex) );
      }
    }
  } else
  { len = (size_t)-1; /* nul-terminated string */
    re->capture_names[ci].type = re->capture_type;
  }
  if ( !(re->capture_names[ci].name = PL_new_atom_mbchars(REP_UTF8, len, s)) )
    return FALSE;
  return TRUE;
}


static int /* bool (FALSE/TRUE), as returned by PL_..._error() */
init_capture_map(re_data *re)
{ int name_count;
  int name_entry_size;
  const char *table;
  int i;
  if ( pcre_fullinfo(re->pcre, re->extra, PCRE_INFO_CAPTURECOUNT,  &re->capture_size)!=0 ||
       pcre_fullinfo(re->pcre, re->extra, PCRE_INFO_NAMECOUNT,     &name_count)      !=0 ||
       pcre_fullinfo(re->pcre, re->extra, PCRE_INFO_NAMEENTRYSIZE, &name_entry_size) !=0 ||
       pcre_fullinfo(re->pcre, re->extra, PCRE_INFO_NAMETABLE,     &table)           !=0 )
    return PL_resource_error("pcre_fullinfo");
  if ( ! (re->capture_names = malloc((re->capture_size+1) * sizeof (cap_how))) )
    return PL_resource_error("memory");
  for(i=0; i<re->capture_size+1; i++)
  { re->capture_names[i].name = 0;
    re->capture_names[i].type = re->capture_type;
  }
  for(i=0; i<name_count; i++, table += name_entry_size)
  { if ( !set_capture_name_and_type(&table[2], re,
				    ((table[0]&0xff)<<8) + (table[1]&0xff)) )
      return FALSE;
  }
  return TRUE;
}


typedef struct re_compile_options
{ unsigned	flags; /* RE_STUDY */
  cap_type	capture_type;
  unsigned	seen_flags;
  unsigned	seen_cap;
} re_compile_options;


static int /* Parameter to re_get_options() - bool (FALSE/TRUE), as returned by PL_get_...() etc  */
re_compile_opt(atom_t opt, term_t arg, void *ctx)
{ re_compile_options *copts = ctx;
  if ( opt == ATOM_optimise || opt == ATOM_optimize )
  { if ( copts->seen_flags&RE_STUDY )
      return TRUE;
    copts->seen_flags |= RE_STUDY;
    switch(effective_bool(arg))
    { case TRUE:
	copts->flags |= RE_STUDY;
	return TRUE;
      case FALSE:
	copts->flags &= ~RE_STUDY;
	return TRUE;
      default:
	return FALSE;
    }
  } else if ( opt == ATOM_capture_type && arg )
  { atom_t aval;

    if ( !PL_get_atom_ex(arg, &aval) )
      return FALSE;

    if ( copts->seen_cap )
      return TRUE;
    copts->seen_cap = TRUE;

    if ( aval == ATOM_range )
      copts->capture_type = CAP_RANGE;
    else if ( aval == ATOM_atom )
      copts->capture_type = CAP_ATOM;
    else if ( aval == ATOM_string )
      copts->capture_type = CAP_STRING;
    else if ( aval == ATOM_term )
      copts->capture_type = CAP_TERM;
    else
    { return PL_domain_error("capture_type", arg);
    }
    return TRUE;
  }

  return TRUE;
}


static void
write_re_options(IOSTREAM *s, const char *sep, int re_options)
{
  /* The following were extracted from pcre.h and sorted, with bsr and newline at
     the end because they're multi-valued: */

  if ( re_options & PCRE_ANCHORED )	     { Sfprintf(s, "%s%s", sep, "ANCHORED");				 sep = " "; }
  if ( re_options & PCRE_AUTO_CALLOUT )	     { Sfprintf(s, "%s%s", sep, "AUTO_CALLOUT");			 sep = " "; }
  if ( re_options & PCRE_CASELESS )	     { Sfprintf(s, "%s%s", sep, "CASELESS");				 sep = " "; }
  if ( re_options & PCRE_DOLLAR_ENDONLY )    { Sfprintf(s, "%s%s", sep, "DOLLAR_ENDONLY");			 sep = " "; }
  if ( re_options & PCRE_DOTALL )	     { Sfprintf(s, "%s%s", sep, "DOTALL");				 sep = " "; }
  if ( re_options & PCRE_DUPNAMES )	     { Sfprintf(s, "%s%s", sep, "DUPNAMES");				 sep = " "; }
  if ( re_options & PCRE_EXTENDED )	     { Sfprintf(s, "%s%s", sep, "EXTENDED");				 sep = " "; }
  if ( re_options & PCRE_EXTRA )	     { Sfprintf(s, "%s%s", sep, "EXTRA");				 sep = " "; }
  if ( re_options & PCRE_FIRSTLINE )	     { Sfprintf(s, "%s%s", sep, "FIRSTLINE");				 sep = " "; }
  if ( re_options & PCRE_JAVASCRIPT_COMPAT ) { Sfprintf(s, "%s%s", sep, "JAVASCRIPT_COMPAT");			 sep = " "; }
  if ( re_options & PCRE_MULTILINE )	     { Sfprintf(s, "%s%s", sep, "MULTILINE");				 sep = " "; }
  if ( re_options & PCRE_NEVER_UTF )	     { Sfprintf(s, "%s%s", sep, "NEVER_UTF");				 sep = " "; } /* PCRE_DFA_SHORTEST (overlay ) */

  if ( re_options & PCRE_NOTBOL )	     { Sfprintf(s, "%s%s", sep, "NOTBOL");				 sep = " "; }
  if ( re_options & PCRE_NOTEMPTY )	     { Sfprintf(s, "%s%s", sep, "NOTEMPTY");				 sep = " "; }
  if ( re_options & PCRE_NOTEMPTY_ATSTART )  { Sfprintf(s, "%s%s", sep, "NOTEMPTY_ATSTART");			 sep = " "; }
  if ( re_options & PCRE_NOTEOL )	     { Sfprintf(s, "%s%s", sep, "NOTEOL");				 sep = " "; }
  if ( re_options & PCRE_NO_AUTO_CAPTURE )   { Sfprintf(s, "%s%s", sep, "NO_AUTO_CAPTURE");			 sep = " "; }
  if ( re_options & PCRE_NO_AUTO_POSSESS )   { Sfprintf(s, "%s%s", sep, "NO_AUTO_POSSESS");			 sep = " "; } /* PCRE_DFA_RESTART (overlay ) */
  if ( re_options & PCRE_NO_START_OPTIMIZE ) { Sfprintf(s, "%s%s", sep, "NO_START_OPTIMIZE");			 sep = " "; } /*  PCRE_NO_START_OPTIMISE (synonym ) */
  if ( re_options & PCRE_NO_UTF8_CHECK )     { Sfprintf(s, "%s%s", sep, "NO_UTF8_CHECK");			 sep = " "; } /* PCRE_NO_UTF16_CHECK, PCRE_NO_UTF32_CHECK (synonym ) */
  if ( re_options & PCRE_PARTIAL_HARD )	     { Sfprintf(s, "%s%s", sep, "PARTIAL_HARD");			 sep = " "; }
  if ( re_options & PCRE_PARTIAL_SOFT )	     { Sfprintf(s, "%s%s", sep, "PARTIAL_SOFT");			 sep = " "; } /* PCRE_PARTIAL (synonym ) */
  if ( re_options & PCRE_UCP )		     { Sfprintf(s, "%s%s", sep, "UCP");					 sep = " "; }
  if ( re_options & PCRE_UNGREEDY )	     { Sfprintf(s, "%s%s", sep, "UNGREEDY");				 sep = " "; }
  if ( re_options & PCRE_UTF8 )		     { Sfprintf(s, "%s%s", sep, "UTF8");				 sep = " "; } /* PCRE_UTF16, PCRE_UTF32 (synonym ) */

  if ( (re_options & OPTBSR_MASK)     == PCRE_BSR_ANYCRLF )	  { Sfprintf(s, "%s%s", sep, "BSR_ANYCRLF");	 sep = " "; }
  if ( (re_options & OPTBSR_MASK)     == PCRE_BSR_UNICODE )	  { Sfprintf(s, "%s%s", sep, "BSR_UNICODE");	 sep = " "; }

  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_CR )	  { Sfprintf(s, "%s%s", sep, "NEWLINE_CR");	 sep = " "; }
  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_LF )	  { Sfprintf(s, "%s%s", sep, "NEWLINE_LF");	 sep = " "; }
  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_CRLF )	  { Sfprintf(s, "%s%s", sep, "NEWLINE_CRLF");	 sep = " "; }
  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_ANY )	  { Sfprintf(s, "%s%s", sep, "NEWLINE_ANY");	 sep = " "; }
  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_ANYCRLF )	  { Sfprintf(s, "%s%s", sep, "NEWLINE_ANYCRLF"); sep = " "; }
}


/** re_portray(+Stream, +Regex) is det.

    Output a debug string for the regexp (from re_compile)
    ('$re_match_options'/2 handles the match options)
*/
static foreign_t
re_portray(term_t stream, term_t regex)
{ IOSTREAM *fd;
  re_data *re;
  if ( !PL_get_stream(stream, &fd, SIO_OUTPUT) || !PL_acquire_stream(fd) )
    return FALSE;
  if ( !get_re(regex, &re) )
    return FALSE;
  Sfprintf(fd, "<regex>(/%s/ [", PL_atom_chars(re->pattern));
  write_re_options(fd, "", re->options_flags.flags);
  /* TODO: compile_opts&RE_STUDY is in a flag that's not in re_data */
  Sfprintf(fd, " %s] $capture=%d", cap_type_str(re->capture_type), re->capture_size);
  if ( re->capture_size && re->capture_names )
  { int i;
    const char *sep = "";
    Sfprintf(fd, " {", re->capture_size);
    for(i=0; i<re->capture_size+1; i++)
    { if ( re->capture_names[i].name )
      { Sfprintf(fd, "%s%d:%s:%s", sep, i, PL_atom_chars(re->capture_names[i].name), cap_type_str(re->capture_names[i].type));
	sep = " ";
      } else
      { Sfprintf(fd, "%s%d:%s", sep, i, cap_type_str(re->capture_names[i].type));
	sep = " ";
      }
    }
    Sfprintf(fd, "}");
  } else
  { /* Sfprintf(fd, " no-capture"); */
  }
  Sfprintf(fd, ")");

  return PL_release_stream(fd);
}


/** re_compile(+Pattern, -Regex, +Options) is det.

    For documentation of this function, see pcre.pl
*/
static foreign_t
re_compile(term_t pat, term_t reb, term_t options)
{ size_t len;
  char *pats;
  re_compile_options copts = {0, CAP_STRING};
  int re_error_code;
  const char *re_error_msg;
  int re_error_offset;
  const unsigned char *tableptr = NULL;
  re_data re;
  memset(&re, 0, sizeof re);

  if ( !re_get_options(options, RE_COMP, &re.options_flags, re_compile_opt, &copts) )
    return FALSE;
  re.capture_type = copts.capture_type;
  re.options_flags.flags |= PCRE_UTF8|PCRE_NO_UTF8_CHECK;

  if ( !PL_get_nchars(pat, &len, &pats, CVT_ATOM|CVT_STRING|CVT_LIST|REP_UTF8|CVT_EXCEPTION) )
    return FALSE;
  if ( strlen(pats) != len )		/* TBD: escape as \0xx */
    return PL_representation_error("nul_byte");

  if ( (re.pcre = pcre_compile2(pats, re.options_flags.flags,
				&re_error_code, &re_error_msg, &re_error_offset,
				tableptr) ) )
  { if ( copts.flags & RE_STUDY )
    { re.extra = pcre_study(re.pcre, 0, &re_error_msg);
					/* TBD: handle error */
    }
    if ( !init_capture_map(&re) )
    { /* init_capture_map() has called an appropriate PL_..._error()
	 to indicate the cause; PL_..._error() returns FALSE, so
	 return that value. */
      return FALSE;
    }

    if ( (PL_get_atom(pat, &re.pattern)) )
      PL_register_atom(re.pattern);
    else
      re.pattern = PL_new_atom_mbchars(REP_UTF8, len, pats);
    { re_data *re_blob_ptr = PL_malloc(sizeof re);
      if ( re_blob_ptr)
      { *re_blob_ptr = re;
	return PL_unify_blob(reb, &re_blob_ptr, sizeof re_blob_ptr, &pcre_blob);
      }
      return FALSE;
    }
  } else
  { return PL_syntax_error(re_error_msg, NULL); /* TBD: location, code */
  }
}



typedef struct matchopts
{ size_t   start;
  unsigned seen_start;
} matchopts;


static int /* Parameter to re_get_options() - bool (FALSE/TRUE), as returned by PL_get_...() etc  */
re_match_opt(atom_t opt, term_t arg, void *ctx)
{ matchopts *opts = ctx;

  if ( arg && opt == ATOM_start )
  { if ( opts->seen_start )
      return TRUE;
    opts->seen_start = TRUE;
    return PL_get_size_ex(arg, &opts->start);
  }

  return TRUE;
}


/* This function probably isn't needed -- it's just in case the re_options have
   somehow incorporated the compile options. */
static void
re_match_opt_postprocess(re_options_flags *options_flags)
{ /* We must *not* "or" the options from regex with the
     match options -- see the pcre_exec() documentation.
       "The only bits that may be set are
	   PCRE_ANCHORED,
	   PCRE_NEWLINE_xxx,
	   PCRE_NOTBOL,
	   PCRE_NOTEOL,
	   PCRE_NOTEMPTY,
	   PCRE_NOTEMPTY_ATSTART,
	   PCRE_NO_START_OPTIMIZE,
	   PCRE_NO_UTF8_CHECK,
	   PCRE_PARTIAL_HARD,
	   and PCRE_PARTIAL_SOFT."
  */
  options_flags->flags &= (PCRE_ANCHORED|PCRE_NEWLINE_CR|PCRE_NEWLINE_LF|
			   PCRE_NEWLINE_CRLF|PCRE_NEWLINE_ANY|PCRE_NEWLINE_ANYCRLF|
			   PCRE_NOTBOL|PCRE_NOTEOL|PCRE_NOTEMPTY| PCRE_NOTEMPTY_ATSTART|
			   PCRE_NO_START_OPTIMIZE|PCRE_NO_UTF8_CHECK|PCRE_PARTIAL_HARD|
			   PCRE_PARTIAL_SOFT);
}


/** '$re_match_options'(+Stream, +Options) is det.

    Output the Options as a debug string.
    (re_portray/2 handles the compile options)
*/
static foreign_t
re_portray_match_options_(term_t stream, term_t options)
{ IOSTREAM *fd;
  re_options_flags options_flags = {0,0};
  matchopts mopts = {0, FALSE};
  if ( !PL_get_stream(stream, &fd, SIO_OUTPUT) || !PL_acquire_stream(fd) )
    return FALSE;

  if ( !re_get_options(options, RE_EXEC, &options_flags,
		       re_match_opt, &mopts) )
    return FALSE;
  re_match_opt_postprocess(&options_flags);

  write_re_options(fd, "", options_flags.flags);
  Sfprintf(fd, " $start=%lu", mopts.start);
  return PL_release_stream(fd);
}


static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
out_of_range(size_t index)
{ term_t ex;

  return ( (ex=PL_new_term_ref()) &&
	   PL_put_int64(ex, index) &&
	   PL_domain_error("offset", ex) );
}


static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
put_capname(term_t t, re_data *re, int i)
{ atom_t name;

  if ( re->capture_names && (name=re->capture_names[i].name) )
    return PL_put_atom(t, name);
  else
    return PL_put_integer(t, i);
}


static int  /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
put_capval(term_t t, re_data *re, re_subject *subject, int i, const int ovector[])
{ const char *s = &subject->subject[ovector[i*2]];
  int len = ovector[i*2+1]-ovector[i*2];
  cap_type ctype = re->capture_type;

  if ( re->capture_names && re->capture_names[i].type )
    ctype = re->capture_names[i].type;

  switch(ctype)
  { case CAP_ATOM:
      return PL_put_chars(t, REP_UTF8|PL_ATOM, len, s);
    case CAP_STRING:
      return PL_put_chars(t, REP_UTF8|PL_STRING, len, s);
    case CAP_INTEGER:
    case CAP_FLOAT:
    case CAP_NUMBER:
    case CAP_TERM:
      return PL_put_term_from_chars(t, REP_UTF8, len, s);
    case CAP_RANGE:
    { term_t av;
      size_t start = bytep_to_charp(subject, ovector[i*2]);
      size_t end   = bytep_to_charp(subject, ovector[i*2+1]);
      int rc = ( (av=PL_new_term_refs(2)) &&
		  PL_put_integer(av+0, start) &&
		  PL_put_integer(av+1, end-start) &&
		  PL_cons_functor_v(t, FUNCTOR_pair2, av) );
      if ( av )
	PL_reset_term_refs(av);
      return rc;
    }
    default:
      assert(0);
      return FALSE;
  }
}

static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
unify_match(term_t t, re_data *re, re_subject *subject,
	    const matchopts *opts, /* TODO: remove because unused? */
	    int ovsize, const int *ovector)
{ int i, rc;
  term_t av   = PL_new_term_refs(4);
  term_t capn = av+0;
  term_t caps = av+1;
  term_t pair = av+2;
  term_t list = av+3;
  (void)opts;  /* TODO: remove because unused? */

  PL_put_nil(list);
  for(i=ovsize-1; i>=0; i--)
  { int rc;

    PL_STRINGS_MARK();
    rc = (put_capname(capn, re, i) &&
	  put_capval(caps, re, subject, i, ovector) &&
	  PL_cons_functor(pair, FUNCTOR_pair2, capn, caps) &&
	  PL_cons_list(list, pair, list));
    PL_STRINGS_RELEASE();
    if ( !rc )
      return FALSE;
  }

  rc = PL_unify(t, list);
  PL_reset_term_refs(av);
  return rc;
}


static int /* bool (FALSE/TRUE), as returned by PL_..._error() */
re_error(int ec) /* ec is error code from pcre_exec */
{ switch(ec)
  { case 0:				/* Too short ovector */
      assert(0);
    case PCRE_ERROR_NOMATCH:
      return FALSE;
    case PCRE_ERROR_NULL:
    case PCRE_ERROR_BADOPTION:
    case PCRE_ERROR_BADMAGIC:
    case PCRE_ERROR_UNKNOWN_OPCODE:
      return PL_representation_error("regex");
    case PCRE_ERROR_NOMEMORY:
      return PL_resource_error("memory");
    case PCRE_ERROR_MATCHLIMIT:
      return PL_resource_error("match_limit");
    /* The following are all the other error codes that are
       documented for pcre_exec(). They have been listed, to
       aid in migration to PCRE2. */
    case PCRE_ERROR_NOSUBSTRING:
    case PCRE_ERROR_CALLOUT:
    case PCRE_ERROR_BADUTF8:
    case PCRE_ERROR_BADUTF8_OFFSET:
    case PCRE_ERROR_PARTIAL:
    case PCRE_ERROR_BADPARTIAL:
    case PCRE_ERROR_INTERNAL:
    case PCRE_ERROR_BADCOUNT:
    case PCRE_ERROR_RECURSIONLIMIT:
    case PCRE_ERROR_BADNEWLINE:
    case PCRE_ERROR_BADOFFSET:
    case PCRE_ERROR_SHORTUTF8:
    case PCRE_ERROR_RECURSELOOP:
    case PCRE_ERROR_JIT_STACKLIMIT:
    case PCRE_ERROR_BADMODE:
    case PCRE_ERROR_BADENDIANNESS:
    case PCRE_ERROR_JIT_BADOPTION:
    case PCRE_ERROR_BADLENGTH:
    case PCRE_UTF8_ERR1:
    case PCRE_UTF8_ERR2:
    case PCRE_UTF8_ERR3:
    case PCRE_UTF8_ERR4:
    case PCRE_UTF8_ERR5:
    case PCRE_UTF8_ERR6:
    case PCRE_UTF8_ERR7:
    case PCRE_UTF8_ERR8:
    case PCRE_UTF8_ERR9:
    case PCRE_UTF8_ERR10:
    case PCRE_UTF8_ERR11:
    case PCRE_UTF8_ERR12:
    case PCRE_UTF8_ERR13:
    case PCRE_UTF8_ERR14:
    case PCRE_UTF8_ERR15:
    case PCRE_UTF8_ERR16:
    case PCRE_UTF8_ERR17:
    case PCRE_UTF8_ERR18:
    case PCRE_UTF8_ERR19:
    case PCRE_UTF8_ERR20:
    case PCRE_UTF8_ERR21:
    case PCRE_UTF8_ERR22:
    default:
      assert(0);			/* TBD */
  }

  return FALSE;
}


static int /* bool (FALSE/TRUE), as returned by PL_..._error() */
alloc_ovector(re_data *re, int *ovecbuf, int *ovecsize, int **ovector)
{ int sz = (re->capture_size+1)*3;

  /* In the following, malloc() is correct, - don't use realloc().
     This is because the caller allocates ovecbuf on the stack; for
     cleanup, the caller checks whether *ovector is still on the stack
     or (if it wasn't big enough and was malloc-ed) it does a free(). */
  if ( sz > *ovecsize )
  { if ( !(*ovector = malloc(sz*sizeof (int))) )
    { return PL_resource_error("memory");
    }
    *ovecsize = sz;
  } else
  { *ovector = ovecbuf;
  }

  return TRUE;
}


/** re_matchsub_(+Regex, +String, -Sub:dict, +Options) is semidet.

    For documentation of this function, see pcre.pl
*/
static foreign_t
re_matchsub_(term_t regex, term_t on, term_t result, term_t options)
{ re_data *re;
  re_subject subject = { NULL, 0, 0, 0, 0 };
  matchopts opts = {0,0};
  re_options_flags options_flags;

  if ( !re_get_options(options, RE_EXEC, &options_flags, re_match_opt, &opts) )
    return FALSE;
  re_match_opt_postprocess(&options_flags);

  if ( get_re(regex, &re) &&
       re_get_subject(on, &subject, 0) )
  { int rc; /* Every path (to label out) must set rc */
    int ovecbuf[30];
    int ovecsize = 30;
    int *ovector;
    if ( !alloc_ovector(re, ovecbuf, &ovecsize, &ovector) )
    { rc = FALSE;
      goto out;
    }

    if ( opts.start )
    { if ( (opts.start=utf8_seek(subject.subject, subject.length,
				 opts.start)) == (size_t)-1 )
      { rc = out_of_range(opts.start);
	goto out;
      }
    }

    /* Note that we must *not* "or" the options from regex with the
       match options -- see the pcre_exec() documentation.
       "The only bits that may be set are
	   PCRE_ANCHORED,
	   PCRE_NEWLINE_xxx,
	   PCRE_NOTBOL,
	   PCRE_NOTEOL,
	   PCRE_NOTEMPTY,
	   PCRE_NOTEMPTY_ATSTART,
	   PCRE_NO_START_OPTIMIZE,
	   PCRE_NO_UTF8_CHECK,
	   PCRE_PARTIAL_HARD, and
	   PCRE_PARTIAL_SOFT."
    */
    options_flags.flags &= (PCRE_ANCHORED|PCRE_NEWLINE_CR|PCRE_NEWLINE_LF|
			    PCRE_NEWLINE_CRLF|PCRE_NEWLINE_ANY|PCRE_NEWLINE_ANYCRLF|
			    PCRE_NOTBOL|PCRE_NOTEOL|PCRE_NOTEMPTY| PCRE_NOTEMPTY_ATSTART|
			    PCRE_NO_START_OPTIMIZE|PCRE_NO_UTF8_CHECK|PCRE_PARTIAL_HARD|
			    PCRE_PARTIAL_SOFT);
    { int re_rc = pcre_exec(re->pcre, re->extra,
			    subject.subject, subject.length,
			    opts.start, options_flags.flags,
			    ovector, ovecsize);

      if ( re_rc > 0 )			/* match */
      { if ( result )
	  rc = unify_match(result, re, &subject, &opts, re_rc, ovector);
	else
	  rc = TRUE;
      } else
      { rc = re_error(re_rc);
      }
    }

  out:
    if ( ovector && ovector != ovecbuf )
      free(ovector);

    re_free_subject(&subject);

    return rc;
  }

  return FALSE;
}


/** re_match_(+Regex, +String) is semidet.
    re_match_(+Regex, +String, +Options) is semidet.

    For documentation of this function, see pcre.pl
*/
static foreign_t
re_match_(term_t regex, term_t on, term_t options)
{ return re_matchsub_(regex, on, 0, options);
}


/** re_foldl_(:Goal, +Regex, +String, ?V0, ?V, +Options) is semidet.

    For documentation of this function, see pcre.pl
*/
static foreign_t
re_foldl_(term_t regex, term_t on,
	  term_t closure, term_t v0, term_t v,
	  term_t options)
{ re_data *re;
  re_subject subject;
  re_options_flags options_flags;
  matchopts opts = {0};

  if ( !re_get_options(options, RE_EXEC, &options_flags, re_match_opt, &opts) )
    return FALSE;
  re_match_opt_postprocess(&options_flags);

  if ( get_re(regex, &re) &&
       re_get_subject(on, &subject, BUF_STACK) )
  { int rc; /* Every path (to label out) must set rc */
    int ovecbuf[30];
    int ovecsize = 30;
    int *ovector;
    int offset = 0;
    static predicate_t pred = 0;
    term_t av = PL_new_term_refs(4);

    if ( !alloc_ovector(re, ovecbuf, &ovecsize, &ovector) )
    { rc = FALSE;
      goto out;
    }

    if ( !pred )
      pred = PL_predicate("re_call_folder", 4, "pcre");

    if ( !PL_put_term(av+0, closure) )
    { rc = FALSE;
      goto out;
    }
	     /* av+1 is match */
    if ( !PL_put_term(av+2, v0) )
    { rc = FALSE;
      goto out;
    }
	     /* av+3 = v1 */

    for(;;)
    { int re_rc = pcre_exec(re->pcre, re->extra,
			    subject.subject, subject.length,
			    offset, options_flags.flags,
			    ovector, ovecsize);

      if ( re_rc > 0 )
      { PL_put_variable(av+1);
	if ( !unify_match(av+1, re, &subject, &opts, re_rc, ovector) ||
	     !PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av) ||
	     !PL_put_term(av+2, av+3) ||
	     !PL_put_variable(av+3) )
	{ rc = FALSE;
	  goto out;
	}
	if ( ovector[1] == offset )
	  offset++;
	else
	  offset = ovector[1];
      } else if ( re_rc == PCRE_ERROR_NOMATCH )
      { rc = PL_unify(av+2, v);
	break;
      } else
      { rc = re_error(re_rc);
	break;
      }
    }

  out:
    if ( ovector != ovecbuf )
      free(ovector);
    re_free_subject(&subject);

    return rc;
  }

  return FALSE;
}



#define MKFUNCTOR(n,a) \
	FUNCTOR_ ## n ## a = PL_new_functor(PL_new_atom(#n), a)
#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)


install_t
install_pcre4pl(void)
{ FUNCTOR_pair2 = PL_new_functor(PL_new_atom("-"), 2);

  MKATOM(optimise);
  MKATOM(optimize);
  MKATOM(bsr);
  MKATOM(compat);
  MKATOM(javascript);
  MKATOM(newline);
  MKATOM(start);
  MKATOM(capture_type);
  MKATOM(string);
  MKATOM(atom);
  MKATOM(term);
  MKATOM(range);
  MKATOM(version);

  PL_register_foreign("re_config",    1, re_config,    0);
  PL_register_foreign("re_compile",   3, re_compile,   0);
  PL_register_foreign("re_match_",    3, re_match_,    0);
  PL_register_foreign("re_matchsub_", 4, re_matchsub_, 0);
  PL_register_foreign("re_foldl_",    6, re_foldl_,    0);
  /* The following two are used by test_pcre.pl but are not exported
     from pcre.pl, so are used with the pcre: module prefix: */
  PL_register_foreign("re_portray",   2, re_portray,   0);
  PL_register_foreign("re_portray_match_options", 2, re_portray_match_options_, 0);
}

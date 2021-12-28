/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2020, VU University Amsterdam
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

#define O_DEBUG 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <pcre.h>
#include <stdio.h> /* for sprintf() */

		 /*******************************
		 *	      RE STRUCT		*
		 *******************************/

typedef enum cap_type
{ CAP_DEFAULT = 0,
  CAP_STRING,
  CAP_ATOM,
  CAP_INTEGER,
  CAP_FLOAT,
  CAP_NUMBER,
  CAP_TERM,
  CAP_RANGE
} cap_type;

/* For debugging */
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

typedef struct cap_how
{ atom_t	name;
  cap_type	type;
} cap_how;

typedef struct re_data
{ atom_t	symbol;			/* regex as blob */
  atom_t	pattern;		/* pattern (as atom) */
  int		re_options;		/* compilation options */
  int		capture_size;		/* # captured subpatterns */
  cap_how      *capture_names;		/* Names for captured data */
  cap_type	capture_type;		/* Default return for capture */
  pcre	       *pcre;			/* the compiled expression */
  pcre_extra   *extra;			/* study result */
} re_data;


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

static atom_t ATOM_optimise;		/* Optimise */
static atom_t ATOM_optimize;
static atom_t ATOM_bsr;
static atom_t ATOM_anycrlf;
static atom_t ATOM_unicode;
static atom_t ATOM_compat;
static atom_t ATOM_javascript;
static atom_t ATOM_newline;
static atom_t ATOM_any;
static atom_t ATOM_anycrlf;
static atom_t ATOM_cr;
static atom_t ATOM_lf;
static atom_t ATOM_crlf;
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

  return ( rea > reb ?	1 :
	   reb < rea ? -1 : 0
	 );
}


static int
write_pcre(IOSTREAM *s, atom_t symbol, int flags)
{ re_data *re = *(re_data**)PL_blob_data(symbol, NULL, NULL);

  Sfprintf(s, "<regex>(%p)", re);	/* TBD: Use pattern? */

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
  int	      flags;			/* Allocation flags */
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
re_get_subject(term_t t, re_subject *subj, int flags)
{ memset(subj, 0, sizeof *subj); /* { NULL, 0, 0, 0, 0 }; */

  subj->flags = flags;
  return PL_get_nchars(t, &subj->length, &subj->subject, flags|SUBJ_FLAGS);
}

static void
re_free_subject(re_subject *subj)
{
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


#define RE_STUDY	0x0001

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
set_flag(term_t arg, int *flags, int mask, int value, int invert, int *seen)
{ *flags &= mask;
  if ( *seen&value )
    return TRUE; /* repeated option - ignore */
  *seen |= value;
  switch( effective_bool(arg) )
  { case TRUE:
      if ( invert )
	*flags &= ~value;
      else
	*flags |= value;
      return TRUE;
    case FALSE:
      if ( invert )
	*flags |= value;
      else
	*flags &= ~value;
      return TRUE;
    default: /* -1 */
      return FALSE;
  }
}


typedef struct re_optdef
{ const char *name;
  int	      flag;
  int	      mode;
  atom_t      atom; /* Filled in as-needed by lookup_optdef() */
} re_optdef;

#define RE_COMP 0x001
#define RE_EXEC 0x002
#define RE_NEG	0x004


static const re_optdef*
lookup_optdef(re_optdef opt_defs[], atom_t name, int mode)
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
lookup_and_apply_optdef(re_optdef opt_defs[], atom_t name, int mode,
			term_t option_term, term_t arg, int mask,
			int *optp, int *seen)
{ const re_optdef *def = lookup_optdef(opt_defs, name, mode);
  if ( def )
    return set_flag(arg, optp, mask, def->flag, def->mode&RE_NEG, seen);
  return PL_type_error("option", option_term);
}


static re_optdef re_optdefs[] =
{ { "anchored",	       PCRE_ANCHORED,	      RE_COMP|RE_EXEC },
  { "auto_capture",    PCRE_NO_AUTO_CAPTURE,  RE_COMP|RE_NEG },
  { "caseless",	       PCRE_CASELESS,	      RE_COMP },
  { "dollar_endonly",  PCRE_DOLLAR_ENDONLY,   RE_COMP },
  { "dotall",	       PCRE_DOTALL,	      RE_COMP },
  { "dupnames",	       PCRE_DUPNAMES,	      RE_COMP },
  { "extended",	       PCRE_EXTENDED,	      RE_COMP },
  { "extra",	       PCRE_EXTRA,	      RE_COMP },
  { "firstline",       PCRE_FIRSTLINE,	      RE_COMP },
  { "greedy",	       PCRE_UNGREEDY,	      RE_COMP|RE_NEG },
  { "multiline",       PCRE_MULTILINE,	      RE_COMP },
  { "no_auto_capture", PCRE_NO_AUTO_CAPTURE,  RE_COMP }, /* backwards compatibility */
  { "ucp",	       PCRE_UCP,	      RE_COMP },
  { "ungreedy",	       PCRE_UNGREEDY,	      RE_COMP }, /* backwards compatibility */
  { "bol",	       PCRE_NOTBOL,	      RE_EXEC|RE_NEG },
  { "eol",	       PCRE_NOTEOL,	      RE_EXEC|RE_NEG },
  { "empty",	       PCRE_NOTEMPTY,	      RE_EXEC|RE_NEG },
  { "empty_atstart",   PCRE_NOTEMPTY_ATSTART, RE_EXEC|RE_NEG },
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
re_get_options(term_t options, int mode, int *optp,
	       int (*func)(atom_t o, term_t a, void *ctx),
	       void *ctx)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  int seen = 0;
  int seen_bsr = FALSE;
  int seen_newlines = FALSE;
  *optp = PCRE_NEWLINE_ANYCRLF|PCRE_NO_UTF8_CHECK;

  while(PL_get_list_ex(tail, head, tail))
  { atom_t name;
    size_t arity;
    term_t arg;
    if ( get_arg_1_if_any(head, &name, &arity, &arg) )
    { if ( name == ATOM_bsr && arg )
      { atom_t aval;
	if ( seen_bsr )
	  return TRUE;
	seen_bsr = TRUE;
	if ( !PL_get_atom_ex(arg, &aval) ||
	     !lookup_and_apply_optdef(re_optbsrs, aval, RE_COMP,
				      head, 0, ~OPTBSR_MASK, optp, &seen) )
	  return FALSE;
      } else if ( name == ATOM_newline && arg )
      { atom_t aval;
	if ( seen_newlines )
	  return TRUE;
	seen_newlines = TRUE;
	if (!PL_get_atom_ex(arg, &aval) ||
	    !lookup_and_apply_optdef(re_optnewlines, aval, RE_COMP,
				     head, 0, ~OPTNEWLINE_MASK, optp, &seen) )
	  return FALSE;
      } else if ( name == ATOM_compat && arg && mode&RE_COMP )
      { atom_t aval;
	if ( seen&PCRE_JAVASCRIPT_COMPAT )
	  continue;
	seen |= PCRE_JAVASCRIPT_COMPAT;
	/* No need for a mask - the only flag option is positive */
	if ( !PL_get_atom_ex(arg, &aval) )
	  return FALSE;
	if ( aval == ATOM_javascript )
	  *optp |= PCRE_JAVASCRIPT_COMPAT;
	else
	  return PL_domain_error("compat_option", arg);
      } else
      { const re_optdef *def = lookup_optdef(re_optdefs, name, mode);
	if ( def )
	{ if ( !set_flag(arg, optp, ~def->flag, def->flag, def->mode&RE_NEG, &seen) )
	    return FALSE;
	} else if ( func )
	{  if ( !(*func)(name, arg, ctx) )
	    return FALSE;
	} else
	{  return PL_type_error("option", head);
	}
      }
    } else /* !get_arg_1_if_any() */
    { return FALSE;
    }
  }

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
  atom_t	  atom; /* Filled in as-needed by re_config() */
} re_config_opt;

static re_config_opt cfg_opts[] =
{ { "utf8",		      PCRE_CONFIG_UTF8,			  CFG_BOOL },
  { "unicode_properties",     PCRE_CONFIG_UNICODE_PROPERTIES,	  CFG_BOOL },
  { "jit",		      PCRE_CONFIG_JIT,			  CFG_BOOL },
  { "jittarget",	      PCRE_CONFIG_JITTARGET,		  CFG_STRING },
  { "newline",		      PCRE_CONFIG_NEWLINE,		  CFG_INTEGER },
  { "bsr",		      PCRE_CONFIG_BSR,			  CFG_INTEGER },
  { "link_size",	      PCRE_CONFIG_LINK_SIZE,		  CFG_INTEGER },
  { "posix_malloc_threshold", PCRE_CONFIG_POSIX_MALLOC_THRESHOLD, CFG_INTEGER },
#ifdef PCRE_CONFIG_PARENS_LIMIT
  { "parens_limit",	      PCRE_CONFIG_PARENS_LIMIT,		  CFG_INTEGER },
#endif
  { "match_limit",	      PCRE_CONFIG_MATCH_LIMIT,		  CFG_INTEGER },
  { "match_limit_recursion",  PCRE_CONFIG_MATCH_LIMIT_RECURSION,  CFG_INTEGER },
  { "stackrecurse",	      PCRE_CONFIG_STACKRECURSE,		  CFG_BOOL },
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
		return PL_unify_atom_chars(arg, val.s);
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


static int /* bool (FALSE/TRUE), as returned by PL_..._error() */
init_capture_map(re_data *re)
{ int count;

  if ( pcre_fullinfo(re->pcre, re->extra, PCRE_INFO_CAPTURECOUNT, &count)==0 )
  { re->capture_size = count;

    if ( pcre_fullinfo(re->pcre, re->extra, PCRE_INFO_NAMECOUNT, &count)==0 &&
	 count > 0 )
    { int es;
      const char *table;

      if ( pcre_fullinfo(re->pcre, re->extra, PCRE_INFO_NAMEENTRYSIZE, &es)==0 &&
	   pcre_fullinfo(re->pcre, re->extra, PCRE_INFO_NAMETABLE, &table)==0 )
      { int i;
	size_t cmsize = (re->capture_size+1)*sizeof(cap_how);

	if ( (re->capture_names = malloc(cmsize)) )
	{ memset(re->capture_names, 0, cmsize);
	} else
	  { return PL_resource_error("memory");
	}

	for(i=0; i<count; i++, table += es)
	{ int ci = ((table[0]&0xff)<<8) + (table[1]&0xff);
	  atom_t name;
	  const char *s = &table[2];
	  const char *fs;
	  size_t len = (size_t)-1;

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
	  { re->capture_names[ci].type = re->capture_type;
	  }

	  if ( !(name = PL_new_atom_mbchars(REP_UTF8, len, s)) )
	    return FALSE;

	  re->capture_names[ci].name = name;
	}
      }
    }
  }

  return TRUE;
}


typedef struct re_compile_options
{ int		flags; /* RE_STUDY */
  cap_type	capture_type;
  int		seen_flags;
  int		seen_cap;
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
      { copts->flags |= RE_STUDY;
	return TRUE;
      }
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
  }

  return TRUE;
}


/* For debugging (used by re_compile_options_(), re_exec_options_()) */
static void
re_options_str(int re_options, char *o_str)
{
  /* The following were extracted from pcre.h and sorted, with bsr and newline at
     the end because they're multi-valued: */

  if ( re_options & PCRE_ANCHORED )	     strcat(o_str, " ANCHORED");
  if ( re_options & PCRE_AUTO_CALLOUT )	     strcat(o_str, " AUTO_CALLOUT");
  if ( re_options & PCRE_CASELESS )	     strcat(o_str, " CASELESS");
  if ( re_options & PCRE_DOLLAR_ENDONLY )    strcat(o_str, " DOLLAR_ENDONLY");
  if ( re_options & PCRE_DOTALL )	     strcat(o_str, " DOTALL");
  if ( re_options & PCRE_DUPNAMES )	     strcat(o_str, " DUPNAMES");
  if ( re_options & PCRE_EXTENDED )	     strcat(o_str, " EXTENDED");
  if ( re_options & PCRE_EXTRA )	     strcat(o_str, " EXTRA");
  if ( re_options & PCRE_FIRSTLINE )	     strcat(o_str, " FIRSTLINE");
  if ( re_options & PCRE_JAVASCRIPT_COMPAT ) strcat(o_str, " JAVASCRIPT_COMPAT");
  if ( re_options & PCRE_MULTILINE )	     strcat(o_str, " MULTILINE");
  if ( re_options & PCRE_NEVER_UTF )	     strcat(o_str, " NEVER_UTF"); /* PCRE_DFA_SHORTEST (overlay ) */

  if ( re_options & PCRE_NOTBOL )	     strcat(o_str, " NOTBOL");
  if ( re_options & PCRE_NOTEMPTY )	     strcat(o_str, " NOTEMPTY");
  if ( re_options & PCRE_NOTEMPTY_ATSTART )  strcat(o_str, " NOTEMPTY_ATSTART");
  if ( re_options & PCRE_NOTEOL )	     strcat(o_str, " NOTEOL");
  if ( re_options & PCRE_NO_AUTO_CAPTURE )   strcat(o_str, " NO_AUTO_CAPTURE");
  if ( re_options & PCRE_NO_AUTO_POSSESS )   strcat(o_str, " NO_AUTO_POSSESS"); /* PCRE_DFA_RESTART (overlay ) */
  if ( re_options & PCRE_NO_START_OPTIMIZE ) strcat(o_str, " NO_START_OPTIMIZE"); /*  PCRE_NO_START_OPTIMISE (synonym ) */
  if ( re_options & PCRE_NO_UTF8_CHECK )     strcat(o_str, " NO_UTF8_CHECK"); /* PCRE_NO_UTF16_CHECK, PCRE_NO_UTF32_CHECK (synonym ) */
  if ( re_options & PCRE_PARTIAL_HARD )	     strcat(o_str, " PARTIAL_HARD");
  if ( re_options & PCRE_PARTIAL_SOFT )	     strcat(o_str, " PARTIAL_SOFT"); /* PCRE_PARTIAL (synonym ) */
  if ( re_options & PCRE_UCP )		     strcat(o_str, " UCP");
  if ( re_options & PCRE_UNGREEDY )	     strcat(o_str, " UNGREEDY");
  if ( re_options & PCRE_UTF8 )		     strcat(o_str, " UTF8"); /* PCRE_UTF16, PCRE_UTF32 (synonym ) */

  if ( (re_options & OPTBSR_MASK)     == PCRE_BSR_ANYCRLF )	  strcat(o_str, " BSR_ANYCRLF");
  if ( (re_options & OPTBSR_MASK)     == PCRE_BSR_UNICODE )	  strcat(o_str, " BSR_UNICODE");

  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_CR )	  strcat(o_str, " NEWLINE_CR");
  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_LF )	  strcat(o_str, " NEWLINE_LF");
  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_CRLF )	  strcat(o_str, " NEWLINE_CRLF");
  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_ANY )	  strcat(o_str, " NEWLINE_ANY");
  if ( (re_options & OPTNEWLINE_MASK) == PCRE_NEWLINE_ANYCRLF )	  strcat(o_str, " NEWLINE_ANYCRLF");
}


/** '$re_compile_options'(+Options, -OptsStr) is det.

    For debugging - process the Options into a string
*/
static foreign_t
re_compile_options_(term_t options, term_t opts_str)
{ int re_options = 0;
  char o_str[1000] = { '\0', '\0' };
  re_compile_options copts = {0, CAP_STRING, FALSE, FALSE};

  if ( !re_get_options(options, RE_COMP, &re_options,
		       re_compile_opt, &copts) )
    return FALSE;

  re_options_str(re_options, o_str);

  /* And now the stuff that's in copts: */
  if ( copts.flags & RE_STUDY ) strcat(o_str, " $STUDY");
  else				strcat(o_str, " $no-study");
  strcat(o_str, " $");
  strcat(o_str, cap_type_str(copts.capture_type));

  return PL_unify_string_chars(opts_str, &o_str[1]);
}


/** re_compile(+Pattern, -Regex, +Options) is det.

    For documentation of this function, see pcre.pl
*/
static foreign_t
re_compile(term_t pat, term_t reb, term_t options)
{ int flags = CVT_ATOM|CVT_STRING|CVT_LIST;
  size_t len;
  char *pats;
  int re_options;
  re_compile_options copts = {0, CAP_STRING};
  pcre *pcre;
  int re_error_code;
  const char *re_error_msg;
  int re_error_offset;
  const unsigned char *tableptr = NULL;

  if ( !re_get_options(options, RE_COMP, &re_options,
		       re_compile_opt, &copts) )
    return FALSE;

  if ( PL_get_nchars(pat, &len, &pats, flags|REP_UTF8|CVT_EXCEPTION) )
  { re_options |= (PCRE_UTF8|PCRE_NO_UTF8_CHECK);
  } else
    return FALSE;

  if ( strlen(pats) != len )		/* TBD: escape as \0xx */
    return PL_representation_error("nul_byte");

  if ( (pcre = pcre_compile2(pats, re_options,
			     &re_error_code, &re_error_msg, &re_error_offset,
			     tableptr) ) )
  { re_data *re;

    if ( !(re = PL_malloc(sizeof(*re))) )
      return FALSE;

    memset(re, 0, sizeof *re);

    re->pcre = pcre;
    re->re_options = re_options;
    re->capture_type = copts.capture_type;
    if ( copts.flags & RE_STUDY )
    { re->extra = pcre_study(pcre, 0, &re_error_msg);
					/* TBD: handle error */
    }
    if ( !init_capture_map(re) )
    { re_free(re);
      /* init_capture_map() has called an appropriate PL_..._error()
	 to indicate the cause; PL_..._error() returns FALSE, so
	 return that value. */
      return FALSE;
    }

    if ( (PL_get_atom(pat, &re->pattern)) )
    { PL_register_atom(re->pattern);
    } else
    { re->pattern = PL_new_atom_mbchars(REP_UTF8, len, pats);
    }

    return PL_unify_blob(reb, &re, sizeof(re), &pcre_blob);
  } else
  { return PL_syntax_error(re_error_msg, NULL); /* TBD: location, code */
  }
}



typedef struct matchopts
{ size_t start;
  int    seen_start;
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


/** '$re_match_options'(+Options, -OptsStr) is det.

    For debugging - process the Options into a string
*/
static foreign_t
re_match_options_(term_t options, term_t opts_str)
{ int re_options = 0;
  char o_str[1000] = { '\0', '\0' };
  matchopts mopts = {0, FALSE};

  if ( !re_get_options(options, RE_EXEC, &re_options,
		       re_match_opt, &mopts) )
    return FALSE;

  re_options_str(re_options, o_str);
  /* And now the stuff that's in mopts: */
  { char start_str[100];
    sprintf(start_str, " $start=%lu", mopts.start);
    strcat(o_str, start_str);
  }

  return PL_unify_string_chars(opts_str, &o_str[1]);
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
      int rc;
      size_t start = bytep_to_charp(subject, ovector[i*2]);
      size_t end   = bytep_to_charp(subject, ovector[i*2+1]);

      rc = ( (av=PL_new_term_refs(2)) &&
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
  (void) opts;  /* TODO: remove because unused? */

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
     and if not, it does a free(). */
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
  matchopts opts = {0};
  int re_options;
  int flags = 0;

  if ( !re_get_options(options, RE_EXEC, &re_options, re_match_opt, &opts) )
    return FALSE;

  if ( get_re(regex, &re) &&
       re_get_subject(on, &subject, flags) )
  { int rc;
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

    { int re_rc = pcre_exec(re->pcre, re->extra,
			    subject.subject, subject.length,
			    opts.start, re_options,
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
  int re_options;
  matchopts opts = {0};

  if ( !re_get_options(options, RE_EXEC, &re_options, re_match_opt, &opts) )
    return FALSE;

  if ( get_re(regex, &re) &&
       re_get_subject(on, &subject, BUF_STACK) )
  { int rc;
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
			    offset, re_options,
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
  MKATOM(anycrlf);
  MKATOM(unicode);
  MKATOM(newline);
  MKATOM(any);
  MKATOM(anycrlf);
  MKATOM(cr);
  MKATOM(lf);
  MKATOM(crlf);
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
  PL_register_foreign("$re_compile_options", 2, re_compile_options_, 0);
  PL_register_foreign("$re_match_options", 2, re_match_options_, 0);
}

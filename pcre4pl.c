/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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
#define PL_ARITY_AS_SIZE 1
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <pcre.h>

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

typedef struct cap_how
{ atom_t	name;
  cap_type	type;
} cap_how;

typedef struct re_data
{ atom_t	symbol;			/* regex as blob */
  atom_t        pattern;		/* pattern (as atom) */
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

  return ( rea > reb ?  1 :
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

static int
re_get_subject(term_t t, re_subject *subj, int flags)
{ memset(subj, 0, sizeof(*subj));

  return PL_get_nchars(t, &subj->length, &subj->subject, flags|SUBJ_FLAGS);
}


		 /*******************************
		 *	   PROLOG EXCHANGE	*
		 *******************************/

static int
get_re(term_t t, re_data **re)
{ void *p;
  size_t len;
  PL_blob_t *type;

  if ( PL_get_blob(t, &p, &len, &type) && type == &pcre_blob )
  { re_data **rep = p;
    *re = *rep;
    return TRUE;
  }

  PL_type_error("regex", t);
  return FALSE;
}


#define RE_STUDY	0x0001

static int
effective_bool(term_t arg)
{ if ( arg )
  { int v;

    if ( PL_get_bool_ex(arg, &v) )
      return v;
    return -1;
  }

  return TRUE;
}


static int
set_flag(term_t arg, int *flags, int value)
{ switch( effective_bool(arg) )
  { case TRUE:
      *flags |= value;
    case FALSE:
      return TRUE;
    default:
      return FALSE;
  }
}


static int
not_flag(term_t head, term_t arg, int *flags, int value)
{ int v;

  if ( !arg )
  { PL_type_error("option", head);
    return FALSE;
  } else if ( PL_get_bool_ex(arg, &v) )
  { if ( !v )
      *flags |= value;
  } else
    return FALSE;

  return TRUE;
}


typedef struct re_optdef
{ const char *name;
  int	      flag;
  int	      mode;
  atom_t      atom;
} re_optdef;

#define RE_COMP 0x001
#define RE_EXEC 0x002
#define RE_NEG  0x004

static re_optdef re_optdefs[] =
{ { "anchored",	       PCRE_ANCHORED,	      RE_COMP },
  { "caseless",	       PCRE_CASELESS,	      RE_COMP },
  { "dollar_endonly",  PCRE_DOLLAR_ENDONLY,   RE_COMP },
  { "dotall",	       PCRE_DOTALL,	      RE_COMP },
  { "dupnames",	       PCRE_DUPNAMES,	      RE_COMP },
  { "extended",	       PCRE_EXTENDED,	      RE_COMP },
  { "extra",	       PCRE_EXTRA,	      RE_COMP },
  { "firstline",       PCRE_FIRSTLINE,	      RE_COMP },
  { "multiline",       PCRE_MULTILINE,	      RE_COMP },
  { "no_auto_capture", PCRE_NO_AUTO_CAPTURE,  RE_COMP },
  { "ucp",	       PCRE_UCP,	      RE_COMP },
  { "ungreedy",	       PCRE_UNGREEDY,	      RE_COMP },
  { "bol",	       PCRE_NOTBOL,	      RE_EXEC|RE_NEG },
  { "eol",	       PCRE_NOTEOL,	      RE_EXEC|RE_NEG },
  { "empty",	       PCRE_NOTEMPTY,	      RE_EXEC|RE_NEG },
  { "empty_atstart",   PCRE_NOTEMPTY_ATSTART, RE_EXEC|RE_NEG },
  { NULL,	       0}
};


static int
re_get_options(term_t options, int mode, int *optp,
	       int (*func)(atom_t o, term_t a, void *ctx),
	       void *ctx)
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();
  term_t argt = PL_new_term_ref();
  int opt = PCRE_NEWLINE_ANYCRLF|PCRE_NO_UTF8_CHECK;

  while(PL_get_list_ex(tail, head, tail))
  { atom_t name;
    size_t arity;

    if ( PL_get_name_arity(head, &name, &arity) && arity <= 1 )
    { term_t arg;

      if ( arity == 1 )
      { _PL_get_arg(1, head, argt);
	arg = argt;
      } else
      { arg = 0;
      }

      if ( name == ATOM_bsr && arg )
      { atom_t aval;

	if ( !PL_get_atom_ex(arg, &aval) )
	  return FALSE;

	if ( aval == ATOM_anycrlf )
	  opt |= PCRE_BSR_ANYCRLF;
	else if ( aval == ATOM_unicode )
	  opt |= PCRE_BSR_UNICODE;
	else
	{ PL_domain_error("bsr_option", arg);
	  return FALSE;
	}
      } else if ( name == ATOM_newline && arg )
      { atom_t aval;

	if ( !PL_get_atom_ex(arg, &aval) )
	  return FALSE;

	opt &= ~PCRE_NEWLINE_ANYCRLF;

	if ( aval == ATOM_any )
	  opt |= PCRE_NEWLINE_ANY;
	else if ( aval == ATOM_anycrlf )
	  opt |= PCRE_NEWLINE_ANYCRLF;
	else if ( aval == ATOM_crlf )
	  opt |= PCRE_NEWLINE_CRLF;
	else if ( aval == ATOM_lf )
	  opt |= PCRE_NEWLINE_LF;
	else if ( aval == ATOM_cr )
	  opt |= PCRE_NEWLINE_CR;
	else
	{ PL_domain_error("newline_option", arg);
	  return FALSE;
	}
      } else if ( name == ATOM_compat && arg && mode == RE_COMP )
      { atom_t aval;

	if ( !PL_get_atom_ex(arg, &aval) )
	  return FALSE;

	if ( aval == ATOM_javascript )
	  opt |= PCRE_JAVASCRIPT_COMPAT;
	else
	{ PL_domain_error("compat_option", arg);
	  return FALSE;
	}
      } else
      { re_optdef *def;

	for(def=re_optdefs; def->name; def++)
	{ if ( !def->atom )
	    def->atom = PL_new_atom(def->name);
	  if ( def->atom == name && (def->mode&mode) )
	  { if ( (def->mode&RE_NEG) )
	    { if ( !not_flag(head, arg, &opt, def->flag) )
		return FALSE;
	    } else
	    { if ( !set_flag(arg, &opt, def->flag) )
		return FALSE;
	    }
	    break;
	  }
	}
	if ( def->name )
	  continue;
	if ( func )
	{ if ( !(*func)(name, arity == 0 ? 0 : arg, ctx) )
	    return FALSE;
	}
      }
    } else
    { PL_type_error("option", head);
      return FALSE;
    }
  }

  if ( PL_get_nil_ex(tail) )
  { *optp  = opt;
    return TRUE;
  }

  return FALSE;
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
  atom_t	  atom;
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

  { NULL }
};


static foreign_t
re_config(term_t opt)
{ atom_t name;
  size_t arity;

  if ( PL_get_name_arity(opt, &name, &arity) && arity == 1 )
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

  return TRUE;
}


static int
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
	{ PL_resource_error("memory");
	  return FALSE;
	}

	for(i=0; i<count; i++, table += es)
	{ int ci = ((table[0]&0xff)<<8) + (table[1]&0xff);
	  atom_t name;
	  const char *s = &table[2];
	  const char *fs;
	  size_t len = (size_t)-1;

	  assert(ci < re->capture_size+1);

	  if ( (fs=strchr(s, '_')) )
	  { if ( fs[1] && !fs[2] )
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
	    }
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
{ int		flags;
  cap_type	capture_type;
} re_compile_options;


static int
re_compile_opt(atom_t opt, term_t arg, void *ctx)
{ re_compile_options *copts = ctx;

  if ( opt == ATOM_optimise )
  { switch(effective_bool(arg))
    { case TRUE:
      { copts->flags |= RE_STUDY;
	return TRUE;
      }
      case FALSE:
	return TRUE;
      default:
	return FALSE;
    }
  } else if ( opt == ATOM_capture_type && arg )
  { atom_t aval;

    if ( !PL_get_atom_ex(arg, &aval) )
      return FALSE;

    if ( aval == ATOM_range )
      copts->capture_type = CAP_RANGE;
    else if ( aval == ATOM_atom )
      copts->capture_type = CAP_ATOM;
    else if ( aval == ATOM_string )
      copts->capture_type = CAP_STRING;
    else if ( aval == ATOM_term )
      copts->capture_type = CAP_TERM;
    else
    { PL_domain_error("capture_type", arg);
    }
  }

  return TRUE;
}



/** re_compile(+Pattern, -Regex, +Options)
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

    if ( !(re=PL_malloc(sizeof(*re))) )
      return FALSE;

    memset(re, 0, sizeof(*re));

    re->pcre = pcre;
    re->re_options = re_options;
    re->capture_type = copts.capture_type;
    if ( copts.flags & RE_STUDY )
    { re->extra = pcre_study(pcre, 0, &re_error_msg);
					/* TBD: handle error */
    }
    if ( !init_capture_map(re) )
    { re_free(re);
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


/**
 * re_matchsub(+Regex, +String, -Result, +Options)
 */

typedef struct matchopts
{ size_t start;
} matchopts;


static int
re_match_opt(atom_t opt, term_t arg, void *ctx)
{ matchopts *opts = ctx;

  if ( arg )
  { if ( opt == ATOM_start )
    { if ( !PL_get_size_ex(arg, &opts->start) )
	return FALSE;
    }
  }

  return TRUE;
}


static int
out_of_range(size_t index)
{ term_t ex;

  return ( (ex=PL_new_term_ref()) &&
	   PL_put_int64(ex, index) &&
	   PL_domain_error("offset", ex) );
}


static int
put_capname(term_t t, re_data *re, int i)
{ atom_t name;

  if ( re->capture_names && (name=re->capture_names[i].name) )
    return PL_put_atom(t, name);
  else
    return PL_put_integer(t, i);
}


static int
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

static int
unify_match(term_t t, re_data *re, re_subject *subject,
	    matchopts *opts, int ovsize, const int *ovector)
{ int i, rc;
  term_t av   = PL_new_term_refs(4);
  term_t capn = av+0;
  term_t caps = av+1;
  term_t pair = av+2;
  term_t list = av+3;

  PL_put_nil(list);
  for(i=ovsize-1; i>=0; i--)
  { if ( !(put_capname(capn, re, i) &&
	   put_capval(caps, re, subject, i, ovector) &&
	   PL_cons_functor(pair, FUNCTOR_pair2, capn, caps) &&
	   PL_cons_list(list, pair, list)) )
    { return FALSE;
    }
  }

  rc = PL_unify(t, list);
  PL_reset_term_refs(av);
  return rc;
}


static int
re_error(int ec)
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
    default:
      assert(0);			/* TBD */
  }

  return FALSE;
}


static int *
alloc_ovector(re_data *re, int *ovector, int *ovecsize)
{ if ( re->capture_size >= (*ovecsize)*3 )
  { int sz = (re->capture_size+1)*3;

    if ( !(ovector = malloc(sz*sizeof(int))) )
    { PL_resource_error("memory");
      ovector = NULL;
    }
    *ovecsize = sz;
  }

  return ovector;
}



static foreign_t
re_matchsub(term_t regex, term_t on, term_t result, term_t options)
{ re_data *re;
  re_subject subject;
  matchopts opts = {0};
  int re_options;
  int flags = 0;

  if ( result )
    flags |= BUF_RING;			/* Results may shift stack */

  if ( !re_get_options(options, RE_EXEC, &re_options, re_match_opt, &opts) )
    return FALSE;

  if ( get_re(regex, &re) &&
       re_get_subject(on, &subject, flags) )
  { int rc;
    int ovecbuf[30];
    int ovecsize = 30;
    int *ovector = alloc_ovector(re, ovecbuf, &ovecsize);

    if ( ovector == NULL )
      return FALSE;

    if ( opts.start )
    { if ( (opts.start=utf8_seek(subject.subject, subject.length,
				 opts.start)) == (size_t)-1 )
	return out_of_range(opts.start);
    }

    rc = pcre_exec(re->pcre, re->extra,
		   subject.subject, subject.length,
		   opts.start, re_options,
		   ovector, ovecsize);

    if ( rc > 0 )			/* match */
    { if ( result )
	rc = unify_match(result, re, &subject, &opts, rc, ovector);
      else
	rc = TRUE;
    } else
    { rc = re_error(rc);
    }

    if ( ovector != ovecbuf )
      free(ovector);

    return rc;
  }

  return FALSE;
}


static foreign_t
re_match(term_t regex, term_t on, term_t options)
{ return re_matchsub(regex, on, 0, options);
}


static foreign_t
re_foldl(term_t regex, term_t on,
	 term_t closure, term_t v0, term_t v,
	 term_t options)
{ re_data *re;
  re_subject subject;
  int re_options;
  matchopts opts = {0};

  if ( !re_get_options(options, RE_EXEC, &re_options, re_match_opt, &opts) )
    return FALSE;

  if ( get_re(regex, &re) &&
       re_get_subject(on, &subject, BUF_RING) )
  { int rc;
    int ovecbuf[30];
    int ovecsize = 30;
    int *ovector = alloc_ovector(re, ovecbuf, &ovecsize);
    int offset = 0;
    static predicate_t pred = 0;
    term_t av = PL_new_term_refs(4);

    if ( ovector == NULL )
      return FALSE;

    if ( !pred )
      pred = PL_predicate("re_call_folder", 4, "pcre");

    PL_put_term(av+0, closure);
	     /* av+1 is match */
    PL_put_term(av+2, v0);
	     /* av+3 = v1 */

    for(;;)
    { rc = pcre_exec(re->pcre, re->extra,
		     subject.subject, subject.length,
		     offset, re_options,
		     ovector, ovecsize);

      if ( rc > 0 )
      { PL_put_variable(av+1);
	if ( !unify_match(av+1, re, &subject, &opts, rc, ovector) ||
	     !PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av) )
	{ rc = FALSE;
	  goto out;
	}
	PL_put_term(av+2, av+3);
	PL_put_variable(av+3);
	if ( ovector[1] == offset )
	  offset++;
	else
	  offset = ovector[1];
      } else
      { rc = PL_unify(av+2, v);
	break;
      }
    }

  out:
    if ( ovector != ovecbuf )
      free(ovector);

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

  PL_register_foreign("re_config",    1, re_config,   0);
  PL_register_foreign("re_compile",   3, re_compile,  0);
  PL_register_foreign("re_match_",    3, re_match,    0);
  PL_register_foreign("re_matchsub_", 4, re_matchsub, 0);
  PL_register_foreign("re_foldl_",    6, re_foldl,    0);
}

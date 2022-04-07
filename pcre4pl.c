/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2022, VU University Amsterdam
			      SWI-Prolog Solution b.v.
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

/* See also: https://www.regular-expressions.info/pcre2.html */
/* See also: https://github.com/PhilipHazel/pcre2/issues/51 */
/* See also: https://www.regular-expressions.info/pcre2.html */
/* See also: https://github.com/i3/i3/issues/4682#issuecomment-973076704 */
/* See also: https://wiki.php.net/rfc/pcre2-migration */
#define PCRE2_CODE_UNIT_WIDTH 8
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <pcre2.h>

/* For testing systems that don't have JIT on systems with a JIT,
   uncomment the following: */
/* #define FORCE_NO_JIT 1 */


		 /*******************************
		 *	      RE STRUCT		*
		 *******************************/

typedef enum cap_type /* capture type */
{ CAP_DEFAULT = 0, /* Use the "global" value for the regex */
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
cap_type_str(uint32_t i) /* cap_type i */
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
   This struct should be initialized to {0,0}.
*/

typedef struct re_options_flags
{ uint32_t seen;
  uint32_t flags;
} re_options_flags;

/* The data in a "regex" blob. This is created by re_compile_() and a
   local copy is used by the matching functions (see get_re_copy()).

   The flags don't need to be kept, but they're convenient for
   processing.  When get_re_copy() makes a local copy, it clears the
   flags that are specific to pcre2_match() - match_options_flags,
   start_flags.
*/

typedef struct re_data
{ atom_t            pattern;		/* pattern (as atom) */
  re_options_flags  compile_options_flags;
  re_options_flags  capture_type;	/* Default capture_type: cap_type */
  re_options_flags  optimise_flags;     /* Turns on JIT */
  re_options_flags  jit_options_flags;
  re_options_flags  compile_ctx_flags;
  re_options_flags  compile_bsr_flags;
  re_options_flags  compile_newline_flags;
  re_options_flags  match_options_flags;
  re_options_flags  start_flags;        /* The start position (int) - a "match" flag */
  uint32_t          capture_size;	/* # captured subpatterns */
  cap_how          *capture_names;	/* Names for captured data */
  pcre2_code_8     *pcre2_code;		/* the compiled expression */
} re_data;

/* The start position is PCRE2_SIZE in pcre2.h: */
/* TODO: Our flag (which contains the size) is only 32 bits but PCRE2 allows 64 bits */
#define OPTSTART_MASK ((uint32_t)PCRE2_SIZE_MAX)

static void
init_re_data(re_data *re)
{ memset(re, 0, sizeof *re);
  /* See also get_re_copy() */
  /* PCRE2_NO_UTF_CHECK means that the pattern string (for compile)
     and subject string (for match) isn't checked for validity; this
     gives a small performance improvement - we simply trust
     SWI-Prolog to do the right thing with UTF8 strings. (The PCRE2
     documentation warns of undefined behavior, including crashes or
     loops when given invalid UTF). If desired, the test can
     explicitly be turned on by utf_check(true). */
  re->compile_options_flags.flags = PCRE2_UTF|PCRE2_NO_UTF_CHECK;
  re->match_options_flags.flags = PCRE2_NO_UTF_CHECK;
  re->capture_type.flags = CAP_STRING;
}

static void
write_re_options(IOSTREAM *s, const char **sep, const re_data *re);

static int
free_pcre(re_data *re)
{ /* TODO: clearing the freed items (by assigning 0 or NULL)
	   isn't necessary. */
  if ( re->pattern )
  { PL_unregister_atom(re->pattern);
    re->pattern = 0;
  }
  pcre2_code_free(re->pcre2_code);
  re->pcre2_code = NULL;
  if ( re->capture_names )
  { uint32_t i;

    for(i=0; i<re->capture_size+1; i++)
    { if ( re->capture_names[i].name )
      {	PL_unregister_atom(re->capture_names[i].name);
	re->capture_names[i].name = 0;
      }
    }

    free(re->capture_names);
    re->capture_names = NULL;
  }
  return TRUE;
}

static int
release_pcre(atom_t symbol)
{ re_data *re = PL_blob_data(symbol, NULL, NULL);
  return free_pcre(re);
}

static functor_t FUNCTOR_pair2;		/* -/2 */


		 /*******************************
		 *	  SYMBOL WRAPPER	*
		 *******************************/


static int
compare_pcres(atom_t a, atom_t b)
{ const re_data *rea = PL_blob_data(a, NULL, NULL);
  const re_data *reb = PL_blob_data(b, NULL, NULL);
  int comparison;

  if ( rea->pattern == reb->pattern )
  { comparison = 0;
  } else
  { const pl_wchar_t *sa, *sb;

    PL_STRINGS_MARK();
    sa = PL_atom_wchars(rea->pattern, NULL);
    sb = PL_atom_wchars(reb->pattern, NULL);
    comparison = wcscmp(sa, sb);
    PL_STRINGS_RELEASE();
  }

  if ( comparison )
    return comparison;

  /* Same pattern, so use address (which is stable) to break tie: */
  return ( (rea > reb) ?  1 :
	   (rea < reb) ? -1 : 0
	 );
}


static int
write_pcre(IOSTREAM *s, atom_t symbol, int flags)
{ (void)flags; /* unused arg */
  const re_data *re = PL_blob_data(symbol, NULL, NULL);
  /* For blob details: re_portray_() - re_portray/2 */
  PL_STRINGS_MARK();
  Sfprintf(s, "<regex>(%p, /%Ws/)", re, PL_atom_wchars(re->pattern, NULL));
  PL_STRINGS_RELEASE();
  return TRUE;
}


static PL_blob_t pcre2_blob =
{ PL_BLOB_MAGIC,
  0,
  "regex",
  release_pcre,
  compare_pcres,
  write_pcre,
  NULL, /* acquire */
  NULL, /* TODO: save */
  NULL  /* TODO: load */
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

static void
init_subject(re_subject *subject)
{ memset(subject, 0, sizeof *subject); /* { NULL, 0, 0, 0 }; */
}

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

#define GET_NCHARS_FLAGS (CVT_ATOM|CVT_STRING|CVT_LIST|REP_UTF8|CVT_EXCEPTION)

static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
re_get_subject(term_t t, re_subject *subj, unsigned int alloc_flags)
{ init_subject(subj);

  return PL_get_nchars(t, &subj->length, &subj->subject, alloc_flags|GET_NCHARS_FLAGS);
}



		 /*******************************
		 *	   PROLOG EXCHANGE	*
		 *******************************/

static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
get_re(term_t t, re_data **re)
{ size_t len;
  PL_blob_t *type;

  if ( PL_get_blob(t, (void **)re, &len, &type) && type == &pcre2_blob )
  { /* assert(len == sizeof **re); */
    return TRUE;
  }

  *re = NULL;
  return PL_type_error("regex", t);
}

/* Make a copy of the data in a regex blob, clearing the flags that
   are specific to pcre2_match(). */
static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
get_re_copy(term_t t, re_data *re)
{ re_data *re_ptr;
  if ( !get_re(t, &re_ptr) )
    return FALSE;
  *re = *re_ptr;
  /* Initialize the "match" flags - see also init_re_data() */
  /* TODO: Combine with init_re_data(), for defaults */
  re->match_options_flags.seen = 0;
  re->match_options_flags.flags = PCRE2_NO_UTF_CHECK;
  re->start_flags.seen = 0;
  re->start_flags.flags = 0;
  return TRUE;
}


#define RE_OPTIMISE 0x0001

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

/* Set a bit flag from a term (e.g. `anchored(true)`). The `mask` has
   1 for where the value can be applied; `value` is the flag value
   (e.g., from re_optdefs[i].flag) and is never zero. `arg` is used to
   determine whether the flag is set to `value` or its inverse, and
   `invert` uses the inverse of `value`. For single bit flags, `mask`
   and `value` are the same; for multi-bit flags, `mask` is an "or" of
   all the possible values.
*/
static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
set_flag(term_t arg, re_options_flags *options_flags, uint32_t mask, uint32_t value, int invert)
{ if ( options_flags->seen&mask )
    return TRUE; /* repeated option - ignore */
  options_flags->seen |= mask;
  options_flags->flags &= ~mask; /* clear the bits where the value goes */
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
    default: /* -1 - Error: neither FALSE nor TRUE */
      return FALSE;
  }
  return TRUE;
}


typedef struct re_optdef
{ const char *name;
  uint32_t    flag; /* Flag in pcre2.h */
  uint32_t    mode; /* RE_COMP_xxx, RE_MATCH_xxx, RE_NEG, etc. */
  atom_t      atom; /* Initially 0; filled in as-needed by lookup_optdef() */
} re_optdef;

#define RE_NEG           0x0001
#define RE_COMP_BOOL     0x0010
#define RE_COMP_BSR      0x0020  /* re_data.compile_options - \R */
#define RE_COMP_NEWLINE  0x0040  /* re_data.compile_options - newilne */
#define RE_COMP_OPTIMISE 0x0080  /* TODO: see also RE_COMPJIT_BOOL */
#define RE_COMP_COMPAT   0x0100  /* backward compatibility */
#define RE_COMP_CAPTURE  0x0200
#define RE_COMPCTX_BOOL  0x0400  /* re_data.compile_ctx_flags */
#define RE_COMPJIT_BOOL  0x0800  /* for pcre2_jit_compile() */
#define RE_MATCH_BOOL    0x1000  /* re_data.match_options */
#define RE_MATCH_START   0x2000
#define RE_SUBS_BOOL     0x4000  /* for pcre2_substitute() */


static const re_optdef*
lookup_optdef(re_optdef opt_defs[], atom_t name)
{ re_optdef *def;
  for(def=opt_defs; def->name; def++)
  { if ( !def->atom ) /* lazily fill in atoms in lookup table */
      def->atom = PL_new_atom(def->name);
    if ( def->atom == name )
      return def;
  }
  return NULL;
}


static int/* bool (FALSE/TRUE), as returned by PL_..._error() */
lookup_and_apply_optdef(re_optdef opt_defs[], atom_t name,
			term_t option_term, term_t arg, uint32_t mask,
			re_options_flags *options_flags)
{ const re_optdef *def = lookup_optdef(opt_defs, name);
  if ( def )
    return set_flag(arg, options_flags, mask, def->flag, def->mode&RE_NEG);
  return PL_type_error("option", option_term);
}

static int/* bool (FALSE/TRUE), as returned by PL_..._error() */
lookup_and_apply_optdef_arg(re_optdef opt_defs[], atom_t name,
			    term_t option_term, term_t arg, uint32_t mask,
			    re_options_flags *options_flags)
{ atom_t aval;
  return PL_get_atom_ex(name, &aval) &&
    lookup_and_apply_optdef(opt_defs, aval, option_term, arg, mask, options_flags);
}


static re_optdef re_optdefs[] =
{ /* Those with flag==0 are for options with names or numbers: */
  { "bsr",           0, RE_COMP_BSR },
  { "bsr2",          0, RE_COMP_BSR },
  { "capture_type",  0, RE_COMP_CAPTURE },
  { "compat",        0, RE_COMP_COMPAT },
  { "newline",       0, RE_COMP_NEWLINE },
  { "newline2",      0, RE_COMP_NEWLINE },
  { "optimise",      0, RE_COMP_OPTIMISE },
  { "optimize",      0, RE_COMP_OPTIMISE },
  { "start",         0, RE_MATCH_START },

  /* The following are in the same order as in pcre2.h, to make it easy to compare them */
  { "anchored",		    PCRE2_ANCHORED,		RE_COMP_BOOL|RE_MATCH_BOOL },
  { "utf_check",	    PCRE2_NO_UTF_CHECK,		RE_COMP_BOOL|RE_MATCH_BOOL },
  { "endanchored",	    PCRE2_ENDANCHORED,		RE_COMP_BOOL|RE_MATCH_BOOL },

  { "allow_empty_class",    PCRE2_ALLOW_EMPTY_CLASS,	RE_COMP_BOOL },
  { "alt_bsux",		    PCRE2_ALT_BSUX,		RE_COMP_BOOL },
  { "auto_callout",	    PCRE2_AUTO_CALLOUT,		RE_COMP_BOOL },
  { "caseless",		    PCRE2_CASELESS,		RE_COMP_BOOL },
  { "dollar_endonly",	    PCRE2_DOLLAR_ENDONLY,	RE_COMP_BOOL },
  { "dotall",		    PCRE2_DOTALL,		RE_COMP_BOOL },
  { "dupnames",		    PCRE2_DUPNAMES,		RE_COMP_BOOL },
  { "extended",		    PCRE2_EXTENDED,		RE_COMP_BOOL },
  { "firstline",	    PCRE2_FIRSTLINE,		RE_COMP_BOOL },
  { "match_unset_backref",  PCRE2_MATCH_UNSET_BACKREF,	RE_COMP_BOOL },
  { "multiline",	    PCRE2_MULTILINE,		RE_COMP_BOOL },
  { "never_ucp",	    PCRE2_NEVER_UCP,		RE_COMP_BOOL },
  { "never_utf",	    PCRE2_NEVER_UTF,		RE_COMP_BOOL },
  { "auto_capture",	    PCRE2_NO_AUTO_CAPTURE,	RE_COMP_BOOL|RE_NEG },
  { "no_auto_capture",	    PCRE2_NO_AUTO_CAPTURE,	RE_COMP_BOOL },	   /* backwards compatibility */
  { "auto_possess",	    PCRE2_NO_AUTO_POSSESS,	RE_COMP_BOOL|RE_NEG },
  { "dotstar_anchor",       PCRE2_NO_DOTSTAR_ANCHOR,	RE_COMP_BOOL|RE_NEG },
  { "start_optimize",       PCRE2_NO_START_OPTIMIZE,	RE_COMP_BOOL|RE_NEG },
  { "ucp",		    PCRE2_UCP,			RE_COMP_BOOL },
  { "greedy",		    PCRE2_UNGREEDY,		RE_COMP_BOOL|RE_NEG },
  { "ungreedy",		    PCRE2_UNGREEDY,		RE_COMP_BOOL },  /* backwards compatibility */
  { "utf",		    PCRE2_UTF,			RE_COMP_BOOL },
  { "never_backslash_c",    PCRE2_NEVER_BACKSLASH_C,	RE_COMP_BOOL },
  { "alt_circumflex",	    PCRE2_ALT_CIRCUMFLEX,	RE_COMP_BOOL },
  { "alt_verbnames",	    PCRE2_ALT_VERBNAMES,	RE_COMP_BOOL },
  { "use_offset_limit",	    PCRE2_USE_OFFSET_LIMIT,	RE_COMP_BOOL },
  { "extended_more",	    PCRE2_EXTENDED_MORE,	RE_COMP_BOOL },
  { "literal",		    PCRE2_LITERAL,		RE_COMP_BOOL },
#ifdef PCRE2_MATCH_INVALID_UTF
  { "match_invalid_utf",    PCRE2_MATCH_INVALID_UTF,	RE_COMP_BOOL },
#endif

  { "extra_allow_surrogate_escapes", PCRE2_EXTRA_ALLOW_SURROGATE_ESCAPES, RE_COMPCTX_BOOL },
  { "extra_bad_escape_is_literal",   PCRE2_EXTRA_BAD_ESCAPE_IS_LITERAL,	  RE_COMPCTX_BOOL },
  { "extra_match_word",	             PCRE2_EXTRA_MATCH_WORD,		  RE_COMPCTX_BOOL },
  { "extra_match_line",              PCRE2_EXTRA_MATCH_LINE,		  RE_COMPCTX_BOOL },
#ifdef PCRE2_EXTRA_ESCAPED_CR_IS_LF
  { "extra_escaped_cr_is_lf",        PCRE2_EXTRA_ESCAPED_CR_IS_LF,	  RE_COMPCTX_BOOL },
#endif
#ifdef PCRE2_EXTRA_ALT_BSUX
  { "extra_alt_bsux",                PCRE2_EXTRA_ALT_BSUX,		  RE_COMPCTX_BOOL },
#endif

  { "jit_complete",     PCRE2_JIT_COMPLETE,	RE_COMPJIT_BOOL },
  { "jit_partial_soft", PCRE2_JIT_PARTIAL_SOFT,	RE_COMPJIT_BOOL },
  { "jit_partial_hard", PCRE2_JIT_PARTIAL_HARD,	RE_COMPJIT_BOOL },
#ifdef PCRE2_JIT_INVALID_UTF
  { "jit_invalid_utf",  PCRE2_JIT_INVALID_UTF ,	RE_COMPJIT_BOOL },
#endif

  /* Some of the follwoing are for pcre2_dfa_match() or
     pcre2_substitute(), but they're all in the same flag */
  { "bol",                        PCRE2_NOTBOL,                     RE_MATCH_BOOL|RE_NEG },
  { "eol",                        PCRE2_NOTEOL,                     RE_MATCH_BOOL|RE_NEG },
  { "empty",                      PCRE2_NOTEMPTY,                   RE_MATCH_BOOL|RE_NEG },
  { "empty_atstart",              PCRE2_NOTEMPTY_ATSTART,           RE_MATCH_BOOL|RE_NEG },
  { "partial_soft",               PCRE2_PARTIAL_SOFT,               RE_MATCH_BOOL },
  { "partial_hard",               PCRE2_PARTIAL_HARD,               RE_MATCH_BOOL },
  { "dfa_restart",                PCRE2_DFA_RESTART,                RE_MATCH_BOOL },
  { "dfa_shortest",               PCRE2_DFA_SHORTEST,               RE_MATCH_BOOL },
  { "substitute_global",          PCRE2_SUBSTITUTE_GLOBAL,          RE_MATCH_BOOL },
  { "substitute_extended",        PCRE2_SUBSTITUTE_EXTENDED,        RE_MATCH_BOOL },
  { "substitute_unset_empty",     PCRE2_SUBSTITUTE_UNSET_EMPTY,     RE_MATCH_BOOL },
  { "substitute_unknown_unset",   PCRE2_SUBSTITUTE_UNKNOWN_UNSET,   RE_MATCH_BOOL },
  { "substitute_overflow_length", PCRE2_SUBSTITUTE_OVERFLOW_LENGTH, RE_MATCH_BOOL },
  { "jit",                        PCRE2_NO_JIT,                     RE_MATCH_BOOL|RE_NEG }, /* TODO: see comment in pcre2.h */
#ifdef PCRE2_COPY_MATCHED_SUBJECT
  { "copy_matched_subject",       PCRE2_COPY_MATCHED_SUBJECT,       RE_MATCH_BOOL },
#endif

  /* TODO: PCRE2_CONVERT_xxx if we support (experimental) pcre2_pattern_convert() */

  { NULL }
};

static re_optdef re_optbsrs[] =
{ { "anycrlf",	       PCRE2_BSR_ANYCRLF,      RE_COMP_BSR },
  { "unicode",	       PCRE2_BSR_UNICODE,      RE_COMP_BSR },
  { NULL }
};

#define OPTBSR_MASK (PCRE2_BSR_ANYCRLF|PCRE2_BSR_UNICODE)


static re_optdef re_optnewlines[] =
/* These are in the same order as in pcre2.h, to make it easy to compare them */
{ { "cr",		    PCRE2_NEWLINE_CR,		RE_COMP_NEWLINE },
  { "lf",		    PCRE2_NEWLINE_LF,		RE_COMP_NEWLINE },
  { "crlf",		    PCRE2_NEWLINE_CRLF,		RE_COMP_NEWLINE },
  { "any",		    PCRE2_NEWLINE_ANY,		RE_COMP_NEWLINE },
  { "anycrlf",		    PCRE2_NEWLINE_ANYCRLF,	RE_COMP_NEWLINE },
  { "nul",		    PCRE2_NEWLINE_NUL,		RE_COMP_NEWLINE },
  { NULL }
};

#define OPTNEWLINE_MASK (PCRE2_NEWLINE_CR|PCRE2_NEWLINE_LF|PCRE2_NEWLINE_CRLF|PCRE2_NEWLINE_ANY|PCRE2_NEWLINE_ANYCRLF)

/* This had "javascript" for PCRE1; the option doesn't exist in PCRE2,
   so this empty array causes an error for compat(javascript). */
static re_optdef re_optcompats[] =
{ { NULL }
};

#define OPTCOMPAT_MASK 0

static re_optdef re_optcaptures[] =
{ { "atom",    CAP_ATOM,    RE_COMP_CAPTURE },
  { "float",   CAP_FLOAT,   RE_COMP_CAPTURE },
  { "integer", CAP_INTEGER, RE_COMP_CAPTURE },
  { "number",  CAP_NUMBER,  RE_COMP_CAPTURE },
  { "range",   CAP_RANGE,   RE_COMP_CAPTURE },
  { "string",  CAP_STRING,  RE_COMP_CAPTURE },
  { "term",    CAP_TERM,    RE_COMP_CAPTURE },
  { NULL }
};

#define OPTCAPTURE_MASK (CAP_ATOM|CAP_FLOAT|CAP_INTEGER|CAP_NUMBER|CAP_RANGE|CAP_STRING|CAP_TERM|CAP_DEFAULT)

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

static int /* bol (FALSE/TRUE), as returned by PL_...error() */
ensure_compile_context(pcre2_compile_context **compile_ctx)
{ if ( !*compile_ctx )
    *compile_ctx = pcre2_compile_context_create(NULL);
  if ( !*compile_ctx )
    return PL_resource_error("memory");
  return TRUE;
}


/* re_get_options assumes that re has been initialized by init_re_data() */
/* This is called in two ways:
   - re_compile: a new re_data (from init_re_data())
   - re_matchsub/re_foldl: a re_data that's already been set up by
     re_compile - it's kept as part of the blob - but with initialized
     match options (from get_re_copy()). If the same options list is
     used by both compile and match, the compile-only options will
     already have "seen" flags, so they'll be skipped; only the match
     options will be processed. Any new compile options will be
     processed but there's nothing in the matching code that uses
     them, nor any check that they're ignored.
*/
static int /* bool (FALSE/TRUE), as returned by PL_..._error() */
re_get_options(term_t options, re_data *re,
	       pcre2_compile_context **compile_ctx) /* Can be NULL; caller must free */
{ term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();

  while(PL_get_list_ex(tail, head, tail))
  { atom_t name;
    size_t arity;
    term_t arg;
    if ( !get_arg_1_if_any(head, &name, &arity, &arg) )
      return FALSE;
    { const re_optdef *def = lookup_optdef(re_optdefs, name);
      /* Some options can appear in multiple situations, so we can't
	 do "else if"s. */
      if ( def )
      { if ( def->mode&RE_COMP_BOOL )
	{ if ( !set_flag(arg, &re->compile_options_flags, def->flag, def->flag, def->mode&RE_NEG) )
	    return FALSE;
	}
	if ( def->mode&RE_MATCH_BOOL )
	{ if ( !set_flag(arg, &re->match_options_flags, def->flag, def->flag, def->mode&RE_NEG) )
	    return FALSE;
	}
	if ( def->mode&RE_COMP_BSR )
	{ if ( !lookup_and_apply_optdef_arg(re_optbsrs, arg,
					    head, 0, OPTBSR_MASK, &re->compile_bsr_flags) )
	    return FALSE;
	}
	if ( def->mode&RE_COMP_NEWLINE )
	{ if ( !lookup_and_apply_optdef_arg(re_optnewlines, arg,
					    head, 0, OPTNEWLINE_MASK, &re->compile_newline_flags) )
	    return FALSE;
	}
	if ( def->mode&RE_COMP_COMPAT )
	{ if ( !lookup_and_apply_optdef_arg(re_optcompats, arg,
					   head, 0, OPTCOMPAT_MASK, &re->compile_options_flags) )
	    return FALSE;
	}
	if ( def->mode&RE_COMP_CAPTURE )
	{ if ( !lookup_and_apply_optdef_arg(re_optcaptures, arg,
					    head, 0, OPTCAPTURE_MASK, &re->capture_type) )
	    return FALSE;
	}
	if ( def->mode&RE_COMP_OPTIMISE )
	{ if ( !set_flag(arg, &re->optimise_flags, RE_OPTIMISE, RE_OPTIMISE, def->mode&RE_NEG) )
	    return FALSE;
	}
	if ( def->mode&RE_COMPCTX_BOOL )
	{ if ( !set_flag(arg, &re->compile_ctx_flags, def->flag, def->flag, def->mode&RE_NEG) )
	    return FALSE;
	}
	if ( def->mode&RE_COMPJIT_BOOL )
	{ if ( !set_flag(arg, &re->jit_options_flags, def->flag, def->flag, def->mode&RE_NEG) )
	    return FALSE;
	}
	if ( def->mode&RE_MATCH_START ) /* TODO: generalize set_flag() and remove duplicate code */
	{ uint64_t start_value;
	  if ( !PL_get_uint64_ex(arg, &start_value) )
	    return FALSE;
	  if ( start_value > (uint64_t)OPTSTART_MASK )
	    return PL_domain_error("int32", arg);
	  /* TODO: Use set_flags() style of checking "seen" etc */
	  /* TODO: allow 64-bit sizes */
	  if ( !(re->start_flags.seen) )
	  { re->start_flags.seen = 1;
	    re->start_flags.flags = start_value;
	  }
	}
      } /* else: ignore unknown option */
    }
  }

  return PL_get_nil_ex(tail);
}


		 /*******************************
		 *	     PREDICATES		*
		 *******************************/

typedef enum re_config_type
{ CFG_INTEGER,
  CFG_INTEGER_BKWD,	/* Backwards compatibility with PCRE1 */
  CFG_BOOL,
  CFG_BOOL_BKWD,	/* Backwards compatibility with PCRE1 */
  CFG_STRINGBUF,
  CFG_STRINGBUF_OPT,	/* STRINGBUF that might return PCRE2_ERROR_BADOPTION */
  CFG_BSR,
  CFG_NEWLINE,
  CFG_TRUE_BKWD,	/* Backwards compatibility with PCRE1 */
  CFG_FALSE_BKWD,	/* Backwards compatibility with PCRE1 */
  CFG_INVALID
} re_config_type;


typedef struct re_config_opt
{ char		 *name;
  int		  id;
  re_config_type  type;
  atom_t	  atom;    /* Initially 0; filled in as-needed by re_config_() */
  functor_t       functor; /* Initially 0; filled in as-needed by re_config_choices_() */
} re_config_opt;

/* Items with id == -1 are for backwards compatibility with PCRE1 */
/* "bsr" and "newline" have been removed because they return different things now */
static re_config_opt cfg_opts[] =
{ { "bsr2",		      PCRE2_CONFIG_BSR,			   CFG_BSR },
  { "compiled_widths",	      PCRE2_CONFIG_COMPILED_WIDTHS,	   CFG_INTEGER },   /* PCRE2 */
  { "depthlimit",	      PCRE2_CONFIG_DEPTHLIMIT,		   CFG_INTEGER },   /* PCRE2 */
  { "heaplimit",	      PCRE2_CONFIG_HEAPLIMIT,		   CFG_INTEGER },   /* PCRE2 */
  { "jit",		      PCRE2_CONFIG_JIT,			   CFG_BOOL },      /* PCRE2 */
  { "jittarget",	      PCRE2_CONFIG_JITTARGET,		   CFG_STRINGBUF_OPT }, /* PCRE2 */
  { "link_size",	      PCRE2_CONFIG_LINKSIZE,		   CFG_INTEGER_BKWD },
  { "linksize",		      PCRE2_CONFIG_LINKSIZE,		   CFG_INTEGER },   /* PCRE2 */ /* was LINK_SIZE */
  { "match_limit",	      PCRE2_CONFIG_MATCHLIMIT,		   CFG_INTEGER_BKWD },
  { "match_limit_recursion",  -1,				   CFG_INVALID },
  { "matchlimit",	      PCRE2_CONFIG_MATCHLIMIT,		   CFG_INTEGER },   /* PCRE2 */
  { "never_backslash_c",      PCRE2_CONFIG_NEVER_BACKSLASH_C,	   CFG_BOOL },	    /* PCRE2 */
  { "newline2",		      PCRE2_CONFIG_NEWLINE,		   CFG_NEWLINE },
  { "parens_limit",	      PCRE2_CONFIG_PARENSLIMIT,		   CFG_INTEGER_BKWD },
  { "parenslimit",	      PCRE2_CONFIG_PARENSLIMIT,		   CFG_INTEGER },   /* PCRE2 */ /* was PARENS_LIIMT */
  { "posix_malloc_threshold", -1,				   CFG_INVALID },
  { "stackrecurse",	      PCRE2_CONFIG_STACKRECURSE,	   CFG_BOOL },
  { "unicode",		      PCRE2_CONFIG_UNICODE,		   CFG_BOOL },	    /* PCRE2 */
  { "unicode_properties",     -1,				   CFG_TRUE_BKWD },
  { "unicode_version",	      PCRE2_CONFIG_UNICODE_VERSION,	   CFG_STRINGBUF }, /* PCRE2 */
  { "utf8",		      PCRE2_CONFIG_UNICODE,		   CFG_BOOL_BKWD },
  { "version",		      PCRE2_CONFIG_VERSION,		   CFG_STRINGBUF }, /* PCRE2 */
  /*			      PCRE2_CONFIG_RECURSIONLIMIT Obsolete synonym */

  { NULL }
};

static intptr_t
next_config(intptr_t i)
{ re_config_opt *def = &cfg_opts[i];
  for( ; def->name; def++ )
  { switch(def->type)
    { case CFG_INVALID:
      case CFG_BOOL_BKWD:
      case CFG_INTEGER_BKWD:
      case CFG_TRUE_BKWD:
      case CFG_FALSE_BKWD:
	break;
      default:
	if ( !def->atom ) /* lazily fill in atoms in lookup table ... */
	  def->atom = PL_new_atom(def->name);
	if ( !def->functor ) /* ... and functors */
	  def->functor = PL_new_functor(def->atom, 1);
	return def - cfg_opts;
    }
  }
  return -1;
}

/** re_config_choice(--Choice) is nondet.

    Unify Choice with an option (a term with the argument
    uninstantiated) for re_config/1. Backtracks through all
    possibilities. Used by re_config/1 when its argument is
    uninstantiated. It's intended that re_config_choice/1 is called
    with Choice uninstantiated, to allow backtracking through all the
    possibilities (see re_config/1, which calls re_config_().

    The context "handle" is the index into cfg_opts - initially 0, and
    incremented by 1 for each retry. next_config() is used to get the
    next entry, skipping those that are now invalid or are for
    backwards compatibility.
*/
static foreign_t
re_config_choice_(term_t choice, control_t handle)
{ intptr_t index;

  /* This code could be made slightly more efficient by passing around
     the next value of index -- this would avoid a final choicepoint.
  */

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
      index = 0;
      break;
    case PL_REDO:
      index = PL_foreign_context(handle);
      break;
    case PL_PRUNED:
      PL_succeed;
    default:
      assert(0);
      PL_fail;
  }

  if ( !PL_is_variable(choice) )
    return PL_uninstantiation_error(choice);

  index = next_config(index);
  if ( index >= 0 &&
       PL_unify_functor(choice, cfg_opts[index].functor) )
    PL_retry(index + 1);
  else
    PL_fail;
}


#ifdef FORCE_NO_JIT
static int
pcre2_config_(uint32_t flag, void *result)
{
  if ( !result )
    return pcre2_config(flag, result);

  switch( flag )
  { case PCRE2_CONFIG_JIT:
      *(uint32_t *)result = 0;
      return 0;
    case PCRE2_CONFIG_JITTARGET:
      return PCRE2_ERROR_BADOPTION;
    default:
      return pcre2_config(flag, result);
  }
}
#else
#define pcre2_config_(flag, result) pcre2_config(flag, result)
#endif


/** re_config(+Term)

    For documentation of this function, see pcre.pl
*/
static foreign_t
re_config_(term_t opt)
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
	  { uint32_t i_unsigned;
	    int32_t  i_signed;
	    char     s_buf[100]; /* PCRE2_CONFIG_JITTARGET requires at least 48 */
	  } val;

	  /* pcre2_config() returns 0 for int, # bytes for stringbuf, PCRE2_ERROR_BADOPTION (negative) for error */
	  if ( pcre2_config_(o->id, &val) >= 0 )
	  { switch(o->type)
	    { case CFG_BOOL:
	      case CFG_BOOL_BKWD:
		return PL_unify_bool(arg, val.i_signed);
	      case CFG_INTEGER:
	      case CFG_INTEGER_BKWD:
		return PL_unify_integer(arg, val.i_signed);
	      case CFG_STRINGBUF:
	      case CFG_STRINGBUF_OPT:
		return PL_unify_atom_chars(arg, val.s_buf);
	      case CFG_BSR:
		switch(val.i_unsigned)
		{ case PCRE2_BSR_UNICODE:
		    return PL_unify_atom_chars(arg, "unicode");
		  case PCRE2_BSR_ANYCRLF:
		    return PL_unify_atom_chars(arg, "anycrlf");
		  default:
		    Sdprintf("CFG_BSR: 0x%08x\n", val.i_unsigned);
		    assert(0);
		}
	      case CFG_NEWLINE:
		{ const char *c;
		  switch(val.i_unsigned)
		  { case PCRE2_NEWLINE_CR:      c = "cr";      break;
		    case PCRE2_NEWLINE_LF:      c = "lf";      break;
		    case PCRE2_NEWLINE_CRLF:    c = "crlf";    break;
		    case PCRE2_NEWLINE_ANY:     c = "any";     break;
		    case PCRE2_NEWLINE_ANYCRLF: c = "anycrlf"; break;
		    case PCRE2_NEWLINE_NUL:     c = "nul";     break;
		    default:
		      Sdprintf("CFG_NEWLINE: 0x%08x\n", val.i_unsigned);
		      c = "???";
		      assert(0);
		  }
		  return PL_unify_atom_chars(arg, c);
		}
	      case CFG_INVALID:
		return FALSE; /* was: PL_existence_error("re_config", opt); */
	      case CFG_TRUE_BKWD:
		return PL_unify_bool(arg, 1);
	      case CFG_FALSE_BKWD:
		return PL_unify_bool(arg, 0);
	      default:
		Sdprintf("PCRE2_CONFIG type(1): 0x%08x", o->type);
		assert(0);
	    }
	  } else
	  { switch(o->type)
	    { case CFG_TRUE_BKWD:
		return PL_unify_bool(arg, 1);
	      case CFG_FALSE_BKWD:
		return PL_unify_bool(arg, 0);
	      case CFG_INVALID:
	      case CFG_STRINGBUF_OPT:
		return FALSE; /* was: PL_existence_error("re_config", opt); */
	      default:
		Sdprintf("PCRE2_CONFIG type(2): 0x%08x", o->type);
		assert(0);
	    }
	  }
	}
      }

      return FALSE; /* was: PL_existence_error("re_config", opt); */
    }
    return FALSE; /* was: PL_type_error("compound", opt); */
  }

  return FALSE; /* was: PL_type_error("compound", opt); */
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
    re->capture_names[ci].type = CAP_DEFAULT;
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
  if ( pcre2_pattern_info(re->pcre2_code, PCRE2_INFO_CAPTURECOUNT,  &re->capture_size)!=0 ||
       pcre2_pattern_info(re->pcre2_code, PCRE2_INFO_NAMECOUNT,     &name_count)      !=0 ||
       pcre2_pattern_info(re->pcre2_code, PCRE2_INFO_NAMEENTRYSIZE, &name_entry_size) !=0 ||
       pcre2_pattern_info(re->pcre2_code, PCRE2_INFO_NAMETABLE,     &table)	      !=0 )
    return PL_resource_error("pcre2_pattern_info");
  if ( ! (re->capture_names = malloc((re->capture_size+1) * sizeof (cap_how))) )
    return PL_resource_error("memory");
  for(i=0; i<re->capture_size+1; i++)
  { re->capture_names[i].name = 0;
    re->capture_names[i].type = CAP_DEFAULT;
  }
  for(i=0; i<name_count; i++, table += name_entry_size)
  { if ( !set_capture_name_and_type(&table[2], re,
				    ((table[0]&0xff)<<8) + (table[1]&0xff)) )
      return FALSE;
  }
  return TRUE;
}


static int /* FALSE or TRUE */
get_pcre2_info(IOSTREAM *s, const re_data *re, uint32_t info_type, const char *descr, uint32_t *result)
{ int rc;
  if ( !re->pcre2_code )
    return FALSE; /* write_re_options() has already output a message */
  rc = pcre2_pattern_info(re->pcre2_code, info_type, result);
  switch( rc )
  { case 0:
      return TRUE;
    case PCRE2_ERROR_NULL:
      Sfprintf(s, "<%s:ERROR_NULL>", descr);
      return FALSE;
    case PCRE2_ERROR_BADMAGIC:
      Sfprintf(s, "<%s:ERROR_BADMAGIC>", descr);
      return FALSE;
    case PCRE2_ERROR_BADOPTION:
      Sfprintf(s, "<%s:ERROR_BADOPTION>", descr);
      return FALSE;
    case PCRE2_ERROR_UNSET: /* TODO: ?? *result = 0; return TRUE */
      Sfprintf(s, "<%s:ERROR_UNSET>", descr);
      return FALSE;
    default:
      Sfprintf(s, "<%s:ERROR_[%d]>", descr, rc);
      return FALSE;
  }
}

static void
write_option_str(IOSTREAM *s, const char **sep, uint32_t *flags, uint32_t field_flag, const char *name)
{ if ( (*flags)&field_flag )
  { Sfprintf(s, "%s%s", *sep, name);
    *sep = " ";
    *flags &= ~field_flag;
  }
}

static void
write_re_options(IOSTREAM *s, const char **sep, const re_data *re)
{ uint32_t ui;

  if ( !re->pcre2_code_8 )
  { Sfprintf(s, "%s<no pcre2_code>", *sep);
    *sep = " ";
  }

  /* The various options are in the order given in pcre2.h, to make it easy to compare them. */

  /* PCRE2_INFO_ALL_OPTIONS combines options in the pattern with arg options;
     PCRE2_INFO_ARGOPTIONS gets just the options to pcre2_compile(). */
  // if ( get_pcre2_info(s, re, PCRE2_INFO_ALLOPTIONS, "INFO_ALLOPTIONS", &ui) )
  if ( get_pcre2_info(s, re, PCRE2_INFO_ARGOPTIONS, "INFO_ARGOPTIONS", &ui) )
  { /* Sfprintf(s, "<all:0x%08x>", ui); */
    /* Special handling for PCRE2_NO_UTF_CHECK and PCRE2_UTF, which default to true */
    if ( !(ui&PCRE2_NO_UTF_CHECK) )
    { Sfprintf(s, "%s%s", *sep, "compile-~NO_UTF_CHECK"); *sep = " ";
    }
    ui &= ~PCRE2_NO_UTF_CHECK;
    if ( !(ui&PCRE2_UTF) )
    { Sfprintf(s, "%s%s", *sep, "compile-~UTF"); *sep = " ";
    }
    ui &= ~PCRE2_UTF;
    write_option_str(s, sep, &ui, PCRE2_ANCHORED,            "compile-ANCHORED");
    write_option_str(s, sep, &ui, PCRE2_ENDANCHORED,         "compile-ENDANCHORED");
    write_option_str(s, sep, &ui, PCRE2_ALLOW_EMPTY_CLASS,   "ALLOW_EMPTY_CLASS");
    write_option_str(s, sep, &ui, PCRE2_ALT_BSUX,            "ALT_BSUX");
    write_option_str(s, sep, &ui, PCRE2_AUTO_CALLOUT,        "AUTO_CALLOUT");
    write_option_str(s, sep, &ui, PCRE2_CASELESS,            "CASELESS");
    write_option_str(s, sep, &ui, PCRE2_DOLLAR_ENDONLY,      "DOLLAR_ENDONLY");
    write_option_str(s, sep, &ui, PCRE2_DOTALL,              "DOTALL");
    write_option_str(s, sep, &ui, PCRE2_DUPNAMES,            "DUPNAMES");
    write_option_str(s, sep, &ui, PCRE2_EXTENDED,            "EXTENDED");
    write_option_str(s, sep, &ui, PCRE2_FIRSTLINE,           "FIRSTLINE");
    write_option_str(s, sep, &ui, PCRE2_MATCH_UNSET_BACKREF, "MATCH_UNSET_BACKREF");
    write_option_str(s, sep, &ui, PCRE2_MULTILINE,           "MULTILINE");
    write_option_str(s, sep, &ui, PCRE2_NEVER_UCP,           "NEVER_UCP");
    write_option_str(s, sep, &ui, PCRE2_NEVER_UTF,           "NEVER_UTF");
    write_option_str(s, sep, &ui, PCRE2_NO_AUTO_CAPTURE,     "NO_AUTO_CAPTURE");
    write_option_str(s, sep, &ui, PCRE2_NO_AUTO_POSSESS,     "NO_AUTO_POSSESS");
    write_option_str(s, sep, &ui, PCRE2_NO_DOTSTAR_ANCHOR,   "NO_DOTSTAR_ANCHOR");
    write_option_str(s, sep, &ui, PCRE2_NO_START_OPTIMIZE,   "NO_START_OPTIMIZE");
    write_option_str(s, sep, &ui, PCRE2_UCP,                 "UCP");
    write_option_str(s, sep, &ui, PCRE2_UNGREEDY,            "UNGREEDY");
    write_option_str(s, sep, &ui, PCRE2_NEVER_BACKSLASH_C,   "NEVER_BACKSLASH_C");
    write_option_str(s, sep, &ui, PCRE2_ALT_CIRCUMFLEX,      "ALT_CIRCUMFLEX");
    write_option_str(s, sep, &ui, PCRE2_ALT_VERBNAMES,       "ALT_VERBNAMES");
    write_option_str(s, sep, &ui, PCRE2_USE_OFFSET_LIMIT,    "USE_OFFSET_LIMIT");
    write_option_str(s, sep, &ui, PCRE2_EXTENDED_MORE,       "EXTENDED_MORE");
    write_option_str(s, sep, &ui, PCRE2_LITERAL,             "LITERAL");
#ifdef PCRE2_MATCH_INVALID_UTF
    write_option_str(s, sep, &ui, PCRE2_MATCH_INVALID_UTF,   "MATCH_INVALID_UTF");
#endif
    if ( ui )
    { Sfprintf(s, "%s<all:remainder:0x%08x>", *sep, ui);
      *sep = " ";
    }
   }

  if ( get_pcre2_info(s, re, PCRE2_INFO_EXTRAOPTIONS, "INFO_EXTRAOPTIONS", &ui) )
  { /* Sfprintf(s, "<extra:0x%08x>", ui); */
    write_option_str(s, sep, &ui, PCRE2_EXTRA_ALLOW_SURROGATE_ESCAPES, "EXTRA_ALLOW_SURROGATE_ESCAPES");
    write_option_str(s, sep, &ui, PCRE2_EXTRA_BAD_ESCAPE_IS_LITERAL,   "EXTRA_BAD_ESCAPE_IS_LITERAL");
    write_option_str(s, sep, &ui, PCRE2_EXTRA_MATCH_WORD,              "EXTRA_MATCH_WORD");
    write_option_str(s, sep, &ui, PCRE2_EXTRA_MATCH_LINE,              "EXTRA_MATCH_LINE");
#ifdef PCRE2_EXTRA_ESCAPED_CR_IS_LF
    write_option_str(s, sep, &ui, PCRE2_EXTRA_ESCAPED_CR_IS_LF,        "EXTRA_ESCAPED_CR_IS_LF");
#endif
#ifdef PCRE2_EXTRA_ALT_BSUX
    write_option_str(s, sep, &ui, PCRE2_EXTRA_ALT_BSUX,                "EXTRA_ALT_BSUX");
#endif
    if ( ui )
    { Sfprintf(s, "%s<all:remainder:0x%08x>", *sep, ui);
      *sep = " ";
    }
  }

  /* TODO:  pcre2_jit_compile() options (RE_COMPJIT_BOOL options):
     PCRE2_JIT_COMPLETE
     PCRE2_JIT_PARTIAL_SOFT
     PCRE2_JIT_PARTIAL_HARD
     PCRE2_JIT_INVALID_UTF
  */

  if ( get_pcre2_info(s, re, PCRE2_INFO_BSR, "INFO_BSR", &ui) )
  { const char *c;
    /* Sfprintf(s, "<bsr:0x%08x>", ui); */
    switch( ui )
    { case PCRE2_BSR_ANYCRLF: c = "BSR_ANYCRLF"; break;
      case PCRE2_BSR_UNICODE: c = "BSR_UNICODE"; break;
      default:
	Sdprintf("GET_PCRE2_INFO_BSR: 0x%08x\n", ui);
	c = "?";
	assert(0);
      }
    Sfprintf(s, "%s%s", *sep, c);
    *sep = " ";
  }

  if (get_pcre2_info(s, re, PCRE2_INFO_NEWLINE, "INFO_NEWLINE", &ui) )
  { uint32_t config_ui = 0;
    int rc_c = pcre2_config_(PCRE2_CONFIG_NEWLINE, &config_ui);
    /* Sfprintf(s, "<newline:0x%08x/config:0x%08x>", ui, config_ui); */
    /* TODO: verify that the following works on Unix, MacOs, Windows: */
    if ( rc_c >= 0 &&
	 ( ((config_ui == PCRE2_NEWLINE_CRLF) && (ui == PCRE2_NEWLINE_CRLF)) ||
	   ((config_ui == PCRE2_NEWLINE_CR)   && (ui == PCRE2_NEWLINE_CR))   ||
	   ((config_ui == PCRE2_NEWLINE_LF)   && (ui == PCRE2_NEWLINE_LF)) ) )
    { /* do nothing */
    } else
    { const char *c;
      /* Sfprintf(s, "<newline:0x%08x:::0x%08x>", ui, config_ui); */
      switch( ui )
      { case PCRE2_NEWLINE_CR:      c = "NEWLINE_CR";      break;
	case PCRE2_NEWLINE_LF:      c = "NEWLINE_LF";      break;
	case PCRE2_NEWLINE_CRLF:    c = "NEWLINE_CRLF";    break;
	case PCRE2_NEWLINE_ANY:     c = "NEWLINE_ANY";     break;
	case PCRE2_NEWLINE_ANYCRLF: c = "NEWLINE_ANYCRLF"; break;
	case PCRE2_NEWLINE_NUL:     c = "NEWLINE_NUL";     break;
	default:
	  Sdprintf("GET_PCRE2_INFO_NEWLINE: 0x%08x\n", ui);
	  c = "?";
	  assert(0);
      }
      Sfprintf(s, "%s%s", *sep, c);
    }
  }

  { ui = re->match_options_flags.flags;
    /* Sfprintf(s, "<match:0x%08x>", ui); */
    /* Note that some of these are for pcre2_dfa_match() or
       pcre2_substitute(), but they're all in the same flag */
    /* Special handling for PCRE2_NO_UTF_CHECK, which defaults to true */
    if ( !(ui&PCRE2_NO_UTF_CHECK) )
    { Sfprintf(s, "%s%s", *sep, "match-~NO_UTF_CHECK");
      *sep = " ";
    }
    ui &= ~PCRE2_NO_UTF_CHECK;
    write_option_str(s, sep, &ui, PCRE2_ANCHORED,                   "match-ANCHORED");
    write_option_str(s, sep, &ui, PCRE2_ENDANCHORED,                "match-ENDANCHORED");
    write_option_str(s, sep, &ui, PCRE2_NOTBOL,                     "NOTBOL");
    write_option_str(s, sep, &ui, PCRE2_NOTEOL,                     "NOTEOL");
    write_option_str(s, sep, &ui, PCRE2_NOTEMPTY,                   "NOTEMPTY");
    write_option_str(s, sep, &ui, PCRE2_NOTEMPTY_ATSTART,           "NOTEMPTY_ATSTART");
    write_option_str(s, sep, &ui, PCRE2_PARTIAL_SOFT,               "PARTIAL_SOFT");
    write_option_str(s, sep, &ui, PCRE2_PARTIAL_HARD,               "PARTIAL_HARD");
    write_option_str(s, sep, &ui, PCRE2_DFA_RESTART,                "DFA_RESTART");
    write_option_str(s, sep, &ui, PCRE2_DFA_SHORTEST,               "DFA_SHORTEST");
    write_option_str(s, sep, &ui, PCRE2_SUBSTITUTE_GLOBAL,          "SUBSTITUTE_GLOBAL");
    write_option_str(s, sep, &ui, PCRE2_SUBSTITUTE_EXTENDED,        "SUBSTITUTE_EXTENDED");
    write_option_str(s, sep, &ui, PCRE2_SUBSTITUTE_UNSET_EMPTY,     "SUBSTITUTE_UNSET_EMPTY");
    write_option_str(s, sep, &ui, PCRE2_SUBSTITUTE_UNKNOWN_UNSET,   "SUBSTITUTE_UNKNOWN_UNSET");
    write_option_str(s, sep, &ui, PCRE2_SUBSTITUTE_OVERFLOW_LENGTH, "SUBSTITUTE_OVERFLOW_LENGTH");
    write_option_str(s, sep, &ui, PCRE2_NO_JIT,                     "NO_JIT");
#ifdef PCRE2_COPY_MATCHED_SUBJECT
    write_option_str(s, sep, &ui, PCRE2_COPY_MATCHED_SUBJECT,       "COPY_MATCHED_SUBJECT");
#endif
    if ( ui )
    { Sfprintf(s, "%s<all:remainder:0x%08x>", ui);
      *sep = " ";
    }
  }
}


/** re_portray(+Stream, +Regex) is det.

    Output a debug string for the regexp (from re_compile)
    ('$re_match_options'/2 handles the match options)
*/
static foreign_t
re_portray_(term_t stream, term_t regex)
{ IOSTREAM *fd;
  re_data re;
  const char *sep = "";
  if ( !PL_get_stream(stream, &fd, SIO_OUTPUT) || !PL_acquire_stream(fd) )
    return FALSE;
  if ( !get_re_copy(regex, &re) )
    return FALSE;
  Sfprintf(fd, "<regex>(/%s/ [", PL_atom_chars(re.pattern));
  write_re_options(fd, &sep, &re);
  Sfprintf(fd, "%s%s] $capture=%d", sep, cap_type_str(re.capture_type.flags), re.capture_size);
  sep = " ";
  if ( re.optimise_flags.flags&RE_OPTIMISE )
    Sfprintf(fd, "%s$optimise", sep);
  if ( re.capture_size && re.capture_names )
  { int i;
    const char* sep2 = "";
    Sfprintf(fd, "%s{", sep, re.capture_size);
    for(i=0; i<re.capture_size+1; i++)
    { if ( re.capture_names[i].name )
      { Sfprintf(fd, "%s%d:%s:%s", sep2, i, PL_atom_chars(re.capture_names[i].name), cap_type_str(re.capture_names[i].type));
	sep2 = " ";
      } else
      { Sfprintf(fd, "%s%d:%s", sep2, i, cap_type_str(re.capture_names[i].type));
	sep2 = " ";
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
re_compile_(term_t pat, term_t reb, term_t options)
{ int rc; /* Every path (to label out) must set rc */
  int must_free_blob = TRUE;
  size_t len;
  char *pats;
  pcre2_compile_context *compile_ctx = NULL;
  int re_error_code;
  PCRE2_SIZE re_error_offset;
  re_data re;
  init_re_data(&re);

  if ( !re_get_options(options, &re, &compile_ctx) )
  { rc = FALSE;
    goto out;
  }
  if ( !PL_get_nchars(pat, &len, &pats, GET_NCHARS_FLAGS) )
  { rc = FALSE;
    goto out;
  }
  if ( strlen(pats) != len )		/* TBD: escape as \0xx */
  { rc = PL_representation_error("nul_byte");
    goto out;
  }

  if ( re.compile_bsr_flags.flags )
  { ensure_compile_context(&compile_ctx);
    if ( 0 != pcre2_set_bsr(compile_ctx, re.compile_bsr_flags.flags) )
    { rc = PL_representation_error("option:bsr"); /* Should never happen */
      goto out;
    }
  }
  if ( re.compile_newline_flags.flags )
  { ensure_compile_context(&compile_ctx);
    if ( 0 != pcre2_set_newline(compile_ctx, re.compile_newline_flags.flags) )
    { rc = PL_representation_error("option:newline"); /* Should never happen */
      goto out;
    }
  }
  if ( re.compile_ctx_flags.flags )
  { ensure_compile_context(&compile_ctx);
    if ( 0 != pcre2_set_compile_extra_options(compile_ctx, re.compile_ctx_flags.flags) )
    { rc = PL_representation_error("option:extra"); /* Should never happen */
      goto out;
    }
  }

  /* pats is ptr to (signed) char; PCRE2_SPTR is ptr to uint8; they're
     compatible as far as we're concerned */
  if ( (re.pcre2_code = pcre2_compile((PCRE2_SPTR)pats, len, re.compile_options_flags.flags,
				      &re_error_code, &re_error_offset, compile_ctx) ) )
  { if ( re.optimise_flags.flags&RE_OPTIMISE )
    { pcre2_jit_compile(re.pcre2_code, re.jit_options_flags.flags);
      /* TBD: handle error that's not from no jit support, etc. */
      /* TODO: unit test to verify jit compile worked and
	       that options were handled properly - needs changes
	       to write_re_options() */
    }
    if ( !init_capture_map(&re) )
    { /* init_capture_map() has called an appropriate PL_..._error()
	 to indicate the cause; PL_..._error() returns FALSE, so
	 return that value. */
      rc = FALSE;
      goto out;
    }

    if ( (PL_get_atom(pat, &re.pattern)) )
      PL_register_atom(re.pattern);
    else
      re.pattern = PL_new_atom_mbchars(REP_UTF8, len, pats);
    must_free_blob = FALSE;
    rc = PL_unify_blob(reb, &re, sizeof re, &pcre2_blob);
    goto out;
  } else
  { PCRE2_UCHAR re_error_msg[256];
    pcre2_get_error_message(re_error_code, re_error_msg, sizeof re_error_msg - 1);
    rc = PL_syntax_error((const char *)re_error_msg, NULL); /* TBD: location, code */
    goto out;
  }

 out:
  pcre2_compile_context_free(compile_ctx);
  if ( must_free_blob )
    free_pcre(&re);
  return rc;
}


/** '$re_match_options'(+Stream, +Options) is det.

    Output the Options as a debug string.
    (re_portray/2 handles the compile options)
*/
static foreign_t
re_portray_match_options_(term_t stream, term_t options)
{ IOSTREAM *fd;
  re_data re;
  pcre2_compile_context *compile_ctx = NULL;
  const char *sep = "";
  int rc;
  init_re_data(&re);
  if ( !PL_get_stream(stream, &fd, SIO_OUTPUT) || !PL_acquire_stream(fd) )
    return FALSE;

  if ( !re_get_options(options, &re, &compile_ctx) )
  { rc = FALSE;
    goto out;
  }

  write_re_options(fd, &sep, &re);
  Sfprintf(fd, "%s$start=%lu", sep, (long unsigned)re.start_flags.flags);
  sep = " ";
  rc = PL_release_stream(fd);

 out:
  pcre2_compile_context_free(compile_ctx);
  return rc;
}


static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
out_of_range(size_t index)
{ term_t ex;

  return ( (ex=PL_new_term_ref()) &&
	   PL_put_int64(ex, index) &&
	   PL_domain_error("offset", ex) );
}


static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
put_capname(term_t t, const re_data *re, int i)
{ atom_t name;

  if ( re->capture_names && (name=re->capture_names[i].name) )
    return PL_put_atom(t, name);
  else
    return PL_put_integer(t, i);
}


static int  /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
put_capval(term_t t, const re_data *re, re_subject *subject, int i, const PCRE2_SIZE ovector[])
{ const char *s = &subject->subject[ovector[i*2]];
  int len = ovector[i*2+1]-ovector[i*2];
  cap_type ctype = re->capture_type.flags;

  if ( re->capture_names && re->capture_names[i].type != CAP_DEFAULT )
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
      Sdprintf("PUT_CAPVAL ctype: 0x%08x\n", ctype);
      assert(0);
      return FALSE;
  }
}

static int /* bool (FALSE/TRUE), as returned by PL_get_...() etc */
unify_match(term_t t, const re_data *re, re_subject *subject,
	    int ovsize, const PCRE2_SIZE *ovector)
{ int i;
  term_t av   = PL_new_term_refs(4);
  term_t capn = av+0;
  term_t caps = av+1;
  term_t pair = av+2;
  term_t list = av+3;
  /* Must guard against patterns such as /(?=.\K)/ that use \K in an
     assertion to set the start of a match later than its end. */
  /* TODO: don't just detect this case and give up. */

  if (ovector[0] > ovector[1])
  {
    /* TODO:  "From end to start the match was: %.*s\n", (int)(ovector[0] - ovector[1]),
							 (char *)(subject + ovector[1]))
    */
    return PL_representation_error("\\K used assertion to set the match start after its end");
  }

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

  { int rc = PL_unify(t, list);
    PL_reset_term_refs(av);
    return rc;
  }
}


static int /* bool (FALSE/TRUE), as returned by PL_..._error() */
re_error(int ec) /* ec is error code from pcre2_match() etc */
{ switch(ec)
  { case 0:				/* Too short ovector */
      Sdprintf("RE_ERROR: 0\n");
      assert(0); /* Should not happen because we used pcre2_match_data_create_from_pattern() */
    case PCRE2_ERROR_NOMATCH:
      return FALSE;
    case PCRE2_ERROR_NULL:
    case PCRE2_ERROR_BADOPTION:
    case PCRE2_ERROR_BADMAGIC:
      return PL_representation_error("regex");
    case PCRE2_ERROR_NOMEMORY:
      return PL_resource_error("memory");
    case PCRE2_ERROR_MATCHLIMIT:
      return PL_resource_error("match_limit");
    case PCRE2_ERROR_BADOFFSET: /* Should be caught by utf8_seek() */
      return PL_representation_error("regex-offset");
    case PCRE2_ERROR_PARTIAL: /* TODO: do we need to handle this? See PCRE2_PARTIAL_HARD, PCRE2_PARTIAL_SOFT options */
    /* TODO: add all the other pcre2_compile(), pcre2_match(), etc. codes here
	     and verify that they shouldn't happen. */
    default:
      Sdprintf("RE_ERROR: 0x%08x\n", ec);
      assert(0);			/* TBD */
  }

  return FALSE;
}


/** re_matchsub_(+Regex, +String, -Sub:dict, +Options) is semidet.

    For documentation of this function, see pcre.pl
*/
static foreign_t
re_matchsub_(term_t regex, term_t on, term_t result, term_t options)
{ re_data re;
  re_subject subject;
  int rc; /* Every path (to label out) must set rc */
  pcre2_match_data *match_data;
  size_t utf8_start;
  init_subject(&subject);

  if ( !get_re_copy(regex, &re) )
    return FALSE;
  if ( !re_get_subject(on, &subject, 0) )
    return FALSE;
  if ( !re_get_options(options, &re, NULL) )
    return FALSE;

  /* From here on, all errors must do "rc = xxx; go to out" */
  /* TODO: conditionally allocate match_data on the stack? (As in the PCRE1 code) */
  match_data = pcre2_match_data_create_from_pattern(re.pcre2_code, NULL);
  /* utf8_seek() returns size_t; pcre2_match() takes int */
  /* re_get_options() ensured that the value isn't greater than INT_MAX */
  utf8_start = utf8_seek(subject.subject, subject.length, re.start_flags.flags);
  if ( utf8_start == (size_t)-1 )
  { rc = out_of_range(re.start_flags.flags);
    goto out;
  }
  { int re_rc = pcre2_match(re.pcre2_code,
			    (PCRE2_SPTR)subject.subject, subject.length,
			    utf8_start, re.match_options_flags.flags,
			    match_data, NULL);  /* TODO: pcre2_match_context instead of NULL */

    if ( re_rc > 0 )			/* match */
    { if ( result )
	rc = unify_match(result, &re, &subject, re_rc,
			 pcre2_get_ovector_pointer(match_data));
      else
	rc = TRUE;
    } else
    { rc = re_error(re_rc);
    }
  }

out:
  pcre2_match_data_free(match_data);
  return rc;
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
{ /* TODO: combine code with re_matchsub_() */
  re_data re;
  re_subject subject;
  int rc; /* Every path (to label out) must set rc */
  pcre2_match_data *match_data;
  size_t utf8_start;
  predicate_t pred = PL_predicate("re_call_folder", 4, "pcre");
  term_t av = PL_new_term_refs(4);
  init_subject(&subject);

  if ( !PL_put_term(av+0, closure) )
    return FALSE;
		 /* av+1 is match */
  if ( !PL_put_term(av+2, v0) )
    return FALSE;
		 /* av+3 = v1 */

  if ( !get_re_copy(regex, &re) )
    return FALSE;
  if ( !re_get_subject(on, &subject, BUF_STACK) ) /* Different from re_matchsub_() */
    return FALSE;
  if ( !re_get_options(options, &re, NULL) )
    return FALSE;

  /* From here on, all errors must do "rc = xxx; go to out" */
  match_data = pcre2_match_data_create_from_pattern(re.pcre2_code, NULL);
  /* utf8_seek() returns size_t; pcre2_match() takes int */
  /* re_get_options() ensured that the value isn't greater than INT_MAX */
  utf8_start = utf8_seek(subject.subject, subject.length, re.start_flags.flags);
  if ( utf8_start == (size_t)-1 )
  { rc = out_of_range(re.start_flags.flags);
    goto out;
  }

  for(;;)
  { int re_rc = pcre2_match(re.pcre2_code,
			    (PCRE2_SPTR)subject.subject, subject.length,
			    utf8_start, re.match_options_flags.flags,
			    match_data, NULL);  /* TODO: pcre2_match_context instead of NULL */
    if ( re_rc > 0 )
    { const PCRE2_SIZE *ovector = pcre2_get_ovector_pointer(match_data);
      PL_put_variable(av+1);
      if ( !unify_match(av+1, &re, &subject, re_rc, ovector) ||
	   !PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, av) ||
	   !PL_put_term(av+2, av+3) ||
	   !PL_put_variable(av+3) )
      { rc = FALSE;
	goto out;
      }
      if ( ovector[1] == utf8_start )
	utf8_start++;
      else
	utf8_start = ovector[1];
    } else if ( re_rc == PCRE2_ERROR_NOMATCH ) /* TODO: also PCRE2_ERROR_PARTIAL? */
    { rc = PL_unify(av+2, v);
      break;
    } else
    { rc = re_error(re_rc);
      break;
    }
  }

 out:
  pcre2_match_data_free(match_data);
  return rc;
}


install_t
install_pcre4pl(void)
{ FUNCTOR_pair2 = PL_new_functor(PL_new_atom("-"), 2);

  PL_register_foreign("re_config_",   1, re_config_,   0);
  PL_register_foreign("re_compile",   3, re_compile_,  0);
  PL_register_foreign("re_match_",    3, re_match_,    0);
  PL_register_foreign("re_matchsub_", 4, re_matchsub_, 0);
  PL_register_foreign("re_foldl_",    6, re_foldl_,    0);
  PL_register_foreign("re_config_choice", 1, re_config_choice_, PL_FA_NONDETERMINISTIC);
  /* The following two are used by test_pcre.pl but are not exported
     from pcre.pl, so are used with the pcre: module prefix: */
  PL_register_foreign("re_portray",   2, re_portray_,  0);
  PL_register_foreign("re_portray_match_options", 2, re_portray_match_options_, 0);
}

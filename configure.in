dnl Process this file with autoconf to produce a configure script.

AC_INIT(install-sh)
AC_PREREQ([2.50])
AC_CONFIG_HEADER(config.h)

AC_SUBST(PCRE_TARGETS)
AC_SUBST(PCRE_PLTARGETS)

PCRE_TARGETS=nolib
PCRE_PLTARGETS=

m4_include([../ac_swi_c.m4])

case "$PLARCH" in
    *darwin*)
	if test -z "$PCREPREFIX"; then
          if test -f /opt/local/lib/libpcre.$SO; then
            PCREPREFIX=/opt/local
          else
            PCREPREFIX=/usr/local
          fi
        fi
	;;
esac

if test ! -z "$PCREPREFIX"; then
  AC_MSG_CHECKING(Whether setting for PCREPREFIX is ok)
  if test -f "$PCREPREFIX/include/pcre.h" -a \
          -f "$PCREPREFIX/lib/libpcre.$SO"; then
    AC_MSG_RESULT(Using $PCREPREFIX)
    LIBS="$LIBS $PCREPREFIX/lib/libpcre.$SO"
    CFLAGS="-I/$PCREPREFIX/include $CFLAGS"
    AC_CHECK_FUNCS(pcre_compile)
    PCRELIB=yes
  else
    AC_MSG_RESULT(no)
    PCREPREFIX=
  fi
fi

if test -z "$PCREPREFIX"; then
  AC_CHECK_HEADER(pcre.h,
		  [ AC_DEFINE([HAVE_PCRE_H], 1,
			      [Define to 1 if you have <pcre.h>.])
		    PCREH=yes
		  ],
		  PCREH=no)

  if test "$PCREH" = yes; then
    AC_CHECK_LIB(pcre, pcre_compile,
		 LIBS="$LIBS -lpcre"
		 PCRELIB=yes,
		 PCRELIB=no)
  fi
fi

if test "$PCRELIB" = yes; then
  PCRE_TARGETS="pcre4pl.$SO"
  PCRE_PLTARGETS=pcre.pl
fi

AC_OUTPUT(Makefile)

# SWI-Prolog interface to Perl Regular Expressions

This repository provides the SWI-Prolog package `pcre`, binding to
[pcre](http://www.pcre.org/). The Prolog library provides matching,
replacement, splitting and running arbitrary goals over matches using a
_fold_ style interface.

The `pcre` library is built as part of the standard build for SWI-Prolog,
so it should not be necessary to install this package manually. If the
`pcre` is failing to load, make sure that you have the PCRE library 
installed for your platform (see the 
[platform-specific build instructions](https://www.swi-prolog.org/build/)
for more information about PCRE and other prerequisites).

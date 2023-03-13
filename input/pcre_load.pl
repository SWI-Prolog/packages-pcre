/*  Part of SWI-Prolog

    Author:        Peter Ludeman
    E-mail:        peter.ludemann@gmail.com
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- module(test_pcre_load, [match_date/2, date_re/1]).

/** <module> Test input for save/load of regular expressions
*/

:- use_module(library(pcre)).
:- use_module(library(debug), [assertion/1]).

goal_expansion(re_compile(Pattern, Re, Options), true) :-
    re_compile(Pattern, Re, Options).

match_date(DateStr, Sub) :-
    date_re(Re),
    assertion(blob(Re, regex)),
    re_matchsub(Re, DateStr, Sub).

date_re(Re) :-
    re_compile("(?<date> (?<year>(?:\\d\\d)?\\d\\d) -
		(?<month>\\d\\d) - (?<day>\\d\\d) )", Re,
	       [extended(true)]).

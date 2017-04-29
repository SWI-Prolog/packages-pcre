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

:- module(test_pcre,
          [ test_pcre/0
          ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(pcre).

test_pcre :-
    run_tests([ pcre
              ]).

:- begin_tests(pcre).

test(match1, Sub == re_match{0:"aap"}) :-
    re_compile("a+p", Re, []),
    re_matchsub(Re, "aapenootjes", Sub, []).
test(start, Sub == re_match{0:"es"}) :-
    re_compile("e.", Re, []),
    re_matchsub(Re, "aapenootjes", Sub, [start(4)]).
test(fold, Words == ["aap", "noot", "mies"]) :-
    re_foldl(add_match, "[a-z]+", "aap noot mies", Words, [], []).
test(named, Sub == re_match{0:"2017-04-20",
                            date:"2017-04-20",
                            day:"20",month:"04",year:"2017"}) :-
    re_compile("(?<date> (?<year>(?:\\d\\d)?\\d\\d) -
                (?<month>\\d\\d) - (?<day>\\d\\d) )", Re,
               [extended]),
    re_matchsub(Re, "2017-04-20", Sub, []).
test(typed, Sub == re_match{0:"2017-04-20",
                            date:"2017-04-20",
                            day:20,month:4,year:2017}) :-
    re_matchsub("(?<date> (?<year_I>(?:\\d\\d)?\\d\\d) -
                 (?<month_I>\\d\\d) - (?<day_I>\\d\\d) )"/x,
                "2017-04-20", Sub, []).
test(range, Sub == re_match{0:"Name: value", value:6-5}) :-
    re_matchsub(".*:\\s(?<value_R>.*)"/x, "Name: value", Sub, []).
test(split, Split == ["","a","b","aa","c"]) :-
    re_split("a+", "abaac", Split, []).
test(replace, NewString == "Abaac") :-
    re_replace("a+", "A", "abaac", NewString).
test(replace, NewString == "A1ba2a3c") :-
    re_replace("a(\\d)", "A\\1", "a1ba2a3c", NewString).
test(replace_all, NewString == "AbAc") :-
    re_replace("a+"/g, "A", "abaac", NewString).
test(replace_all, NewString == "A1bA2A3c") :-
    re_replace("a(\\d)"/g, "A\\1", "a1ba2a3c", NewString).
test(replace_none, NewString == "a1ba2a3c") :-
    re_replace("x(\\d)"/g, "A\\1", "a1ba2a3c", NewString).

:- end_tests(pcre).

add_match(Dict, [Dict.0|List], List).

/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Peter Ludemann
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2022, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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

:- encoding(utf8).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).

:- use_module(library(plunit)).
:- use_module(library(pcre)).
:- use_module(library(error)).
:- use_module(library(debug), [assertion/1]).

/* Testing notes.

   Different versions of PCRE2 can cause problems.
   Following are some results from running pcre2_mingw.c
       https://gist.github.com/kamahen/9f54a160773125f382c81d2b5470a872
   gcc pcre2_mingw.c -lpcre2-8 && ./a.out

   (Ubuntu 20.04.1 with apt libpcre2-dev)
     VERSION (len=17 rc=17) 0x0000000b: '10.34 2019-11-21'
     JITTARGET (len=38 rc=38) 0x00000002: 'x86 64bit (little endian + unaligned)'
     UNICODE_VERSION (len=7 rc=7) 0x0000000a: '12.1.0'
     JIT (len=4 rc=0) 0x00000001: 0x00000001

   (MacOS M1, MinGW)
     VERSION (len=17 rc=17) 0x0000000b: '10.39 2021-10-29'
     FAILED: JITTARGET len=-34 rc=-34
     UNICODE_VERSION (len=7 rc=7) 0x0000000a: '14.0.0'
     JIT (len=4 rc=0) 0x00000001: 0x00000000
*/

test_pcre :-
    run_tests([ pcre
	      ]).

% TODO: make this part of plunit?
%       see https://swi-prolog.discourse.group/t/plunit-and-individual-test-setup-cleanup/4853
:- dynamic seen_re_test/1.
:- retractall(seen_re_test(_)).

term_expansion((re_test(Name) :- Body),
                  (test(Name, Options2) :- Body)) :-
    ( seen_re_test(Name) -> throw(error(dup_re_test(Name), _)) ; true ),
    assertz(seen_re_test(Name)),
    expand_re_test_options([], Options2).
term_expansion((re_test(Name, Options) :- Body),
                  (test(Name, Options2) :- Body)) :-
    ( seen_re_test(Name) -> throw(error(dup_re_test(Name), _)) ; true ),
    assertz(seen_re_test(Name)),
    expand_re_test_options(Options, Options2).
:- det(expand_re_test_options/2).
expand_re_test_options([], Options2) =>
    Options2 = [setup(re_flush)].
expand_re_test_options([O1|O2], Options2) =>
    % assertion(\+ memberchk(setup(_), [O1|O2])) doesn't work as expected
    (   memberchk(setup(_), [O1|O2])
    ->  throw(error(setup_conflict([O1|O2]), _))
    ;   true
    ),
    Options2 = [setup(re_flush),O1|O2].
expand_re_test_options(Option, Options2) => % e.g.: re_test(t123, X==[1,2,3]) :- pred(123, X).
    expand_re_test_options([Option], Options2).

:- begin_tests(pcre, [cleanup(test_report(fixme)), setup(re_flush)]).

re_test(match1) :-
    re_match("a+p", "xxxaapenootjes", []).
re_test(match2, fail) :-
    re_match("a+p", "xxxaapenootjes", [anchored(true)]).
re_test(match3) :-
    re_match("a+p"/i, "xxxAAAPnootjes", []),
    % Check that "a+p"/i is cached with its options:
    assertion(\+ re_match("a+p", "xxxAAAPnootjes", [])).
re_test(match4) :-
    re_match("a+p", "xxxAAAPnootjes", [caseless(true)]).

re_test(matchsub1a, Sub == re_match{0:"aap"}) :-
    re_compile("a+p", Re, []),
    re_matchsub(Re, "aapenootjes", Sub, []).
re_test(matchsub1b, Sub == re_match{0:'aap'}) :-
    re_compile("a+p", Re, [capture_type(atom)]),
    re_matchsub(Re, "aapenootjes", Sub, []).
re_test(matchsub2, Sub == re_match{1:"aap", 2:"aaaaaaap", 0:"aapenootjes  aaaaaaap"}) :-
    re_matchsub("(a+?p).*?(a+?p)", "meer aapenootjes  aaaaaaapenootjes", Sub, []).
re_test(matchsub2b, fail) :-
    re_compile("a+p", Re, [anchored(true)]),
    re_matchsub(Re, "---aapenootjes", _Sub, []).
re_test(matchsub3, Sub == re_match{0:"AAP"}) :-
    re_compile("a+p", Re, [anchored(false), caseless(true)]),
    re_matchsub(Re, "---AAPenootjes", Sub, []).
re_test(matchsub4, Sub == re_match{0:"AAP"}) :-
    re_matchsub("a+p"/i, "---AAPenootjes", Sub),
    % Check that "a+p"/i is cached with its options:
    assertion(\+ re_matchsub("a+p", "---AAPenootjes", _)).
re_test(matchsub5, Sub == re_match{0:'AAP'}) :-
    re_matchsub("a+p"/ia, "---AAPenootjes", Sub).

re_test(anchored1, Sub == re_match{0:"aaa"}) :-
    re_matchsub("a+", "aaabc", Sub, [anchored(true)]).
re_test(anchored2, fail) :-
    re_matchsub("a+", "xaaabc", _Sub, [anchored(true)]).
re_test(anchored3, Sub == re_match{0:"aaa"}) :-
    re_matchsub("a+", "xaaabc", Sub, [anchored(false)]).
re_test(anchored4, Sub == re_match{0:"aaa"}) :-
    re_compile("a+", Re, [anchored(false)]),
    re_matchsub(Re, "xaaabc", Sub, []).
re_test(anchored5, fail) :-
    re_compile("a+", Re, [anchored(true)]),
    re_matchsub(Re, "xaaabc", _Sub, []).
re_test(anchored6, fail) :-
    re_compile("a+", Re, []),
    re_matchsub(Re, "xaaabc", _Sub, [anchored(true)]).
re_test(anchored7, Sub == re_match{0:"aaa"}) :-
    re_compile("a+", Re, []),
    re_matchsub(Re, "xaaabc", Sub, [anchored(false)]).

re_test(compile_option1, error(type_error(option,compat(javascript)),_)) :-
    re_compile("a+b", _Re, [compat(javascript)]).
re_test(compile_option2, error(type_error(option,compat(qqsv)),_)) :-
    re_compile("a+b", _Re, [compat(qqsv)]).

% compile_memoryleak tests that a memory leak doesn't happen because
% the blob unification failed.
re_test(compile_memory_double_free, fail) :-
    re_compile(".", foo, []).

re_test(start, Sub == re_match{0:"es"}) :-
    re_compile("e.", Re, []),
    re_matchsub(Re, "aapenootjes", Sub, [start(4)]).

re_test(fold1, Words == ["aap", "noot", "mies"]) :-
    re_foldl(add_match, "[a-z]+", "aap noot mies", Words, [], []).
re_test(fold2, Words == [re_match{0:"aap"},re_match{0:"noot"},re_match{0:"mies"}]) :-
    re_foldl(add_match2, "[a-z]+", "  aap    noot mies ", Words, [], []).
re_test(fold3, Count == 2) :- % re_match_count/3 example from documentation
    Regex = "a",
    String = "aap",
    re_foldl(increment, Regex, String, 0, Count, []).

re_test(fold4a, Letters == ["a", "b", "c"]) :-
    re_foldl(add_match, ".", "abc", Letters, [], []).

re_test(fold4b, Letters == ['網','目','錦','へ','び',' ','[','à','m','í','m','é',' ','n','í','s','h','í','k','í','h','é','ꜜ','b','ì',']']) :-
    re_foldl(add_match, ".", "網目錦へび [àmímé níshíkíhéꜜbì]", Letters, [], [capture_type(atom)]).

% TODO: change following to atoms once Issue #14 is fixed.
re_test(fold4c, Letters == ["網","目","錦","へ","び"," ","[","à","m","í","m","é"," ","n","í","s","h","í","k","í","h","é","ꜜ","b","ì","]"]) :-
    re_split(".", "網目錦へび [àmímé níshíkíhéꜜbì]", Split, []),
    post_split_dot(Split, Letters).

re_test(named, Sub == re_match{0:"2017-04-20",
                               date:"2017-04-20",
                               day:"20",month:"04",year:"2017"}) :-
    re_compile("(?<date> (?<year>(?:\\d\\d)?\\d\\d) -
		(?<month>\\d\\d) - (?<day>\\d\\d) )", Re,
	       [extended(true)]),
    re_matchsub(Re, "2017-04-20", Sub, []),
    re_portray_string(Re, RegexStr),
    assertion(RegexStr == "<regex>(/(?<date> (?<year>(?:\\d\\d)?\\d\\d) -\n\t\t(?<month>\\d\\d) - (?<day>\\d\\d) )/ [EXTENDED BSR_UNICODE CAP_STRING] $capture=4 {0:CAP_DEFAULT 1:date:CAP_DEFAULT 2:year:CAP_DEFAULT 3:month:CAP_DEFAULT 4:day:CAP_DEFAULT})").
re_test(typed1, Sub == re_match{0:"2017-04-20",
				date:"2017-04-20",
				day:20,month:4,year:2017}) :-
    re_matchsub("(?<date> (?<year_I>(?:\\d\\d)?\\d\\d) -
		 (?<month_I>\\d\\d) - (?<day_I>\\d\\d) )"/x,
		"2017-04-20", Sub, []).
re_test(typed2, Sub == re_match{0:"2017-04-20",
				date:"2017-04-20",
				day:20,month_:4,year_x:2017}) :-
    % Names with more than one "_", for testing type suffix
    re_matchsub("(?<date> (?<year_x_I>(?:\\d\\d)?\\d\\d) -
		 (?<month__I>\\d\\d) - (?<day_I>\\d\\d) )"/x,
		"2017-04-20", Sub, []).
re_test(typed3a, Sub == re_match{0:'2017-04-20',
				 date:'2017-04-20',
				 day:20,month_:4,year_x:"2017"}) :-
    % Names with more than one "_", for testing type suffix
    re_matchsub("(?<date> (?<year_x_S>(?:\\d\\d)?\\d\\d) -
		 (?<month__I>\\d\\d) - (?<day_I>\\d\\d) )",
		"2017-04-20", Sub, [extended(true), capture_type(atom)]).
re_test(typed3b, Sub == re_match{0:'2017-04-20',
				 date:'2017-04-20',
				 day:20,month_:4,year_x:"2017"}) :-
    % Names with more than one "_", for testing type suffix
    re_compile("(?<date> (?<year_x_S>(?:\\d\\d)?\\d\\d) -
		 (?<month__I>\\d\\d) - (?<day_I>\\d\\d) )",
		Re, [extended(true), capture_type(atom)]),
    re_matchsub(Re, "2017-04-20", Sub, []).
re_test(typed3c, Sub == re_match{0:"2017-04-20",
				 date:"2017-04-20",
				 day:20,month_:4,year_x:"2017"}) :-
    % Names with more than one "_", for testing type suffix
    re_compile("(?<date> (?<year_x_S>(?:\\d\\d)?\\d\\d) -
		 (?<month__I>\\d\\d) - (?<day_I>\\d\\d) )",
		Re, [extended(true)]),
    re_matchsub(Re, "2017-04-20", Sub, []).
re_test(range, Sub == re_match{0:"Name: value", value:6-5}) :-
    re_matchsub(".*:\\s(?<value_R>.*)"/x, "Name: value", Sub, []).
re_test(capture_string1a, Subs == re_match{0:"abc", 1:"a", 2:"b", 3:"c"}) :-
    re_matchsub('(a)(b)(c)', 'xabc', Subs, [capture_type(string)]).
re_test(capture_string1b, Subs == re_match{0:"abc", 1:"a", 2:"b", 3:"c"}) :-
    re_compile('(a)(b)(c)', Re, [capture_type(string)]),
    re_matchsub(Re, 'xabc', Subs, []).
re_test(capture_atom1a, Subs == re_match{0:'abc', 1:'a', 2:'b', 3:'c'}) :-
    re_matchsub('(a)(b)(c)', 'xabc', Subs, [capture_type(atom)]).
re_test(capture_atom1b, Subs == re_match{0:'abc', 1:'a', 2:'b', 3:'c'}) :-
    re_compile('(a)(b)(c)', Re, [capture_type(atom)]),
    re_matchsub(Re, 'xabc', Subs, [capture_type(atom)]).
re_test(capture_atom1c, Subs == re_match{0:'abc', 1:'a', 2:'b', 3:'c'}) :-
    re_compile('(a)(b)(c)', Re, [capture_type(atom)]),
    re_matchsub(Re, 'xabc', Subs, []).
re_test(capture_range1, Subs == re_match{0:1-3, 1:1-1, 2:2-1, 3:3-1}) :-
    re_matchsub('(a+)(b+)(c+)', 'xabc', Subs, [capture_type(range)]).
re_test(capture_range2, Subs == re_match{0:1-3, 1:1-1, 2:2-1, 3:3-1}) :-
    re_compile('(a+)(b+)(c+)', Re, [capture_type(range)]),
    re_matchsub(Re, 'xabc', Subs, []).
re_test(capture_range3, Subs == re_match{0:1-3, 1:1-1, 2:2-1, 3:3-1}) :-
    re_matchsub('(a+)(b+)(c+)'/r, 'xabc', Subs).
re_test(capture_atom2, Subs == re_match{0:'Name: value', value:'value'}) :-
    re_matchsub(".*:\\s(?<value>.*)", "Name: value", Subs, [extended(true), capture_type(atom)]).

re_test(split_1, Split == ["","a","b","aa","c"]) :-
    re_split("a+", "abaac", Split).
re_test(split_2, Split == ['','a','b','aa','c']) :-
    re_split("a+"/a, "abaac", Split).
re_test(split_3, Split == ['','a','b','aa','c']) :-
    re_split("a+", "abaac", Split, [capture_type(atom)]).
re_test(split_4a, Letters = ["", "a", "", "b", "", "c", ""]) :-
    re_split(".", "abc", Letters, []).
re_test(split_4b, Letters = ['', 'a', '', 'b', '', 'c', '']) :-
    re_split('.', 'abc', Letters, [capture_type(atom)]).

re_test(replace1, NewString == "Abaac") :-
    re_replace("a+", "A", "abaac", NewString).
re_test(replace2a, NewString == "A[1]ba2a3c") :-
    re_replace("a(\\d)", "A[\\1]", "a1ba2a3c", NewString).
re_test(replace2b, NewString == "A[1]ba2a3c") :-
    re_replace("a(\\d)", "A[$1]", "a1ba2a3c", NewString).
re_test(replace2c, NewString == "A[1]ba2a3c") :-
    re_replace("a(\\d)", "A[${1}]", "a1ba2a3c", NewString).
re_test(replace2d, NewString == "A[1]ba2a3c") :-
    re_replace("a(\\d)", "A[\\{1}]", "a1ba2a3c", NewString).
re_test(replace_all1, NewString == "AbAc") :-
    re_replace("a+"/g, "A", "abaac", NewString).
re_test(replace_all2a, NewString == 'XbXc') :-
    re_replace("a+"/gia, "X", "AbaAc", NewString).
re_test(replace_all2b, NewString == 'XbXc') :-
    re_replace("a+"/ga, "X", "AbaAc", NewString, [caseless(true)]).
re_test(replace_all2c, NewString == 'XbXc') :-
    re_replace("a+"/g, "X", "AbaAc", NewString, [caseless(true), capture_type(atom)]).
re_test(replace_all3, NewString == "A[1]bA[2]A[3]c") :-
    re_replace("a(\\d)"/g, "A[\\1]", "a1ba2a3c", NewString).
re_test(replace_none, NewString == "A[1]bA[2]A[3]c") :-
    re_replace("a(\\d)"/g, "A[\\1]", "a1ba2a3c", NewString).
re_test(replace_capture_type_error1, NewString == "A[1]bA[2]A[3]c") :- % capture_type ignored
    re_replace("a(\\d)"/g, "A[\\1]", "a1ba2a3c", NewString, [capture_type(range)]).
re_test(replace_capture_type_error2, NewString == "A[1]bA[2]A[3]c") :- % capture_type ignored
    re_replace("a(\\d)"/g, "A[\\1]", "a1ba2a3c", NewString, [capture_type(term)]).
re_test(replace_capture_type_precedence1, NewString == "A[1]bA[2]A[3]c") :-
    re_replace("a(\\d)"/gs, "A[\\1]", "a1ba2a3c", NewString, [capture_type(atom)]).
re_test(replace_capture_type_precedence2, NewString == 'A[1]bA[2]A[3]c') :-
    re_replace("a(\\d)"/ga, "A[\\1]", "a1ba2a3c", NewString, [capture_type(string)]).
re_test(replace_capture_type_precedence3, NewString == 'A[1]bA[2]A[3]c') :-
    re_replace("a(\\d)"/gas, "A[\\1]", "a1ba2a3c", NewString, [capture_type(string)]).

re_test(replace_unicode1,
     [condition(re_config(unicode(true))),
      true(NewString == "網目錦蛇 [reticulated python へび]")]) :-
    re_replace('àmímé níshíkíhéꜜbì', "reticulated python へび",
	       '網目錦蛇 [àmímé níshíkíhéꜜbì]', NewString).
re_test(replace_unicode2,
     [condition(re_config(unicode(true))),
      true(NewString == "網目錦へび [àmímé níshíkíhéꜜbì]")]) :-
    re_replace('(a蛇é)+', "へび",
	       '網目錦a蛇éa蛇éa蛇éa蛇é [àmímé níshíkíhéꜜbì]', NewString).
re_test(replace_unicode3,
     [condition(re_config(unicode(true))),
      true(NewString == "網目へび [àmímé níshíkíhéꜜbì]")]) :-
    re_replace("[蛇錦]+", "へび",
	       "網目錦蛇 [àmímé níshíkíhéꜜbì]", NewString).
re_test(replace_name1a, NewString == "[a][b][c]") :-
    re_replace("(?<any>.)"/g, "[$any]", "abc", NewString).
re_test(replace_name1b, NewString == "[a][b][c]") :-
    re_replace("(?<any_A>.)"/g, "[\\any]", "abc", NewString).
re_test(replace_name1c, NewString == "[a][b][c]") :-
    re_replace("(?<any>.)"/g, "[${any}]", "abc", NewString).
re_test(replace_name1d, NewString == "[a][b][c]") :-
    re_replace("(?<any>.)"/g, "[\\{any}]", "abc", NewString).
re_test(replace_name1e, error(existence_error(re_type_flag, 'X'), _)) :-
    re_replace("(?<any_X>.)"/g, "[\\{any}]", "abc", _NewString).
re_test(replace_name2, error(existence_error(key, bar, re_match{0:0-1, foo:0-1}), _)) :-
    re_replace('(?<foo>.)', "[$bar]", "abc", _NewString).
re_test(replace_date, NewString == "4/20/2017") :-
    re_replace("(?<date> (?<year_I>(?:\\d\\d)?\\d\\d) -
                (?<month_I>\\d\\d) - (?<day_I>\\d\\d) )"/x,
               "$month/$day/$year",
               "2017-04-20",
               NewString).

re_test(config_not_compound1, fail) :- % was: error(type_error(compound,version(A,B)),_)
    re_config(version(_A,_B)).
re_test(config_not_compound2, fail) :- % was: error(type_error(compound,foo(A,B)),_)
    re_config(foo(_A,_B)).
re_test(config_not_compound3, fail) :- % was: error(type_error(compound,bsr),_)
    re_config(bsr).
re_test(config_not_compound4, fail) :- % was: error(type_error(compound,123),_)
    re_config(123).
re_test(config_invalid, fail) :- % was: error(existence_error(re_config,qqsv(V)),_)
    re_config(qqsv(_V)).
re_test(config_version) :-
    re_config(version(V)),
    must_be(atom, V), % TODO: V is of the form '10.34 2019-11-21'.
    re_config(version(V)). % Check that it takes an argument
re_test(config_version_type, fail) :-
    re_config(version(V)),
    atom_string(V, Vstr),
    re_config(version(Vstr)).
test(config_version_value1, [setup((re_config(version(V)),
			     atomic_concat(V, '---', V2))),
                             fail]) :-
    % Tests that re_config(version(V2)) fails when given an argument
    % that is guaranteed to not be the version (it has an extra '---' on the end)
    re_config(version(V2)).
re_test(config_all1) :-
    forall(re_config(Config),
           assertion(ground(Config))).
re_test(config_all2) :-
    forall(re_config(Config),
           ( re_config(Config),
             assertion(ground(Config)) )).
% The following tests that our documentation matches the code.  The
% validity test uses =/2 because the query instantiates the values.
re_test(config_all3, DocSorted = ConfigsSorted) :-
    Doc0 = [bsr2(_),
           compiled_widths(_),
           depthlimit(_),
           heaplimit(_),
           jit(_),
           linksize(_),
           matchlimit(_),
           never_backslash_c(_),
           newline2(_),
           parenslimit(_),
           stackrecurse(_),
           unicode(_),
           unicode_version(_),
           version(_)
           ],
    (   re_config(jit(true))
    ->  Doc = [jittarget(_)|Doc0]
    ;   Doc = Doc0
    ),
    sort(Doc, DocSorted),
    bagof(C, re_config(C), Configs),
    assertion(ground(Configs)),
    msort(Configs, ConfigsSorted), % note msort/2, in case of dups
    % For more easily finding the first mismatch:
    maplist(assertion_eq, DocSorted, ConfigsSorted).
test(config_version_value2, [setup(re_config(version(V)))]) :-
    re_config(version(V)).
re_test(config_unicode) :-
    re_config(unicode(V)),
    must_be(boolean, V).
re_test(config_unicode_value, [nondet]) :- % For verifying that this works: condition(re_config(unicode(true)))
    (   re_config(unicode(true)) % TODO: shouldn't leave choicepoint
    ;   re_config(unicode(false))
    ).
re_test(config_unicode_properties) :-
    re_config(unicode_properties(V)),
    must_be(boolean, V).
re_test(config_jit) :-
    re_config(jit(V)),
    must_be(boolean, V).
re_test(config_jittarget, [condition(re_config(jit(true)))]) :-
    re_config(jittarget(V)), % was: error(existence_error(re_config, jittarget(V)),_)
    must_be(atom, V).
re_test(config_jittarge2, [condition(re_config(jit(false))), fail]) :-
    re_config(jittarget(_V)).
re_test(config_newline, fail) :- % was: error(existence_error(re_config,newline(V)),_)
    re_config(newline(_V)). % newline in PCRE1 becomes newline2 in PCRE2
re_test(config_newline2) :-
    re_config(newline2(V)),
    assertion(memberchk(V, [cr,lf,crlf,any,anycrlf,nul])).
re_test(config_bsr, fail) :- % was: error(existence_error(re_config,bsr(V)),_)
    re_config(bsr(_V)). % bsr in PCRE1 becomes bsr2 in PCRE2
re_test(config_bsr2) :-
    re_config(bsr2(V)),
    assertion(memberchk(V, [unicode,anycrlf])).
re_test(config_link_size) :-
    re_config(link_size(V)),
    must_be(integer, V).
re_test(config_posix_malloc_threshold, fail) :- % was: error(existence_error(re_config,posix_malloc_threshold(V)),_)
    re_config(posix_malloc_threshold(V)),
    must_be(integer, V).
re_test(config_parens_limit) :-
    re_config(parens_limit(V)),
    must_be(integer, V).
re_test(config_match_limit) :-
    re_config(match_limit(V)),
    must_be(integer, V).
re_test(config_match_limit_recursion, fail) :- % was: error(existence_error(re_config,posix_malloc_threshold(V)),_)
    re_config(match_limit_recursion(V)),
    must_be(integer, V).
re_test(config_stackrecurse) :-
    re_config(stackrecurse(V)),
    must_be(boolean, V).
test(config_linksize) :-
    re_config(linksize(V)),
    must_be(integer, V),
    assertion(memberchk(V, [2,3,4])).
test(config_parenslimit) :-
    re_config(parenslimit(V)),
    must_be(integer, V).
test(config_matchlimit) :-
    re_config(matchlimit(V)),
    must_be(integer, V).
test(config_depthlimit) :-
    re_config(depthlimit(V)),
    must_be(integer, V).
test(config_unicode) :-
    re_config(unicode(V)),
    must_be(boolean, V).
test(config_unicode_version) :-
    re_config(unicode_version(V)),
    must_be(atom, V).
test(config_heaplimit) :-
    re_config(heaplimit(V)),
    must_be(integer, V).
test(config_heaplimit) :-
    re_config(heaplimit(V)),
    must_be(integer, V).
test(config_never_backslash_c) :-
    re_config(never_backslash_c(V)),
    must_be(boolean, V).
test(config_compiled_widths) :-
    re_config(compiled_widths(V)),
    must_be(integer, V).

re_test(compile_portray_0,
        RegexStr == "<regex>(/./ [BSR_UNICODE CAP_STRING] $capture=0)") :-
    re_compile(".", Regex, []),
    re_portray_string(Regex, RegexStr).
re_test(compile_portray_0a,
        RegexStr == "<regex>(/./ [BSR_UNICODE NEWLINE_ANYCRLF CAP_STRING] $capture=0)") :-
    re_compile(".", Regex, [newline(anycrlf)]),
    re_portray_string(Regex, RegexStr).
re_test(compile_portray_1a,
        RegexStr == "<regex>(/(.)/ [BSR_UNICODE CAP_STRING] $capture=1 {0:CAP_DEFAULT 1:CAP_DEFAULT})") :-
    re_compile("(.)", Regex, []),
    re_portray_string(Regex, RegexStr).
re_test(compile_portray_1b,
        RegexStr == "<regex>(/(.)/ [BSR_UNICODE CAP_ATOM] $capture=1 {0:CAP_DEFAULT 1:CAP_DEFAULT})") :-
    re_compile("(.)", Regex, [capture_type(atom)]),
    re_portray_string(Regex, RegexStr).
re_test(compile_portray_2,
        RegexStr == "<regex>(/(?<foo>.)([a-z]*)(?<bar_A>.)/ [BSR_UNICODE CAP_STRING] $capture=3 {0:CAP_DEFAULT 1:foo:CAP_DEFAULT 2:CAP_DEFAULT 3:bar:CAP_ATOM})") :-
    re_compile("(?<foo>.)([a-z]*)(?<bar_A>.)", Regex, []),
    re_portray_string(Regex, RegexStr).

re_test(compile_config_1, true) :-
        % negative: NO_AUTO_CAPTURE NO_AUTO_POSSES NO_DOTSTAR_ANCHOR NO_START_OPTIMIZE UNGREEDY
        % defaulted: NO_UTF_CHECK UTF
        % TODO: missing: JIT_COMPLETE JIT_PARTIAL_SOFT JIT_PARTIAL_HARD JIT_INVALID_UTF
    re_compile('.',  Regex,
	       [ anchored(true), % Also re_match/3
                 endanchored(true),
                 allow_empty_class(true),
                 alt_bsux(true),
                 auto_callout(true),
                 caseless(true),
                 dollar_endonly(true),
                 dotall(true),
                 dupnames(true),
                 extended(true),
                 firstline(true),
                 match_unset_backref(true),
                 multiline(true),
                 % never_utf(true), % TODO: other options: "Syntax error: using UTF is disabled by the application"
                 auto_capture(true),
                 % no_auto_capture(true), % backwards compatibility
                 auto_possess(true),
                 dotstar_anchor(true),
                 start_optimize(true),
                 ucp(true),
                 greedy(true),
                 % ungreedy(true), % Backwards compatibility
                 utf(true),
                 never_backslash_c(true),
                 alt_circumflex(true),
                 alt_verbnames(true),
                 use_offset_limit(true),
                 extended_more(true),
                 % literal(true), % TODO: Causes syntax error with other options "The only other main options that are allowed with PCRE2_LITERAL are: PCRE2_ANCHORED, PCRE2_ENDANCHORED, PCRE2_AUTO_CALLOUT, PCRE2_CASELESS, PCRE2_FIRSTLINE, PCRE2_MATCH_INVALID_UTF, PCRE2_NO_START_OPTIMIZE, PCRE2_NO_UTF_CHECK, PCRE2_UTF, and PCRE2_USE_OFFSET_LIMIT. The extra options PCRE2_EXTRA_MATCH_LINE and PCRE2_EXTRA_MATCH_WORD"
                 match_invalid_utf(true),
                 jit_complete(true),
                 jit_partial_soft(true),
                 jit_partial_hard(true),
                 jit_invalid_utf(true),

                 optimize(true),
                 capture_type(range),
                 bsr(anycrlf),
                 newline(anycrlf)
	       ]),
    re_portray_string(Regex, RegexStr),
    % Older versions of PCRE2 don't have MATCH_INVALID_UTF
    % TODO: wrap the test with re_config(version(V)) to control
    %       which RegexStr should match
    assertion((RegexStr == "<regex>(/./ [compile-ANCHORED compile-ENDANCHORED ALLOW_EMPTY_CLASS ALT_BSUX AUTO_CALLOUT CASELESS DOLLAR_ENDONLY DOTALL DUPNAMES EXTENDED FIRSTLINE MATCH_UNSET_BACKREF MULTILINE UCP NEVER_BACKSLASH_C ALT_CIRCUMFLEX ALT_VERBNAMES USE_OFFSET_LIMIT EXTENDED_MORE MATCH_INVALID_UTF BSR_ANYCRLF NEWLINE_ANYCRLF CAP_RANGE] $capture=0 $optimise)"
	      ;RegexStr == "<regex>(/./ [compile-ANCHORED compile-ENDANCHORED ALLOW_EMPTY_CLASS ALT_BSUX AUTO_CALLOUT CASELESS DOLLAR_ENDONLY DOTALL DUPNAMES EXTENDED FIRSTLINE MATCH_UNSET_BACKREF MULTILINE UCP NEVER_BACKSLASH_C ALT_CIRCUMFLEX ALT_VERBNAMES USE_OFFSET_LIMIT EXTENDED_MORE BSR_ANYCRLF NEWLINE_ANYCRLF CAP_RANGE] $capture=0 $optimise)")).


% Note: Match options are tested in compile_match_1
re_test(compile_config_1_inverse,
        RegexStr == "<regex>(/./ [compile-~UTF NO_AUTO_CAPTURE NO_AUTO_POSSESS NO_DOTSTAR_ANCHOR NO_START_OPTIMIZE UNGREEDY BSR_UNICODE NEWLINE_NUL CAP_RANGE] $capture=0 $optimise)") :-
    re_compile('.', Regex,
	       [ anchored(false), % Also re_match/3
                 endanchored(false),
                 allow_empty_class(false),
                 alt_bsux(false),
                 auto_callout(false),
                 caseless(false),
                 dollar_endonly(false),
                 dotall(false),
                 dupnames(false),
                 extended(false),
                 firstline(false),
                 match_unset_backref(false),
                 multiline(false),
                 never_ucp(false),
                 never_utf(false),
                 auto_capture(false),
                 % no_auto_capture(false), % backwards compatibility
                 auto_possess(false),
                 dotstar_anchor(false),
                 start_optimize(false),
                 ucp(false),
                 greedy(false),
                 % ungreedy(false), % Backwards compatibility
                 utf(false),
                 never_backslash_c(false),
                 alt_circumflex(false),
                 alt_verbnames(false),
                 use_offset_limit(false),
                 extended_more(false),
                 literal(false),
                 match_invalid_utf(false),
                 jit_complete(false),
                 jit_partial_soft(false),
                 jit_partial_hard(false),
                 jit_invalid_utf(false),

                 optimize(true),
                 capture_type(range),
                 bsr(unicode),
                 newline(nul),

                 % Invert them (they'll be ignored):
                 anchored(true), % Also re_match/3
                 endanchored(true),
                 allow_empty_class(true),
                 alt_bsux(true),
                 auto_callout(true),
                 caseless(true),
                 dollar_endonly(true),
                 dotall(true),
                 dupnames(true),
                 extended(true),
                 firstline(true),
                 match_unset_backref(true),
                 multiline(true),
                 never_ucp(true),
                 never_utf(true),
                 auto_capture(true),
                 % no_auto_capture(true), % backwards compatibility
                 auto_possess(true),
                 dotstar_anchor(true),
                 start_optimize(true),
                 ucp(true),
                 greedy(true),
                 % ungreedy(true), % Backwards compatibility
                 utf(true),
                 never_backslash_c(true),
                 alt_circumflex(true),
                 alt_verbnames(true),
                 use_offset_limit(true),
                 extended_more(true),
                 literal(true),
                 match_invalid_utf(true),
                 jit_complete(true),
                 jit_partial_soft(true),
                 jit_partial_hard(true),
                 jit_invalid_utf(true),

                 optimize(true),
                 capture_type(string),
                 bsr(anycrlf),
                 newline(lf)
	       ]),
    re_portray_string(Regex, RegexStr).

re_test(compile_config_2,
     RegexStr == "<regex>(/./ [BSR_UNICODE NEWLINE_NUL CAP_ATOM] $capture=0)") :-
    re_compile('.', Regex, [multiline(false),caseless(false),capture_type(atom),foo,newline(nul),newline2(cr)]),
    re_portray_string(Regex, RegexStr).

re_test(compile_config_3,
        % TODO: if NEWLINE_CRLF is default on Windows, won't display NEWLINE_CRLF
        RegexStr == "<regex>(/./ [CASELESS MULTILINE BSR_UNICODE NEWLINE_CRLF CAP_TERM] $capture=0)") :-
    re_compile('.', Regex, [qqsv,zot(123),optimise(false),capture_type(term),multiline(true),caseless(true),newline(crlf)]),
    re_portray_string(Regex, RegexStr).

re_test(compile_config_4, error(type_error(option, newline(qqsv)), _)) :-
    re_compile('.', _Regex, [newline(qqsv)]).

% TODO: extra_match_line(true) results in compile-ANCHORED with
%       PCRE2_INFO_ALLOPTIONS but not with PCRE2_INFO_ARGOPTIONS (in
%       write_re_options())
%       - is this a bug in pcre2 or expected behavior?

re_test(compile_extra_1, true) :-
    re_compile('.', Regex, [extra_allow_surrogate_escapes,
                            extra_bad_escape_is_literal,
                            extra_match_word,
                            extra_match_line,
                            extra_escaped_cr_is_lf,
                            extra_alt_bsux]),
    re_portray_string(Regex, RegexStr),
    % Older versions of PCRE2 don't have EXTRA_ESCAPED_CR_IS_LF EXTRA_ALT_BSUX
    % TODO: wrap the test with re_config(version(V)) to control
    %       which RegexStr should match
    assertion((RegexStr == "<regex>(/./ [EXTRA_ALLOW_SURROGATE_ESCAPES EXTRA_BAD_ESCAPE_IS_LITERAL EXTRA_MATCH_WORD EXTRA_MATCH_LINE EXTRA_ESCAPED_CR_IS_LF EXTRA_ALT_BSUX BSR_UNICODE CAP_STRING] $capture=0)"
	      ;RegexStr == "<regex>(/./ [EXTRA_ALLOW_SURROGATE_ESCAPES EXTRA_BAD_ESCAPE_IS_LITERAL EXTRA_MATCH_WORD EXTRA_MATCH_LINE BSR_UNICODE CAP_STRING] $capture=0)")).

re_test(compile_jit) :-
    % TODO: also test the options jit_complete, jit_partial_soft, etc. and
    %       needs updates to write_re_options().
    %       Needs to be conditional, according to re_config(jit(true)).
    re_compile('.', _Regex, [optimize(true)]).

re_test(compile_match_1, [Sub, Sub2] == [re_match{0:"a"}, re_match{0:"b"}]) :-
    re_compile('.', Regex, [anchored(true),bol(false),eol(false),empty(false),empty_atstart(false),start(666)]), % start(666) is ignored
    MatchOpts = [jit(false), anchored(false),bol(false),eol(false),empty(false),empty_atstart(false),start(0)], % anchored(false) overrides in re_match()
    re_portray_match_options_string(MatchOpts, MatchOptsStr),
    re_matchsub(Regex, "abc", Sub, MatchOpts),
    assertion(MatchOptsStr == "<no pcre2_code> NOTBOL NOTEOL NOTEMPTY NOTEMPTY_ATSTART NO_JIT $start=0"),
    re_portray_string(Regex, RegexStr),
    % TODO: need to check: the BSR result in the following is the
    %       default (possibly change the portray code).
    %       Verify that: re_config(bsr2(unicode))
    assertion(RegexStr == "<regex>(/./ [compile-ANCHORED BSR_UNICODE CAP_STRING] $capture=0)"),
    re_matchsub(Regex, "abc", Sub2, [start(1)|MatchOpts]).

re_test(compile_match_2,
     MatchOptsStr == "<no pcre2_code> $start=999") :-
    re_portray_match_options_string([anchored(false),bol(true),eol(true),empty(true),empty_atstart(true),start(999)],
					    MatchOptsStr).

re_test(compile_match_3,
        MatchOptionsStr == "<no pcre2_code> $start=0") :-
    % TODO: verify that newline(...) is not a pcre2_match() option
    re_portray_match_options_string([newline(nul)], MatchOptionsStr).

re_test(match_ok_start, Sub==re_match{0:"c"}) :-
    re_matchsub('.', "abc", Sub, [start(2)]).
re_test(match_bad_start1, error(domain_error(offset,3),_)) :-
    re_matchsub('.', "abc", _Sub, [start(3)]).
re_test(match_bad_start2,  error(type_error(option,start=3),_)) :-
    re_matchsub('.', "abc", _Sub, [start=3]).
re_test(match_bad_start3, error(domain_error(offset,0x80000000),_)) :-
    re_matchsub('.', "abc", _Sub, [start(0x80000000)]).

re_test(replace_bad_ref_1, error(existence_error(key,1,re_match{0:0-1}),_)) :-
    re_replace(".", "$1", "abc", _).
re_test(replace_bad_ref_2, error(existence_error(key,1,re_match{0:0-1,foo:0-1}),_)) :-
    re_replace("(?<foo>.)", "$1", "abc", _).
re_test(replace_bad_ref_3, error(existence_error(key,foob,re_match{0:0-1,foo:0-1}),_)) :-
    re_replace("(?<foo>.)", "${foob}", "abc", _).
% replace_bad_ref_4 is also a memory leak test - it causes the call to
% init_capture_map() to fail, but some fields in the blob have already
% been allocated.
re_test(replace_bad_ref_4, error(existence_error(re_type_flag, 'X'), _)) :-
    re_replace("(?<foo_X>.)", "${foo}", "xabc", _).
re_test(replace_ok_ref_4a, Result == "xabc") :-
    re_replace("(?<foo_A>.)", "${foo}", "xabc", Result).
re_test(replace_ok_ref_4b, Result == "123bc") :-
    re_replace("(?<foo_I>\\d+)", "${foo}", "00123bc", Result).

re_test(replace_escape_dollar1a, Result == "$bc") :-
    re_replace(".", "$$", "abc", Result).
re_test(replace_escape_dollar1b, Result == '$bc') :-
    re_replace(".", "$$", "abc", Result, [capture_type(atom)]).
re_test(replace_escape_dollar1c, Result == '$bc') :-
    re_replace("."/a, "$$", "abc", Result).
re_test(replace_escape_dollar2, Result == "$bbc") :-
    re_replace(".(.)", "$$$1$1", "abc", Result).
re_test(replace_escape_dollar3, Result == "x$y$zb$bc") :-
    re_replace(".(.)", "x$$y$$z$1$$$1", "abc", Result).
% Doubled "\"s because of SWI-Prolog string escaping rules:
re_test(replace_escape_backslash1, Result == "\\bc") :-
    re_replace(".", "\\\\", "abc", Result).
re_test(replace_escape_backslash2, Result == "\\bbc") :-
    re_replace(".(.)", "\\\\\\1\\1", "abc", Result).
re_test(replace_escape_backslash3, Result == "x\\y\\zb\\bc") :-
    re_replace(".(.)", "x\\\\y\\\\z\\1\\\\\\1", "abc", Result).

re_test(cached_compile_1a) :-
    re_compile('b', Re1, [caseless(true)]),
    re_compile('b', Re2, [caseless(false)]),
    assertion(   re_match(Re1, "ABC")),
    assertion(\+ re_match(Re2, "ABC")).
re_test(cached_compile_1b) :- % as cached_compile_1a but using text instead of Regex
    assertion(   re_match('b', "ABC", [caseless(true)])),
    assertion(\+ re_match('b', "ABC", [caseless(false)])).
re_test(cached_compile_1bx ) :- % as cached_compile_1b but without the assertions.
                 re_match('b', "ABC", [caseless(true)]),
              \+ re_match('b', "ABC", [caseless(false)]).
re_test(cached_compile_1c) :- % as cached_compile_1b but ensure no caching
    re_flush,
    assertion(   re_match('b', "ABC", [caseless(true)])),
    re_flush,
    assertion(\+ re_match('b', "ABC", [caseless(false)])).

re_test(wb_1, NewString == "carted") :-
    re_replace("^(.*?)d(.*)$", "$1c$2", "darted", NewString).

% Test from Wouter Beek (https://github.com/SWI-Prolog/packages-pcre/issues/5#issuecomment-1019583301)
% Cannot be specified in the SWI library
% <https://github.com/SWI-Prolog/packages-pcre/issues/5>.
% https://www.w3.org/TR/xpath-functions/#func-replace
% This WILL NOT be fixed -- unless PCRE2 changes its compilation of this kind of regexp
re_test(wb_2, blocked(javascript_compat)) :-
    re_replace("(ab)|(a)", "[1=$1][2=$2]", "abcd", Result, [compat(javascript)]),
    assertion(Result == "[1=ab][2=]cd").

% Fix for https://github.com/SWI-Prolog/packages-pcre/issues/6
re_test(wb_3, Result == "abbraccaddabbra") :-
    re_replace("a(.)"/g, "a$1$1", "abracadabra", Result).

% TODO: similar tests for all flags - better than checking whether the
%       flags have been set (using re_portray_string/2).
re_test(greedy_false, Result == re_match{0:"{START} Mary {END}", 1:" Mary "}) :-
    re_matchsub("{START}(.*){END}", "{START} Mary {END} had a {START} little lamb {END}", Result, [greedy(false)]).

re_test(greedy_true, Result == re_match{0:"{START} Mary {END} had a {START} little lamb {END}"}, 1:" Mary {END} had a {START} little lamb ") :-
    re_matchsub("{START}(.*){END}", "{START} Mary {END} had a {START} little lamb {END}", Result, [greedy(true)]).

re_test(greedy_default, Result == re_match{0:"{START} Mary {END} had a {START} little lamb {END}", 1:" Mary {END} had a {START} little lamb "}) :-
    re_matchsub("{START}(.*){END}", "{START} Mary {END} had a {START} little lamb {END}", Result , []).

re_test(foldl_notgreedy_example, Matches == [8-"Mary", 33-"little lamb"]) :-
    String = "{START} Mary {END} had a {START} little lamb {END}",
    re_foldl(range_match,
             "{START} *?(?<piece>.*) *?{END}",
             String, _{string:String,index:piece}-Matches, _-[],
             [capture_type(range),greedy(false)]).

% compare* regression tests - there was a long-standing typo in the
% code that wasn't caught because there was no test.
re_test(compare1) :-
    re_compile('a', Re1, []),
    re_compile('a', Re2, []),
    with_output_to(string(Re1Str), write(current_output, Re1)),
    with_output_to(string(Re2Str), write(current_output, Re2)),
    compare(Comp1, Re1Str, Re2Str),
    compare(Comp2, Re1, Re2),    % Same regexp, different pointers
    assertion(Comp1 == Comp2),
    assertion(memberchk(Comp1, [(<), (>)])).
re_test(compare2) :-
    % It would be nice to check that ReB has a pointer that's < ReA's
    % pointer, but that isn't guaranteed. If it were, we could compare
    % the "write" strings as an additional check that the pattern
    % strings are compared before the pointers (which is how the code
    % in compare_pcres() is written).
    re_compile('b', ReB, []),
    re_compile('a', ReA, []),
    compare(CompareBlob, ReB, ReA),
    assertion(CompareBlob == (>)). % internal knowledge: pattern strings are compared: b > a
re_test(compare3) :-
    re_compile('a', Re1, []),
    compare(Comp1, Re1, Re1),
    assertion(Comp1 == (=)).

re_test(write, Result == re_match{0:"хелло 蛇"}) :-
    % Make sure that write_pcre() works witn non-ASCII.
    re_compile('хелло 蛇', Re, []),
    with_output_to(string(ReStr), write(current_output, Re)),
    re_matchsub(Re, ReStr, Result),
    assertion(re_match("^<regex>\\(.*\\)$", ReStr)).

% TODO: test for options in patterns with write_re_options().
%       (See comment in write_re_options() for PCRE2_ALLOPTIONS)

:- end_tests(pcre).

assertion_eq(A, B) :-
    assertion(A = B).

% Predicates used by re_foldl tests:

add_match(Dict, [Dict.0|List], List).

add_match2(Dict, [Dict|List], List).

increment(_Match, V0, V1) :- V1 is V0+1.

range_match(Dict, StringIndex-[MatchStart-Substring|List], StringIndex-List) :-
    Dict.(StringIndex.index) = MatchStart-MatchLen,
    sub_string(StringIndex.string, MatchStart, MatchLen, _, Substring).

post_split_dot([""], []).
post_split_dot(["",Letter|Xs], [Letter|Ys]) :-
    post_split_dot(Xs, Ys).


%! re_portray(+Stream, +Regex) is det.
%
%  Output debug info for a Regex on Stream (used in tests).
%  (defined in pcre4pl.c)

re_portray(Regex) :-
    pcre:re_portray(current_output, Regex).

re_portray_string(Regex, String) :-
    with_output_to(string(String),
                   pcre:re_portray(current_output, Regex)).

%! re_portray_match_options(+Stream, +Options) is det.
%
% Output debug info from parsing Options on Stream (used in tests).
%  (defined in pcre4pl.c)

re_portray_match_options_string(Options, String) :-
    with_output_to(string(String),
                   pcre:re_portray_match_options(current_output, Options)).

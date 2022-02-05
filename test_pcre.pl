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

:- encoding(utf8).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).

:- use_module(library(plunit)).
:- use_module(library(pcre)).
:- use_module(library(error)).

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

re_test(compile_option1) :-
    re_compile("a+b", _Re, [compat(javascript)]).
re_test(compile_option2, error(domain_error(compat_option, qqsv), _)) :-
    re_compile("a+b", _Re, [compat(qqsv)]).

re_test(start, Sub == re_match{0:"es"}) :-
    re_compile("e.", Re, []),
    re_matchsub(Re, "aapenootjes", Sub, [start(4)]).
re_test(fold1, Words == ["aap", "noot", "mies"]) :-
    re_foldl(add_match, "[a-z]+", "aap noot mies", Words, [], []).
re_test(fold2, Words == [re_match{0:"aap"},re_match{0:"noot"},re_match{0:"mies"}]) :-
    re_foldl(add_match2, "[a-z]+", "  aap    noot mies ", Words, [], []).
re_test(named, [Sub, RegexStr] ==
	       [re_match{0:"2017-04-20",
			 date:"2017-04-20",
			 day:"20",month:"04",year:"2017"},
		"<regex>(/(?<date> (?<year>(?:\\d\\d)?\\d\\d) -\n\t\t(?<month>\\d\\d) - (?<day>\\d\\d) )/ [EXTENDED NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_STRING] $capture=4 {0:CAP_STRING 1:date:CAP_STRING 2:year:CAP_STRING 3:month:CAP_STRING 4:day:CAP_STRING})"]) :-
    re_compile("(?<date> (?<year>(?:\\d\\d)?\\d\\d) -
		(?<month>\\d\\d) - (?<day>\\d\\d) )", Re,
	       [extended]),
    re_matchsub(Re, "2017-04-20", Sub, []),
    pcre:'$re_portray_string'(Re, RegexStr).
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
re_test(replace_all2c, [NewString == 'XbXc']) :-
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
     [condition(re_config(utf8(true))),
      true(NewString == "網目錦蛇 [reticulated python へび]")]) :-
    re_replace('àmímé níshíkíhéꜜbì', "reticulated python へび",
	       '網目錦蛇 [àmímé níshíkíhéꜜbì]', NewString).
re_test(replace_unicode2,
     [condition(re_config(utf8(true))),
      true(NewString == "網目錦へび [àmímé níshíkíhéꜜbì]")]) :-
    re_replace('(a蛇é)+', "へび",
	       '網目錦a蛇éa蛇éa蛇éa蛇é [àmímé níshíkíhéꜜbì]', NewString).
re_test(replace_unicode3,
     [condition(re_config(utf8(true))),
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

re_test(config_not_compound1, error(type_error(compound,version(A,B)),_)) :-
    re_config(version(A,B)).
re_test(config_not_compound2, error(type_error(compound,foo(A,B)),_)) :-
    re_config(foo(A,B)).
re_test(config_not_compound3, error(type_error(compound,bsr),_)) :-
    re_config(bsr).
re_test(config_not_compound4, error(type_error(compound,123),_)) :-
    re_config(123).
re_test(config_not_compound5, error(instantiation_error,_)) :-
    re_config(_).
re_test(config_invalid, error(existence_error(re_config,qqsv(V)),_)) :-
    re_config(qqsv(V)).
re_test(config_version) :-
    re_config(version(V)),
    must_be(atom, V), % TODO: V is of the form '8.39 2016-06-14'.
    re_config(version(V)). % Check that it takes an argument
re_test(config_version_type, fail) :-
    re_config(version(V)),
    atom_string(V, Vstr),
    re_config(version(Vstr)).
test(config_version_value1, [setup((re_config(version(V)),
			     atomic_concat(V, '---', V2))),
                             fail]) :-
    % Test that re_config(version(V2)) fails when given an argument
    % that is guaranteed to not be the version (it has an extra '---' on the end)
    re_config(version(V2)).
test(config_version_value2, [setup(re_config(version(V)))]) :-
    re_config(version(V)).
re_test(config_utf8) :-
    re_config(utf8(V)),
    must_be(boolean, V).
re_test(config_utf8_value, [nondet]) :- % For verifying that this works: condition(re_config(utf8(true)))
    (   re_config(utf8(true)) % TODO: shouldn't leave choicepoint
    ;   re_config(utf8(false))
    ).
re_test(config_unicode_properties) :-
    re_config(unicode_properties(V)),
    must_be(boolean, V).
re_test(config_jit) :-
    re_config(jit(V)),
    must_be(boolean, V).
re_test(config_jittarget) :-
    re_config(jittarget(V)),
    must_be(atom, V).
re_test(config_newline) :-
    re_config(newline(V)),
    must_be(integer, V).
re_test(config_bsr) :-
    re_config(bsr(V)),
    must_be(integer, V).
re_test(config_link_size) :-
    re_config(link_size(V)),
    must_be(integer, V).
re_test(config_posix_malloc_threshold) :-
    re_config(posix_malloc_threshold(V)),
    must_be(integer, V).
re_test(config_parens_limit) :-
    re_config(parens_limit(V)),
    must_be(integer, V).
re_test(config_match_limit) :-
    re_config(match_limit(V)),
    must_be(integer, V).
re_test(config_match_limit_recursion) :-
    re_config(match_limit_recursion(V)),
    must_be(integer, V).
re_test(config_stackrecurse) :-
    re_config(stackrecurse(V)),
    must_be(boolean, V).

re_test(compile_portray_0,
        RegexStr == "<regex>(/./ [NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_STRING] $capture=0)") :-
    re_compile(".", Regex, []),
    pcre:'$re_portray_string'(Regex, RegexStr).
re_test(compile_portray_1a,
        RegexStr == "<regex>(/(.)/ [NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_STRING] $capture=1 {0:CAP_STRING 1:CAP_STRING})") :-
    re_compile("(.)", Regex, []),
    pcre:'$re_portray_string'(Regex, RegexStr).
re_test(compile_portray_1b,
        RegexStr == "<regex>(/(.)/ [NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_ATOM] $capture=1 {0:CAP_ATOM 1:CAP_ATOM})") :-
    re_compile("(.)", Regex, [capture_type(atom)]),
    pcre:'$re_portray_string'(Regex, RegexStr).
re_test(compile_portray_2,
        RegexStr == "<regex>(/(?<foo>.)([a-z]*)(?<bar_A>.)/ [NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_STRING] $capture=3 {0:CAP_STRING 1:foo:CAP_STRING 2:CAP_STRING 3:bar:CAP_ATOM})") :-
    re_compile("(?<foo>.)([a-z]*)(?<bar_A>.)", Regex, []),
    pcre:'$re_portray_string'(Regex, RegexStr).

re_test(compile_config_1,
     RegexStr == "<regex>(/./ [ANCHORED CASELESS DOLLAR_ENDONLY DOTALL DUPNAMES EXTENDED EXTRA FIRSTLINE JAVASCRIPT_COMPAT MULTILINE NO_AUTO_CAPTURE NO_UTF8_CHECK UCP UNGREEDY UTF8 BSR_ANYCRLF NEWLINE_CRLF CAP_RANGE] $capture=0)") :-
    re_compile('.',  Regex,
	       [anchored(true),
		auto_capture(false),
		caseless(true),
		dollar_endonly(true),
		dotall(true),
		dupnames(true),
		extended(true),
		extra(true),
		firstline(true),
		greedy(false),
		compat(javascript),
		multiline(true),
		ucp(true),
		optimize(true),
		capture_type(range),
		bsr(anycrlf),
		newline(crlf)
	       ]),
    pcre:'$re_portray_string'(Regex, RegexStr).

re_test(compile_config_1_inverse,
     RegexStr == "<regex>(/./ [JAVASCRIPT_COMPAT NO_UTF8_CHECK UTF8 BSR_UNICODE NEWLINE_CR CAP_RANGE] $capture=0)") :-
    re_compile('.', Regex,
	       [anchored(false),
		auto_capture(true),
		caseless(false),
		dollar_endonly(false),
		dotall(false),
		dupnames(false),
		extended(false),
		extra(false),
		firstline(false),
		greedy(true),
		compat(javascript), % duplicated
		multiline(false),
		ucp(false),
		bol(false),
		eol(false),
		empty(false),
		empty_atstart(false),
		optimize(false),
		capture_type(range),
		bsr(unicode),
		newline(cr),
		% Invert them (they'll be ignored):
		anchored(true),
		caseless(true),
		dollar_endonly(true),
		dotall(true),
		dupnames(true),
		extended(true),
		extra(true),
		firstline(true),
		greedy(false),
		compat(ignored),
		multiline(true),
		auto_capture(false),
		ucp(true),
		bol(true),
		eol(true),
		empty(true),
		empty_atstart(true),
		optimize(true),
		capture_type(string),
		bsr(anycrlf),
		newline(lf)
	       ]),
    pcre:'$re_portray_string'(Regex, RegexStr).

re_test(compile_config_2,
     RegexStr == "<regex>(/./ [NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_ATOM] $capture=0)") :-
    re_compile('.', Regex, [multiline(false),caseless(false),capture_type(atom),foo]),
    pcre:'$re_portray_string'(Regex, RegexStr).

re_test(compile_config_3,
     RegexStr == "<regex>(/./ [CASELESS MULTILINE NO_UTF8_CHECK UTF8 NEWLINE_LF CAP_TERM] $capture=0)") :-
    re_compile('.', Regex, [qqsv,zot(123),optimise(false),capture_type(term),multiline(true),caseless(true),newline(lf)]),
    pcre:'$re_portray_string'(Regex, RegexStr).

re_test(compile_config_4, error(type_error(option, newline(qqsv)), _)) :-
    re_compile('.', _Regex, [newline(qqsv)]).

re_test(compile_exec_1,
     [RegexStr,  MatchOptsStr, Sub, Sub2] ==
     ["<regex>(/./ [ANCHORED NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_STRING] $capture=0)",
      "NOTBOL NOTEMPTY NOTEMPTY_ATSTART NOTEOL NO_UTF8_CHECK NEWLINE_ANYCRLF $start=0",
      re_match{0:"a"},
      re_match{0:"b"}]) :-
    re_compile('.', Regex, [anchored(true),bol(false),eol(false),empty(false),empty_atstart(false),start(666)]), % start(666) is ignored
    MatchOpts = [anchored(false),bol(false),eol(false),empty(false),empty_atstart(false),start(0)], % anchored(false) overrides
    pcre:'$re_portray_match_options_string'(MatchOpts, MatchOptsStr),
    re_matchsub(Regex, "abc", Sub, MatchOpts),
    pcre:'$re_portray_string'(Regex, RegexStr),
    re_matchsub(Regex, "abc", Sub2, [start(1)|MatchOpts]).

re_test(compile_exec_2,
     MatchOptsStr == "NO_UTF8_CHECK NEWLINE_ANYCRLF $start=0") :-
    pcre:'$re_portray_match_options_string'([anchored(false),bol(true),eol(true),empty(true),empty_atstart(true)],
					    MatchOptsStr).

re_test(compile_exec_3,
     MatchOptionsStr == "NO_UTF8_CHECK NEWLINE_ANYCRLF $start=0") :-
    pcre:'$re_portray_match_options_string'([], MatchOptionsStr).

re_test(match_ok_start, Sub==re_match{0:"c"}) :-
    re_matchsub('.', "abc", Sub, [start(2)]).
re_test(match_bad_start1, error(domain_error(offset,-1),_)) :- % TODO: -1 vs 3
    re_matchsub('.', "abc", _Sub, [start(3)]).
re_test(match_bad_start2,  error(type_error(option,start=3),_)) :-
    re_matchsub('.', "abc", _Sub, [start=3]).

re_test(replace_bad_ref_1, error(existence_error(key,1,re_match{0:0-1}),_)) :-
    re_replace(".", "$1", "abc", _).
re_test(replace_bad_ref_2, error(existence_error(key,1,re_match{0:0-1,foo:0-1}),_)) :-
    re_replace("(?<foo>.)", "$1", "abc", _).
re_test(replace_bad_ref_3, error(existence_error(key,foob,re_match{0:0-1,foo:0-1}),_)) :-
    re_replace("(?<foo>.)", "${foob}", "abc", _).
re_test(replace_bad_ref_4, [blocked(re_match_range), Result == "xabc"]) :-
    % See document: `?<foo_A>` - the "_A" suffix isn't allowed.
    % TODO: This should throw an error but instead might  give weird results.
    re_replace("(?<foo_A>.)", "${foo}", "xabc", Result).

re_test(replace_escape_dollar1a, Result == "$bc") :-
    re_replace(".", "$$", "abc", Result).
re_test(replace_escape_dollar1b, [fixme(capture_type_atom), Result == '$bc']) :-
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
%
% https://www.w3.org/TR/xpath-functions/#func-replace
% I'm not sure why "cd" appears after the "]", but this is how it is
% specified in the XPath standard:
re_test(wb_2, fixme(javascript_compat)) :-
    re_replace("(ab)|(a)", "[1=$1][2=$2]", "abcd", Result, [compat(javascript)]),
    assertion(Result == "[1=ab][2=]cd").

% Fix for https://github.com/SWI-Prolog/packages-pcre/issues/6
re_test(wb_3, Result == "abbraccaddabbra") :-
    re_replace("a(.)"/g, "a$1$1", "abracadabra", Result).

:- end_tests(pcre).

add_match(Dict, [Dict.0|List], List).

add_match2(Dict, [Dict|List], List).

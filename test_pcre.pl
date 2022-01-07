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

:- begin_tests(pcre).

test(match1, Sub == re_match{0:"aap"}) :-
    re_compile("a+p", Re, []),
    re_matchsub(Re, "aapenootjes", Sub, []).
test(match1a, Sub == re_match{1:"aap", 2:"aaaaaaap", 0:"aapenootjes  aaaaaaap"}) :-
    re_matchsub("(a+?p).*?(a+?p)", "meer aapenootjes  aaaaaaapenootjes", Sub, []).
test(match1b, fail) :-
    re_compile("a+p", Re, [anchored(true)]),
    re_matchsub(Re, "---aapenootjes", _Sub, []).
test(match1c, Sub == re_match{0:"AAP"}) :-
    re_compile("a+p", Re, [anchored(false), caseless(true)]),
    re_matchsub(Re, "---AAPenootjes", Sub, []).

test(compile_option1) :-
    re_compile("a+b", _Re, [compat(javascript)]).
test(compile_option2, error(domain_error(compat_option, qqsv), _)) :-
    re_compile("a+b", _Re, [compat(qqsv)]).

test(start, Sub == re_match{0:"es"}) :-
    re_compile("e.", Re, []),
    re_matchsub(Re, "aapenootjes", Sub, [start(4)]).
test(fold, Words == ["aap", "noot", "mies"]) :-
    re_foldl(add_match, "[a-z]+", "aap noot mies", Words, [], []).
test(fold2, Words == [re_match{0:"aap"},re_match{0:"noot"},re_match{0:"mies"}]) :-
    re_foldl(add_match2, "[a-z]+", "  aap    noot mies ", Words, [], []).
test(named, [Sub, RegexStr] ==
	    [re_match{0:"2017-04-20",
		      date:"2017-04-20",
		      day:"20",month:"04",year:"2017"},
	     "<regex>(/(?<date> (?<year>(?:\\d\\d)?\\d\\d) -\n\t\t(?<month>\\d\\d) - (?<day>\\d\\d) )/ [EXTENDED NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_STRING] capture(4){0:- 1:date:CAP_STRING} 2:year:CAP_STRING} 3:month:CAP_STRING} 4:day:CAP_STRING})"]) :-
    re_compile("(?<date> (?<year>(?:\\d\\d)?\\d\\d) -
		(?<month>\\d\\d) - (?<day>\\d\\d) )", Re,
	       [extended]),
    re_matchsub(Re, "2017-04-20", Sub, []),
    pcre:'$re_portray_string'(Re, RegexStr).
test(typed, Sub == re_match{0:"2017-04-20",
			    date:"2017-04-20",
			    day:20,month:4,year:2017}) :-
    re_matchsub("(?<date> (?<year_I>(?:\\d\\d)?\\d\\d) -
		 (?<month_I>\\d\\d) - (?<day_I>\\d\\d) )"/x,
		"2017-04-20", Sub, []).
test(typed2, Sub == re_match{0:"2017-04-20",
			    date:"2017-04-20",
			    day:20,month_:4,year_x:2017}) :-
    % Names with more than one "_", for testing type suffix
    re_matchsub("(?<date> (?<year_x_I>(?:\\d\\d)?\\d\\d) -
		 (?<month__I>\\d\\d) - (?<day_I>\\d\\d) )"/x,
		"2017-04-20", Sub, []).
test(range, Sub == re_match{0:"Name: value", value:6-5}) :-
    re_matchsub(".*:\\s(?<value_R>.*)"/x, "Name: value", Sub, []).
test(capture_string, Subs == re_match{0:"abc", 1:"a", 2:"b", 3:"c"}) :-
    re_matchsub('(a)(b)(c)', 'xabc', Subs, [capture_type(string)]).
test(capture_atom, [true(Subs == re_match{0:'abc', 1:'a', 2:'b', 3:'c'}), fixme(global_capture_type)]) :-
    re_matchsub('(a)(b)(c)', 'xabc', Subs, [capture_type(atom)]).
test(capture_range, [true(Subs == re_match{0:'abc', 1:0-1, 2:1-1, 3:2-1}), fixme(global_capture_type)]) :-
    re_matchsub('(a+)(b+)(c+)', 'xabc', Subs, [extended(true), capture_type(range)]).
test(capture_atom2, [true(Subs == re_match{0:"Name: value", value:'value'}), fixme(global_capture_type)]) :-
    re_matchsub(".*:\\s(?<value>.*)", "Name: value", Subs, [extended(true), capture_type(atom)]).
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

test(replace_unicode1,
     [condition(re_config(utf8(true))),
      true(NewString == "網目錦蛇 [reticulated python へび]")]) :-
    re_replace('àmímé níshíkíhéꜜbì', "reticulated python へび",
	       '網目錦蛇 [àmímé níshíkíhéꜜbì]', NewString).
test(replace_unicode2,
     [condition(re_config(utf8(true))),
      true(NewString == "網目錦へび [àmímé níshíkíhéꜜbì]")]) :-
    re_replace('(a蛇é)+', "へび",
	       '網目錦a蛇éa蛇éa蛇éa蛇é [àmímé níshíkíhéꜜbì]', NewString).
test(replace_unicode3,
     [condition(re_config(utf8(true))),
      true(NewString == "網目へび [àmímé níshíkíhéꜜbì]")]) :-
    re_replace("[蛇錦]+", "へび",
	       "網目錦蛇 [àmímé níshíkíhéꜜbì]", NewString).

test(config_not_compound1, error(type_error(compound,version(A,B)),_)) :-
    re_config(version(A,B)).
test(config_not_compound2, error(type_error(compound,foo(A,B)),_)) :-
    re_config(foo(A,B)).
test(config_not_compound3, error(type_error(compound,bsr),_)) :-
    re_config(bsr).
test(config_not_compound4, error(type_error(compound,123),_)) :-
    re_config(123).
test(config_not_compound5, error(instantiation_error,_)) :-
    re_config(_).
test(config_invalid, error(existence_error(re_config,qqsv(V)),_)) :-
    re_config(qqsv(V)).
test(config_version) :-
    re_config(version(V)),
    must_be(atom, V), % TODO: V is of the form '8.39 2016-06-14'.
    re_config(version(V)). % Check that it takes an argument
test(config_version_type, fail) :-
    re_config(version(V)),
    atom_string(V, Vstr),
    re_config(version(Vstr)).
test(config_version_value, [setup((re_config(version(V)),
				   atomic_concat(V, ' ', V2))),
			    fail]) :-
    re_config(version(V2)).
test(config_utf8) :-
    re_config(utf8(V)),
    must_be(boolean, V).
test(config_utf8_value, [nondet]) :- % For verifying that this works: condition(re_config(utf8(true)))
    (   re_config(utf8(true)) % TODO: shouldn't leave choicepoint
    ;   re_config(utf8(false))
    ).
test(config_unicode_properties) :-
    re_config(unicode_properties(V)),
    must_be(boolean, V).
test(config_jit) :-
    re_config(jit(V)),
    must_be(boolean, V).
test(config_jittarget) :-
    re_config(jittarget(V)),
    must_be(atom, V).
test(config_newline) :-
    re_config(newline(V)),
    must_be(integer, V).
test(config_bsr) :-
    re_config(bsr(V)),
    must_be(integer, V).
test(config_link_size) :-
    re_config(link_size(V)),
    must_be(integer, V).
test(config_posix_malloc_threshold) :-
    re_config(posix_malloc_threshold(V)),
    must_be(integer, V).
test(config_parens_limit) :-
    re_config(parens_limit(V)),
    must_be(integer, V).
test(config_match_limit) :-
    re_config(match_limit(V)),
    must_be(integer, V).
test(config_match_limit_recursion) :-
    re_config(match_limit_recursion(V)),
    must_be(integer, V).
test(config_stackrecurse) :-
    re_config(stackrecurse(V)),
    must_be(boolean, V).

test(compile_config_0,
     RegexStr == "<regex>(/./ [NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_STRING])") :-
    re_compile(".", Regex, []),
    pcre:'$re_portray_string'(Regex, RegexStr).

test(compile_config_1,
     RegexStr == "<regex>(/./ [ANCHORED CASELESS DOLLAR_ENDONLY DOTALL DUPNAMES EXTENDED EXTRA FIRSTLINE JAVASCRIPT_COMPAT MULTILINE NO_AUTO_CAPTURE NO_UTF8_CHECK UCP UNGREEDY UTF8 BSR_ANYCRLF NEWLINE_CRLF CAP_RANGE])") :-
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

test(compile_config_1_inverse,
     RegexStr == "<regex>(/./ [JAVASCRIPT_COMPAT NO_UTF8_CHECK UTF8 BSR_UNICODE NEWLINE_CR CAP_RANGE])") :-
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

test(compile_config_2,
     RegexStr == "<regex>(/./ [NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_ATOM])") :-
    re_compile('.', Regex, [multiline(false),caseless(false),capture_type(atom),foo]),
    pcre:'$re_portray_string'(Regex, RegexStr).

test(compile_config_3,
     RegexStr == "<regex>(/./ [CASELESS MULTILINE NO_UTF8_CHECK UTF8 NEWLINE_LF CAP_TERM])") :-
    re_compile('.', Regex, [qqsv,zot(123),optimise(false),capture_type(term),multiline(true),caseless(true),newline(lf)]),
    pcre:'$re_portray_string'(Regex, RegexStr).

test(compile_config_4, error(type_error(option, newline(qqsv)), _)) :-
    re_compile('.', _Regex, [newline(qqsv)]).

test(compile_exec_1,
     [RegexStr,  MatchOptsStr, Sub, Sub2] ==
     ["<regex>(/./ [ANCHORED NO_UTF8_CHECK UTF8 NEWLINE_ANYCRLF CAP_STRING])",
      "NOTBOL NOTEMPTY NOTEMPTY_ATSTART NOTEOL NO_UTF8_CHECK NEWLINE_ANYCRLF $start=0",
      re_match{0:"a"},
      re_match{0:"b"}]) :-
    re_compile('.', Regex, [anchored(true),bol(false),eol(false),empty(false),empty_atstart(false),start(666)]), % start(666) is ignored
    MatchOpts = [anchored(false),bol(false),eol(false),empty(false),empty_atstart(false),start(0)], % anchored(false) overrides
    pcre:'$re_portray_match_options_string'(MatchOpts, MatchOptsStr),
    re_matchsub(Regex, "abc", Sub, MatchOpts),
    pcre:'$re_portray_string'(Regex, RegexStr),
    re_matchsub(Regex, "abc", Sub2, [start(1)|MatchOpts]).

test(compile_exec_2,
     MatchOptsStr == "NO_UTF8_CHECK NEWLINE_ANYCRLF $start=0") :-
    pcre:'$re_portray_match_options_string'([anchored(false),bol(true),eol(true),empty(true),empty_atstart(true)],
					    MatchOptsStr).

test(compile_exec_3,
     MatchOptionsStr == "NO_UTF8_CHECK NEWLINE_ANYCRLF $start=0") :-
    pcre:'$re_portray_match_options_string'([], MatchOptionsStr).

test(match_ok_start, Sub==re_match{0:"c"}) :-
    re_matchsub('.', "abc", Sub, [start(2)]).
test(match_bad_start1, error(domain_error(offset,-1),_)) :- % TODO: -1 vs 3
    re_matchsub('.', "abc", _Sub, [start(3)]).
test(match_bad_start2, error(type_error(option,start=3),_)) :-
    re_matchsub('.', "abc", _Sub, [start=3]).

:- end_tests(pcre).

add_match(Dict, [Dict.0|List], List).

add_match2(Dict, [Dict|List], List).

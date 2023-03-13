% File used to test pcre blob save/load

:- module(test_pcre_load, [match_date/2, date_re/1]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).

:- use_module(library(pcre)).

user:goal_expansion(re_compile(Pattern, Re, Options), true) :-
    re_compile(Pattern, Re, Options).

match_date(DateStr, Sub) :-
    date_re(Re),
    assertion(blob(Re, regex)),
    % with_output_to(string(ReStr),
    %                re_portray(current_output, Re), % TODO: remove
    % "<regex>(/(?<date> (?<year>(?:\\d\\d)?\\d\\d) -\n\t\t(?<month>\\d\\d) - (?<day>\\d\\d) )/ [EXTENDED BSR_UNICODE CAP_STRING] $capture=4 {4 0:CAP_DEFAULT 1:date:CAP_DEFAULT 2:year:CAP_DEFAULT 3:month:CAP_DEFAULT 4:day:CAP_DEFAULT})".
    re_matchsub(Re, DateStr, Sub).

date_re(Re) :-
    re_compile("(?<date> (?<year>(?:\\d\\d)?\\d\\d) -
		(?<month>\\d\\d) - (?<day>\\d\\d) )", Re,
	       [extended(true)]).



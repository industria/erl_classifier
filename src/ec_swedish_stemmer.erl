%%%-------------------------------------------------------------------
%%% File    : ec_swedish_stemmer.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Implements a Swedish stemmer based on the snowball
%%% See: http://snowball.tartarus.org/algorithms/swedish/stemmer.html
%%%
%%% Created :  5 May 2011 by James Lindstorff <james@ind-x301>
%%%-------------------------------------------------------------------
-module(ec_swedish_stemmer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([stem/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: stem
%% Description: Stem a word using the Swedish Snowball stemmer.
%%--------------------------------------------------------------------
stem(Word) when is_binary(Word) ->
    W = unicode:characters_to_list(Word, utf8),
    Stem = stem(W),
    unicode:characters_to_binary(Stem, utf8, utf8);
stem(Word) when length(Word) > 3 ->
    LWord = string:to_lower(Word),
    {ok, R1, _} = regions(LWord),
    {ok, S1Word, R1AfterStep1} = step1a(LWord, R1),
    {ok, S2Word, R1AfterStep2} = step2(S1Word, R1AfterStep1),
    {ok, Stem, _} = step3(S2Word, R1AfterStep2),
    Stem;
stem(Word) ->
    string:to_lower(Word).

%%====================================================================
%% Internal functions
%%====================================================================
regions(Word) ->
    %% Creates the regions R1 and R2 for the stemmer as defined by:
    %% R1 is the region after the first non-vowel following a vowel,
    %% or is the null region at the end of the word if there is no 
    %% such non-vowel.
    %% R2 is the region after the first non-vowel following a vowel
    %% in R1, or is the null region at the end of the word if there
    %% is no such non-vowel.
    Vowels = "aeiouy\x{E4}\x{E5}\x{F6}",
    R1 = ec_stemming:r1(Word, Vowels),
    %% R1 is adjusted so that the region before it contains 
    %% at least 3 letters.
    R1Adjusted = ec_stemming:adjustR1(Word, R1),
    %% R2 is not used by the Swedish stemmer, so it's not calculated
    %% To calculate it do r1(R1)
    {ok, R1Adjusted, []}.


step1a(Word, R1) ->
    %% Search for the longest among the following suffixes in R1, 
    %% and perform the action indicated. Which is delete.
    Step1suffix = ["heterna", "hetens", "anden", "andes", "andet",
		   "arens", "arnas", "ernas", "heten", "heter",
		   "ornas", "ande", "aren", "arna", "arne", "aste",
		   "erna", "erns", "orna", "ades", "ade", "are",
		   "ast", "ens", "ern", "het", "ad", "ar", "as",
		   "at", "en", "er", "es", "or", "a", "e"],
    case ec_stemming:match_suffix(Step1suffix, R1) of
	{ok, MatchedSuffix} ->
	    ec_stemming:remove_suffix(Word, R1, MatchedSuffix);
	{nomatch} ->
	    %% This corresponds to step 1 b
	    SEndings = ["b", "c", "d", "f", "g", "h", "j", "k", "l",
			"m", "n", "o", "p", "r", "t", "v", "y"],
	    ec_stemming:remove_trailing_s_ending(Word, R1, SEndings)
    end.

step2(Word, R1) ->
    Suffixes = ["dd", "gd", "nn", "dt", "gt", "kt", "tt"],
    ec_stemming:remove_last_on_suffix_match(Word, R1, Suffixes).

step3(Word, R1) ->
    {ok, WordLast, R1Last} = step3removelast(Word, R1),
    step3removesuffix(WordLast, R1Last).

step3removelast(Word, R1) ->
    Suffixes = ["fullt", "l\x{F6}st"],
    ec_stemming:remove_last_on_suffix_match(Word, R1, Suffixes).

step3removesuffix(Word, R1) ->
    Step3suffix = ["lig", "ig", "els"],
    case ec_stemming:match_suffix(Step3suffix, R1) of
	{ok, MatchedSuffix} ->
	    ec_stemming:remove_suffix(Word, R1, MatchedSuffix);
	{nomatch} ->
	    {ok, Word, R1}
    end.


%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).
stem_test() ->
    %% Run through all examples from
    %% http://snowball.tartarus.org/algorithms/swedish/stemmer.html
    ?assertEqual("jakt", stem("jakt")),
    ?assertEqual("jaktb\x{F6}ss", stem("jaktb\x{F6}ssa")),
    ?assertEqual("jakt", stem("jakten")),
    ?assertEqual("jakthund", stem("jakthund")),
    ?assertEqual("jaktkarl", stem("jaktkarl")),
    ?assertEqual("jaktkarl", stem("jaktkarlar")),
    ?assertEqual("jaktkarl", stem("jaktkarlarne")),
    ?assertEqual("jaktkarl", stem("jaktkarlens")),
    ?assertEqual("jaktl\x{F6}jtnant", stem("jaktl\x{F6}jtnant")),
    ?assertEqual("jaktl\x{F6}jtnant", stem("jaktl\x{F6}jtnanten")),
    ?assertEqual("jaktl\x{F6}jtnant", stem("jaktl\x{F6}jtnantens")),
    ?assertEqual("jalusi", stem("jalusi")),
    ?assertEqual("jalusi", stem("jalusien")),
    ?assertEqual("jalusi", stem("jalusier")),
    ?assertEqual("jalusi", stem("jalusierna")),
    ?assertEqual("jamaik", stem("jamaika")),
    ?assertEqual("jam", stem("jamat")),
    ?assertEqual("jamr", stem("jamrande")),
    ?assertEqual("jamt", stem("jamt")),
    ?assertEqual("jand", stem("jande")),
    ?assertEqual("januari", stem("januari")),
    ?assertEqual("japansk", stem("japanska")),
    ?assertEqual("jaquet", stem("jaquette")),
    ?assertEqual("jaquettekapp", stem("jaquettekappa")),
    ?assertEqual("jargong", stem("jargong")),
    ?assertEqual("jasmin", stem("jasmin")),
    ?assertEqual("jasmin", stem("jasminen")),
    ?assertEqual("jasmin", stem("jasminer")),
    ?assertEqual("jasminh\x{E4}ck", stem("jasminh\x{E4}ck")),
    ?assertEqual("jaspis", stem("jaspis")),
    ?assertEqual("jas\x{E5}", stem("jas\x{E5}")),
    ?assertEqual("jav\x{E4}l", stem("jav\x{E4}l")),
    ?assertEqual("jazzvind", stem("jazzvindens")),
    ?assertEqual("jcrn", stem("jcrn")),
    ?assertEqual("jcsus", stem("jcsus")),
    ?assertEqual("je", stem("je")),
    ?assertEqual("jemf\x{F6}r", stem("jemf\x{F6}ra")),
    ?assertEqual("jemf\x{F6}r", stem("jemf\x{F6}ras")),
    ?assertEqual("jemf\x{F6}r", stem("jemf\x{F6}relse")),
    ?assertEqual("jemf\x{F6}r", stem("jemf\x{F6}relser")),
    ?assertEqual("klo", stem("klo")),
    ?assertEqual("kloak", stem("kloaken")),
    ?assertEqual("klock", stem("klock")),
    ?assertEqual("klock", stem("klocka")),
    ?assertEqual("klockan", stem("klockan")),
    ?assertEqual("klockan", stem("klockans")),
    ?assertEqual("klock", stem("klockare")),
    ?assertEqual("klock", stem("klockaren")),
    ?assertEqual("klock", stem("klockarens")),
    ?assertEqual("klockarf", stem("klockarfar")),
    ?assertEqual("klockarn", stem("klockarn")),
    ?assertEqual("klockarson", stem("klockarsonen")),
    ?assertEqual("klock", stem("klockas")),
    ?assertEqual("klockkedjan", stem("klockkedjan")),
    ?assertEqual("klocklik", stem("klocklikt")),
    ?assertEqual("klock", stem("klockor")),
    ?assertEqual("klock", stem("klockorna")),
    ?assertEqual("klock", stem("klockornas")),
    ?assertEqual("klockor", stem("klockors")),
    ?assertEqual("klockringning", stem("klockringning")),
    ?assertEqual("kloek", stem("kloekornas")),
    ?assertEqual("klok", stem("klok")),
    ?assertEqual("klok", stem("kloka")),
    ?assertEqual("klok", stem("klokare")),
    ?assertEqual("klok", stem("klokast")),
    ?assertEqual("klok", stem("klokaste")),
    ?assertEqual("klok", stem("kloke")),
    ?assertEqual("klok", stem("klokhet")),
    ?assertEqual("klok", stem("klokheten")),
    ?assertEqual("klokt", stem("klokt")),
    ?assertEqual("klolikn", stem("kloliknande")),
    ?assertEqual("klor", stem("klor")),
    ?assertEqual("klorn", stem("klorna")),
    ?assertEqual("kloroform", stem("kloroform")),
    ?assertEqual("klost", stem("kloster")),
    ?assertEqual("klosterg\x{E5}rd", stem("klosterg\x{E5}rden")),
    ?assertEqual("klosterlik", stem("klosterlik")),
    ?assertEqual("klot", stem("klot")),
    ?assertEqual("klotb", stem("klotb")),
    ?assertEqual("klotrund", stem("klotrund")).


regions_test() ->
    %% Tests from: http://snowball.tartarus.org/texts/r1r2.html
    %% Note for Swedish stemmer R2 is always an empty list
    ?assertMatch({ok, "iful", []}, regions("beautiful")),
    ?assertMatch({ok, "y", []}, regions("beauty")),
    ?assertMatch({ok, [], []}, regions("beau")),
    %% Next one will be hit by the R1 3 char adjust
    ?assertMatch({ok, "madversion", []}, regions("animadversion")),
    ?assertMatch({ok, "kled", []}, regions("sprinkled")),
    ?assertMatch({ok, "harist", []}, regions("eucharist")).

regions_bestemmelse_test() ->
    ?assertMatch({ok, "temmelse", []}, regions("bestemmelse")).


step1a_bestemmelse_test() ->
    Word = "bestemmelse",
    {ok, R1, _R2} = regions(Word),
    ?assertMatch({ok, "bestemmels", "temmels"}, step1a(Word, R1)).

step1a_nomatch_test() ->
    ?assertMatch({ok, "nomatch", "nomatch"}, step1a("nomatch", "nomatch")).

step3_test() ->
    ?assertMatch({ok, "goodfull", "dfull"}, step3("goodfullt","dfullt")),
    ?assertMatch({ok, "goodl\x{F6}s", "dl\x{F6}s"}, step3("goodl\x{F6}st","dl\x{F6}st")),
    ?assertMatch({ok, "goodd", "d"}, step3("gooddlig","dlig")),
    ?assertMatch({ok, "goodd", "d"}, step3("gooddig","dig")),
    ?assertMatch({ok, "goodd", "d"}, step3("gooddels","dels")).
    
step3removelast_test() ->
    ?assertMatch({ok, "goodfull", "dfull"}, step3removelast("goodfullt","dfullt")),
    ?assertMatch({ok, "goodl\x{F6}s", "dl\x{F6}s"}, step3removelast("goodl\x{F6}st","dl\x{F6}st")).

step3removesuffix_test() ->
    ?assertMatch({ok, "goodd", "d"}, step3removesuffix("gooddlig","dlig")),
    ?assertMatch({ok, "goodd", "d"}, step3removesuffix("gooddig","dig")),
    ?assertMatch({ok, "goodd", "d"}, step3removesuffix("gooddels","dels")).

stem_binary_test() ->
    ?assertEqual(<<"unders\x{E5}t"/utf8>>, stem(<<"unders\x{E5}tter"/utf8>>)),
    ?assertEqual(<<"indtag"/utf8>>, stem(<<"indtage"/utf8>>)),
    ?assertEqual(<<"indt"/utf8>>, stem(<<"indt"/utf8>>)),
    ?assertEqual(<<"ind"/utf8>>, stem(<<"ind"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, stem(<<"i"/utf8>>)).

-endif.

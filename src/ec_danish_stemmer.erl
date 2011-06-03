%%%-------------------------------------------------------------------
%%% File    : ec_danish_stemmer.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Implements a Danish stemmer based on the snowball
%%% See:  http://snowball.tartarus.org/algorithms/danish/stemmer.html
%%%
%%% Created :  1 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_danish_stemmer).

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
%% Description: Stem a word using the Danish Snowball stemmer.
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
    {ok, S3Word, R1AfterStep3} = step3(S2Word, R1AfterStep2),
    {ok, Stem, _} = step4(S3Word, R1AfterStep3),
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
    Vowels = "aeiouy\x{E6}\x{F8}\x{E5}",
    R1 = ec_stemming:r1(Word, Vowels),
    %% R1 is adjusted so that the region before it contains 
    %% at least 3 letters.
    R1Adjusted = ec_stemming:adjustR1(Word, R1),
    %% R2 is not used by the Danish stemmer, so it's not calculated
    %% To calculate it do r1(R1)
    {ok, R1Adjusted, []}.



step1a(Word, R1) ->
    %% Search for the longest among the following suffixes in R1, 
    %% and perform the action indicated. Which is delete.
    Step1suffix = ["erendes", "erende", "hedens", "endes", "erede", 
		   "erens", "erets", "ernes", "ethed", "heden",
		   "heder", "ered", "ende", "enes", "eren", "erne",
		   "erer", "eres", "eret", "heds", "ene", "ens",
		   "ere", "ers", "ets", "hed", "en", "er", "es",
		   "et", "e"],
    case ec_stemming:match_suffix(Step1suffix, R1) of
	{ok, MatchedSuffix} ->
	    ec_stemming:remove_suffix(Word, R1, MatchedSuffix);
	{nomatch} ->
	    %% This corresponds to step 1 b
	    SEndings = ["a", "b", "c", "d", "f", "g", "h", "j", "k", "l", "m",
			"n", "o", "p", "r", "t", "v", "y", "z", "\x{E5}"],
	    ec_stemming:remove_trailing_s_ending(Word, R1, SEndings)
    end.

step2(Word, R1) ->
    Suffixes = ["gd", "dt", "gt", "kt"],
    ec_stemming:remove_last_on_suffix_match(Word, R1, Suffixes).

step3(Word, R1) ->
    {ok, WordIgst, R1Igst} = step3igst(Word, R1),
    {ok, Word3b, R13b} = step3loest(WordIgst, R1Igst),
    step3a(Word3b, R13b).

step3igst(Word, R1) ->
    %% If the R1/word ends igst, remove the final st.
    EndsIgst = lists:suffix("igst", R1),
    if
	EndsIgst =:= true ->
	    ec_stemming:remove_suffix(Word, R1, "st");
	true ->
	    {ok, Word, R1}
    end.
    
step3loest(Word, R1) ->
    %% Step 3 b suffix løst replace with løs
    Endsloest = lists:suffix("l\x{F8}st", R1),
    if
	Endsloest =:= true ->
	    {ok, ec_stemming:remove_last(Word), ec_stemming:remove_last(R1)};
	true ->
	    {ok, Word, R1}
    end.

step3a(Word, R1) ->
    %% Search for the longest among the following suffixes in R1, 
    %% and delate and repeat step 2
    Suffixes = ["elig", "els", "lig", "ig"],
    case ec_stemming:match_suffix(Suffixes, R1) of
	{ok, MatchedSuffix} ->
	    {ok, NewWord, NewR1} = ec_stemming:remove_suffix(Word, R1, MatchedSuffix),
	    step2(NewWord, NewR1);
	{nomatch} ->
	    {ok, Word, R1}
    end.

step4(Word, R1) when length(Word) =< 3 ->
    {ok, Word, R1};
step4(Word, R1) ->
    %% If the word ends with double consonant in R1, 
    %% remove one of the consonants
    DoubleConsonant = ["bb", "cc", "dd", "ff", "gg", "hh", "jj", "kk", 
		       "ll", "mm", "nn", "pp", "qq", "rr", "ss", "tt", 
		       "vv", "ww", "xx", "zz"],
    ec_stemming:remove_last_on_suffix_match(Word, R1, DoubleConsonant).

%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).
stem_test() ->
    %% Run through all examples from 
    %% http://snowball.tartarus.org/algorithms/danish/stemmer.html
    ?assertEqual("indtag", stem("indtage")),
    ?assertEqual("indtag", stem("indtagelse")),
    ?assertEqual("indtag", stem("indtager")),
    ?assertEqual("indtag", stem("indtages")),
    ?assertEqual("indtag", stem("indtaget")),
    ?assertEqual("indtil", stem("indtil")),
    ?assertEqual("indtog", stem("indtog")),
    ?assertEqual("indtraf", stem("indtraf")),
    ?assertEqual("indtryk", stem("indtryk")),
    ?assertEqual("indtr\x{E6}d", stem("indtr\x{E6}de")),
    ?assertEqual("indtr\x{E6}d", stem("indtr\x{E6}der")),
    ?assertEqual("indtr\x{E6}f", stem("indtr\x{E6}ffe")),
    ?assertEqual("indtr\x{E6}f", stem("indtr\x{E6}ffer")),
    ?assertEqual("indtr\x{E6}ng", stem("indtr\x{E6}ngende")),
    ?assertEqual("indt\x{E6}g", stem("indt\x{E6}gt")),
    ?assertEqual("indt\x{E6}g", stem("indt\x{E6}gter")),
    ?assertEqual("indvandred", stem("indvandrede")),
    ?assertEqual("indvandr", stem("indvandret")),
    ?assertEqual("indvend", stem("indvender")),
    ?assertEqual("indvend", stem("indvendig")),
    ?assertEqual("indvend", stem("indvendige")),
    ?assertEqual("indvend", stem("indvendigt")),
    ?assertEqual("indvending", stem("indvending")),
    ?assertEqual("indvending", stem("indvendingerne")),
    ?assertEqual("indvi", stem("indvie")),
    ?assertEqual("indvied", stem("indviede")),
    ?assertEqual("indvi", stem("indvielse")),
    ?assertEqual("indvi", stem("indvielsen")),
    ?assertEqual("indvielsesl\x{F8}ft", stem("indvielsesl\x{F8}fte")),
    ?assertEqual("indvielsestid", stem("indvielsestid")),
    ?assertEqual("indvi", stem("indvier")),
    ?assertEqual("indvi", stem("indvies")),
    ?assertEqual("indvi", stem("indviet")),
    ?assertEqual("indvikl", stem("indvikle")),
    ?assertEqual("indvikl", stem("indvikler")),
    ?assertEqual("indvold", stem("indvolde")),
    ?assertEqual("indvold", stem("indvoldene")),
    ?assertEqual("indvort", stem("indvortes")),
    ?assertEqual("ind\x{E5}nd", stem("ind\x{E5}nde")),
    ?assertEqual("ind\x{E5}nded", stem("ind\x{E5}ndede")),
    ?assertEqual("underst", stem("underste")),
    ?assertEqual("unders\x{E5}t", stem("unders\x{E5}tter")),
    ?assertEqual("unders\x{E5}t", stem("unders\x{E5}tters")),
    ?assertEqual("unders\x{F8}g", stem("unders\x{F8}g")),
    ?assertEqual("unders\x{F8}g", stem("unders\x{F8}ge")),
    ?assertEqual("unders\x{F8}g", stem("unders\x{F8}gelse")),
    ?assertEqual("unders\x{F8}g", stem("unders\x{F8}gelsen")),
    ?assertEqual("unders\x{F8}g", stem("unders\x{F8}ger")),
    ?assertEqual("unders\x{F8}g", stem("unders\x{F8}gt")),
    ?assertEqual("unders\x{F8}g", stem("unders\x{F8}gte")),
    ?assertEqual("undertryk", stem("undertryk")),
    ?assertEqual("undertryk", stem("undertrykke")),
    ?assertEqual("undertryk", stem("undertrykkelse")),
    ?assertEqual("undertryk", stem("undertrykker")),
    ?assertEqual("undertryk", stem("undertrykkere")),
    ?assertEqual("undertryk", stem("undertrykkeren")),
    ?assertEqual("undertryk", stem("undertrykkerens")),
    ?assertEqual("undertryk", stem("undertrykkeres")),
    ?assertEqual("undertryk", stem("undertrykkes")),
    ?assertEqual("undertryk", stem("undertrykt")),
    ?assertEqual("undertryk", stem("undertrykte")),
    ?assertEqual("undertryk", stem("undertryktes")),
    ?assertEqual("undertvang", stem("undertvang")),
    ?assertEqual("undertvung", stem("undertvunget")),
    ?assertEqual("undertvungn", stem("undertvungne")),
    ?assertEqual("undervej", stem("undervejs")),
    ?assertEqual("underverden", stem("underverdenen")),
    ?assertEqual("undervis", stem("undervise")),
    ?assertEqual("undervis", stem("underviser")),
    ?assertEqual("undervis", stem("undervises")),
    ?assertEqual("undervisning", stem("undervisning")),
    ?assertEqual("undervisning", stem("undervisningen")),
    ?assertEqual("undervist", stem("undervist")),
    ?assertEqual("undervist", stem("underviste")),
    ?assertEqual("underv\x{E6}rk", stem("underv\x{E6}rk")),
    ?assertEqual("underv\x{E6}rk", stem("underv\x{E6}rker")),
    ?assertEqual("undevis", stem("undevise")),
    ?assertEqual("undevist", stem("undeviste")),
    ?assertEqual("undfang", stem("undfange")),
    ?assertEqual("undfanged", stem("undfanged")).

regions_test() ->
    %% Tests from: http://snowball.tartarus.org/texts/r1r2.html
    %% Note for Danish stemmer R2 is always an empty list
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
    ?assertMatch({ok, "good", "d"}, step3("goodigst","digst")).


step3igst_test() ->
    ?assertMatch({ok, "goodigt", "digt"}, step3igst("goodigt","digt")),
    ?assertMatch({ok, "goodig", "dig"}, step3igst("goodigst","digst")).

step3loest_test() ->
    ?assertMatch({ok, "m\x{E5}ll\x{F8}s", "l\x{F8}s"}, step3loest("m\x{E5}ll\x{F8}st","l\x{F8}st")),
    ?assertMatch({ok, "modl\x{F8}s", "l\x{F8}s"}, step3loest("modl\x{F8}s","l\x{F8}s")).
    
step3a_test() ->
    ?assertMatch({ok, "bud", ""}, step3a("budelig", "elig")),
    ?assertMatch({ok, "buds", "s"}, step3a("budslig", "slig")),
    ?assertMatch({ok, "bud", ""}, step3a("budels", "els")),
    ?assertMatch({ok, "bud", ""}, step3a("budig", "ig")),
    ?assertMatch({ok, "budd", "d"}, step3a("buddtig", "dtig")),
    ?assertMatch({ok, "nomatch", "nomatch"}, step3a("nomatch", "nomatch")).
    
step4_test() ->
    ?assertMatch({ok, "nodoubledc", "bledc"}, step4("nodoubledc", "bledc")),
    ?assertMatch({ok, "ncc", "ncc"}, step4("ncc", "ncc")),
    ?assertMatch({ok, "noc", "noc"}, step4("nocc", "nocc")).

stem_binary_test() ->
    ?assertEqual(<<"unders\x{E5}t"/utf8>>, stem(<<"unders\x{E5}tter"/utf8>>)),
    ?assertEqual(<<"indtag"/utf8>>, stem(<<"indtage"/utf8>>)),
    ?assertEqual(<<"indt"/utf8>>, stem(<<"indt"/utf8>>)),
    ?assertEqual(<<"ind"/utf8>>, stem(<<"ind"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, stem(<<"i"/utf8>>)).

-endif.

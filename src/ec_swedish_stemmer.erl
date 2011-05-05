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
    W = [ X || <<X/utf8>> <= Word],
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
    R1 = r1(Word),
    %% R1 is adjusted so that the region before it contains 
    %% at least 3 letters.
    R1Adjusted = adjustR1(Word, R1),
    %% R2 is not used by the Swedish stemmer, so it's not calculated
    %% To calculate it do r1(R1)
    {ok, R1Adjusted, []}.

r1([]) ->
    [];
r1(Word) ->
    %% R1 is the region after the first non-vowel following a vowel,
    %% or is the null region at the end of the word if there is no 
    %% such non-vowel.
    [H | T] = Word,
    r1(T, H).
r1([], _Last) ->
    [];
r1([H | T], Last) ->
    PrevVowel = is_vowel(Last),
    CurrVowel = is_vowel(H),
    if
	(PrevVowel =:= true) and (CurrVowel =:= false) ->
	    T;
	true ->
	    r1(T, H)
    end.

is_vowel(Character) ->
    %% Classifies the character as being a vowel or not a vowel
    %% vowels are: aeiouyäåö
    lists:member(Character, "aeiouy\x{E4}\x{E5}\x{F6}").

adjustR1(Word, R1) ->
    %% Region before R1 needs to be at least 3 characters
    WordRegionLength = string:len(Word) - string:len(R1),
    if
	(3 =<  WordRegionLength) ->
	    R1;
	(3 > WordRegionLength) and (0 < WordRegionLength) ->
	    string:substr(Word, 4);
	true ->
	    Word
    end.

step1a(Word, R1) ->
    %% Search for the longest among the following suffixes in R1, 
    %% and perform the action indicated. Which is delete.
    Step1suffix = ["heterna", "hetens", "anden", "andes", "andet",
		   "arens", "arnas", "ernas", "heten", "heter",
		   "ornas", "ande", "aren", "arna", "arne", "aste",
		   "erna", "erns", "orna", "ades", "ade", "are",
		   "ast", "ens", "ern", "het", "ad", "ar", "as",
		   "at", "en", "er", "es", "or", "a", "e"],
    case matchsuffix(Step1suffix, R1) of
	{ok, MatchedSuffix} ->
	    removesuffix(Word, R1, MatchedSuffix);
	{nomatch} ->
	    step1b(Word, R1)
    end.

step1b(Word, R1) ->
    %% Delete trailing S if preceded by a valid s-ending
    IsSuffixS = lists:suffix("s", R1),
    if
	IsSuffixS ->
	    SEndings = ["b", "c", "d", "f", "g", "h", "j", "k", "l",
			"m", "n", "o", "p", "r", "t", "v", "y"],
	    BeforeS = string:substr(Word, string:len(Word) - 1, 1),
	    ValidSEnding = lists:any(fun(X) -> BeforeS =:= X end, SEndings),
	    if
		ValidSEnding =:= true ->
		    {ok, removelast(Word), removelast(R1)};
		true ->
		    {ok, Word, R1}
	    end;
	true ->
	    {ok, Word, R1}	    
    end.

removelast(Word) ->
    %% Remove last character from word
    ReverseWord = lists:reverse(Word),
    [_ | T] = ReverseWord,
    lists:reverse(T).

step2(Word, R1) ->
    %% Search for one of (dd, gd, nn, dt, gt, kt, tt) suffixes in R1, and if found
    %% delete the last letter
    Suffixes = ["dd", "gd", "nn", "dt", "gt", "kt", "tt"],
    IsSuffixFound = lists:any(fun(X) -> lists:suffix(X, R1) end, Suffixes),
    if
	IsSuffixFound =:= true ->
	    {ok, removelast(Word), removelast(R1)};
	true ->
	    {ok, Word, R1}
    end.


step3(Word, R1) ->
    {ok, WordLast, R1Last} = step3removelast(Word, R1),
    step3removesuffix(WordLast, R1Last).

%% "lig"   "ig"   "els" 
%% %%delete 

%% "löst" 
%% %%replace with lös 

%% "fullt" 
%% %%replace with full


step3removelast(Word, R1) ->
    Suffixes = ["fullt", "l\x{F6}st"],
    IsSuffixFound = lists:any(fun(X) -> lists:suffix(X, R1) end, Suffixes),
    if
	IsSuffixFound =:= true ->
	    {ok, removelast(Word), removelast(R1)};
	true ->
	    {ok, Word, R1}
    end.
    
step3removesuffix(Word, R1) ->
    Step3suffix = ["lig", "ig", "els"],
    case matchsuffix(Step3suffix, R1) of
	{ok, MatchedSuffix} ->
	    removesuffix(Word, R1, MatchedSuffix);
	{nomatch} ->
	    {ok, Word, R1}
    end.


matchsuffix([], _R1) ->
    {nomatch};

matchsuffix([H | T], R1) ->
    Match = lists:suffix(H, R1),
    if
	Match =:= true -> 
	    {ok, H};
	true ->
	    matchsuffix(T, R1)
    end.

removesuffix(Word, R1, Suffix) ->
    %% Remove the Suffix from Word and R1
    LenSuffix = string:len(Suffix),
    LenWord = string:len(Word),
    LenR1 = string:len(R1),
    NewWord = string:substr(Word, 1, LenWord - LenSuffix),
    NewR1 = string:substr(R1, 1, LenR1 - LenSuffix),
    {ok, NewWord, NewR1}.

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

adjustR1_test() ->
    ?assertEqual("urn", adjustR1("return", "turn")),
    ?assertEqual("return", adjustR1("return", "return")),
    ?assertEqual("urn", adjustR1("return", "urn")).

step1a_bestemmelse_test() ->
    Word = "bestemmelse",
    {ok, R1, _R2} = regions(Word),
    ?assertMatch({ok, "bestemmels", "temmels"}, step1a(Word, R1)).

step1a_nomatch_test() ->
    ?assertMatch({ok, "nomatch", "nomatch"}, step1a("nomatch", "nomatch")).

step1b_test() ->
    ?assertMatch({ok, "ddws", "ddws"}, step1b("ddws","ddws")),
    ?assertMatch({ok, "ddl", "l"}, step1b("ddls","ls")),
    ?assertMatch({ok, "bestemmel", "temmel"}, step1b("bestemmels","temmels")).

removelast_test() ->
    ?assertEqual("fis", removelast("fisk")),
    ?assertEqual("", removelast("k")).

step2_test() ->
    ?assertMatch({ok, "frisk", "frisk"}, step2("friskt","friskt")),
    ?assertMatch({ok, "frisk", "sk"}, step2("frisk","sk")),
    ?assertMatch({ok, "goog", "g"}, step2("googd","gd")),
    ?assertMatch({ok, "goog", "g"}, step2("googt","gt")),
    ?assertMatch({ok, "good", "d"}, step2("goodt","dt")).

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

is_vowel_test() ->
    ?assert(is_vowel($a)),
    ?assert(is_vowel($e)),
    ?assert(is_vowel($i)),
    ?assert(is_vowel($o)),
    ?assert(is_vowel($u)),
    ?assert(is_vowel($y)),
    ?assert(is_vowel(16#E4)), % ä
    ?assert(is_vowel(16#E5)), % å  
    ?assert(is_vowel(16#F6)), % ö
    ?assertNot(is_vowel($x)),
    ?assertNot(is_vowel($b)).

removesuffix_test() ->
    ?assertMatch({ok, "fisk", "k"}, removesuffix("fiskdel", "kdel", "del")).
    
stem_binary_test() ->
    ?assertEqual(<<"unders\x{E5}t"/utf8>>, stem(<<"unders\x{E5}tter"/utf8>>)),
    ?assertEqual(<<"indtag"/utf8>>, stem(<<"indtage"/utf8>>)),
    ?assertEqual(<<"indt"/utf8>>, stem(<<"indt"/utf8>>)),
    ?assertEqual(<<"ind"/utf8>>, stem(<<"ind"/utf8>>)),
    ?assertEqual(<<"i"/utf8>>, stem(<<"i"/utf8>>)).

-endif.

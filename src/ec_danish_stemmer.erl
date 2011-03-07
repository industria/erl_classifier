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
%% Description: Stems a word using the Danish Snowball stemmer.
%%--------------------------------------------------------------------
stem(Word) ->
    LowerWord = string:to_lower(Word),
    {ok, R1, _R2} = regions(LowerWord), %% R2 unused in danish stemmer
    {ok, S1aLowerWord, S1aR1} = step1a(LowerWord, R1),
    {ok, S1bLowerWord, S1bR1} = step1b(S1aLowerWord, S1aR1),
    {ok, S2LowerWord, S2R1} = step2(S1bLowerWord, S1bR1),
    {ok, S3LowerWord, S3R1} = step3(S2LowerWord, S2R1),
    {ok, S4LowerWord, S4R1} = step4(S3LowerWord, S3R1),
    S4LowerWord.

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
    R2 = r1(R1),
    {ok, R1Adjusted, R2}.

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
	(PrevVowel == true) and (CurrVowel == false) ->
	    T;
	true -> %% else
	    r1(T, H)
    end.

is_vowel(Character) ->
    %% Classifies the character as being a vowel or not a vowel 
    lists:member(Character, "aeiouyæøå").

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
    Step1suffix = ["erendes", "erende", "hedens", "endes", "erede", 
		   "erens", "erets", "ernes", "ethed", "heden",
		   "heder", "ered", "ende", "enes", "eren", "erne",
		   "erer", "eres", "eret", "heds", "ene", "ens",
		   "ere", "ers", "ets", "hed", "en", "er", "es",
		   "et", "e"],
    case step1asuffix(Step1suffix, R1) of
	{ok, MatchedSuffix} ->
	    LenMatchSuffix = string:len(MatchedSuffix),
	    LenWord = string:len(Word),
	    LenR1 = string:len(R1),
	    NewWord = string:substr(Word, 1, LenWord - LenMatchSuffix),
	    NewR1 = string:substr(R1, 1, LenR1 - LenMatchSuffix),
	    {ok, NewWord, NewR1};
	{nomatch} ->
	    {ok, Word, R1}
    end.

step1asuffix([], _R1) ->
    {nomatch};

step1asuffix([H | T], R1) ->
    Match = lists:suffix(H, R1),
    if
	Match == true -> 
	    {ok, H};
	true ->
	    step1asuffix(T, R1)
    end.

step1b(Word, R1) ->
    %% Delete trailing S if preceded by a valid s-ending
    SEndings = ["a", "b", "c", "d", "f", "g", "h", "j", "k", "l", "m",
	        "n", "o", "p", "r", "t", "v", "y", "z", "å"],
    IsSuffixS = lists:suffix("s", R1),
    if
	IsSuffixS ->
	    BeforeS = string:substr(Word, string:len(Word) - 1, 1),
	    ValidSEnding = lists:any(fun(X) -> BeforeS == X end, SEndings),
	    if
		ValidSEnding == true ->
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
    %% Search for one of (gd, dt, gt, kt) suffixes in R1, and if found
    %% delete the last letter
    Suffixes = ["gd", "dt", "gt", "kt"],
    IsSuffixFound = lists:any(fun(X) -> lists:suffix(X, R1) end, Suffixes),
    if
	IsSuffixFound == true ->
	    {ok, removelast(Word), removelast(R1)};
	true ->
	    {ok, Word, R1}
    end.


step3(Word, R1) ->
    {ok, WordIgst, R1Igst} = step3igst(Word, R1),
    {ok, Word3b, R13b} = step3loest(WordIgst, R1Igst),
    step3a(Word3b, R13b).

step3igst(Word, R1) ->
    %% If the R1/word ends igst, remove the final st.
    EndsIgst = lists:suffix("igst", R1),
    if
	EndsIgst == true ->
	    {ok, removelast(removelast(Word)), removelast(removelast(R1))};
	true ->
	    {ok, Word, R1}
    end.
    
step3loest(Word, R1) ->
    %% Step 3 b suffix løst replace with løs
    Endsloest = lists:suffix("løst", R1),
    if
	Endsloest == true ->
	    {ok, removelast(Word), removelast(R1)};
	true ->
	    {ok, Word, R1}
    end.

step3a(Word, R1) ->
    %% Search for the longest among the following suffixes in R1, 
    %% and delate and repeat step 2
    Suffixes = ["elig", "els", "lig", "ig"],
    case step1asuffix(Suffixes, R1) of
	{ok, MatchedSuffix} ->
	    LenMatchSuffix = string:len(MatchedSuffix),
	    LenWord = string:len(Word),
	    LenR1 = string:len(R1),
	    NewWord = string:substr(Word, 1, LenWord - LenMatchSuffix),
	    NewR1 = string:substr(R1, 1, LenR1 - LenMatchSuffix),
	    step2(NewWord, NewR1);
	{nomatch} ->
	    {ok, Word, R1}
    end.

step4(Word, R1) ->
    %% If the word ends with double consonant in R1, 
    %% remove one of the consonants
    DoubleConsonant = ["bb", "cc", "dd", "ff", "gg", "hh", "jj", "kk", 
		       "ll", "mm", "nn", "pp", "qq", "rr", "ss", "tt", 
		       "vv", "ww", "xx", "zz"],
    WordLength = string:len(Word),
    HasDoubleConsonant = lists:any(fun(X) -> lists:suffix(X, R1) end, DoubleConsonant),
    if
	(HasDoubleConsonant == true) and (3 < WordLength) ->
	    {ok, removelast(Word), removelast(R1)};
	true ->
	    {ok, Word, R1}
    end.


%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).
regions_test() ->
    %% Tests from: http://snowball.tartarus.org/texts/r1r2.html
    ?assertMatch({ok, "iful", "ul"}, regions("beautiful")),
    ?assertMatch({ok, "y", []}, regions("beauty")),
    ?assertMatch({ok, [], []}, regions("beau")),
    %% Next one will be hit by the R1 3 char adjust
    ?assertMatch({ok, "madversion", "adversion"}, regions("animadversion")),
    ?assertMatch({ok, "kled", []}, regions("sprinkled")),
    ?assertMatch({ok, "harist", "ist"}, regions("eucharist")).

regions_bestemmelse_test() ->
    ?assertMatch({ok, "temmelse", "melse"}, regions("bestemmelse")).

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
    ?assertMatch({ok, "good", "d"}, step3("goodigst","digst")).


step3igst_test() ->
    ?assertMatch({ok, "goodigt", "digt"}, step3igst("goodigt","digt")),
    ?assertMatch({ok, "goodig", "dig"}, step3igst("goodigst","digst")).

step3loest_test() ->
    ?assertMatch({ok, "målløs", "løs"}, step3loest("målløst","løst")),
    ?assertMatch({ok, "modløs", "løs"}, step3loest("modløs","løs")).
    
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

-endif.

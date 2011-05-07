%%%-------------------------------------------------------------------
%%% File    : ec_stemming.erl
%%% Author  : James Lindstorff <james@ind-x301>
%%% Description : Module containing support function for stemming.
%%%
%%% Created :  6 May 2011 by James Lindstorff <james@ind-x301>
%%%-------------------------------------------------------------------
-module(ec_stemming).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% API
-export([stem/2]).
-export([adjustR1/2, match_suffix/2, r1/2, remove_suffix/3, remove_last/1,
	 remove_trailing_s_ending/3, remove_last_on_suffix_match/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: stem(language, term) -> StemmedWord
%% Description: Stem a word for a given language.
%% Currently language can be the atoms danish or swedish
%% Unknown languages are handled by passing the Term directly through.
%%--------------------------------------------------------------------
stem(danish, Term) ->
    ec_danish_stemmer:stem(Term);
stem(swedish, Term) ->
    ec_swedish_stemmer:stem(Term);
stem(_, Term) ->
    Term.

%%--------------------------------------------------------------------
%% Function: adjustR1(Word, R1) -> adjusted R1
%% Description: Region before R1 needs to be at least 3 characters
%% so either R1 of a fitting portion of Word is returned as the
%% R1 to be used further in the stemming.
%%--------------------------------------------------------------------
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


%%--------------------------------------------------------------------
%% Function: match_suffix(SuffixList, StringToMatch) -> {ok, MatchedSuffix} |
%%                                                     {nomatch}
%% Description: Check a string for a suffix match from the list
%% of suffixes. The first matched suffix is returned and if nothing
%% matched nomatch is returned.
%%--------------------------------------------------------------------
match_suffix([], _R1) ->
    {nomatch};

match_suffix([H | T], R1) ->
    Match = lists:suffix(H, R1),
    if
	Match =:= true -> 
	    {ok, H};
	true ->
	    match_suffix(T, R1)
    end.

%%--------------------------------------------------------------------
%% Function: r1(Word, Vowels) - R1
%% Description: Pull R1 form the word using the scandinavian
%% rules for R1 with a vowel list.
%% R1 is the region after the first non-vowel following a vowel,
%% or is the null region at the end of the word if there is no 
%% such non-vowel.
%%--------------------------------------------------------------------
r1([], _Vowels) ->
    [];
r1(Word, Vowels) ->
    [H | T] = Word,
    r1(T, H, Vowels).
r1([], _Last, _Vowels) ->
    [];
r1([H | T], Last, Vowels) ->
    PrevVowel = is_vowel(Last, Vowels),
    CurrVowel = is_vowel(H, Vowels),
    if
	(PrevVowel =:= true) and (CurrVowel =:= false) ->
	    T;
	true ->
	    r1(T, H, Vowels)
    end.

%%--------------------------------------------------------------------
%% Function: is_vowel(Character, Vowels) -> boolean()
%% Description: Return true is the character is in the vowel list.
%%--------------------------------------------------------------------
is_vowel(Character, Vowels) ->
        lists:member(Character, Vowels).

%%--------------------------------------------------------------------
%% Function: remove_suffix(Word, R1, Suffix) -> {ok, NewWord, NewR1}
%% Description: Remove the suffix from the Word and R1 strings.
%% Note that the suffix is removed based on the suffix length,
%% so a confirmed suffix match is a precondition for using this
%% function.
%%--------------------------------------------------------------------
remove_suffix(Word, R1, Suffix) ->
    %% Remove the Suffix from Word and R1
    LenSuffix = string:len(Suffix),
    LenWord = string:len(Word),
    LenR1 = string:len(R1),
    NewWord = string:substr(Word, 1, LenWord - LenSuffix),
    NewR1 = string:substr(R1, 1, LenR1 - LenSuffix),
    {ok, NewWord, NewR1}.


%%--------------------------------------------------------------------
%% Function: remove_last(Word) -> Word minus last character
%% Description: Remove the last character form the word.
%%--------------------------------------------------------------------
remove_last(Word) ->
    lists:sublist(Word, length(Word) - 1).

%%--------------------------------------------------------------------
%% Function: remove_trailing_s_ending(Word, R1, SEndings) 
%% Description: Remove trailing S if preceded by a valid s-ending.
%% This is generally step1b in scandinavian snowball stemmers.
%%--------------------------------------------------------------------
remove_trailing_s_ending(Word, R1, SEndings) ->
    IsSuffixS = lists:suffix("s", R1),
    if
	IsSuffixS ->
	    BeforeS = string:substr(Word, string:len(Word) - 1, 1),
	    ValidSEnding = lists:any(fun(X) -> BeforeS =:= X end, SEndings),
	    if
		ValidSEnding =:= true ->
		    {ok, ec_stemming:remove_last(Word), ec_stemming:remove_last(R1)};
		true ->
		    {ok, Word, R1}
	    end;
	true ->
	    {ok, Word, R1}	    
    end.

%%--------------------------------------------------------------------
%% Function: remove_last_on_suffix_match(Word, R1, SuffixList)
%% Description: Remove the last character from Word and R1 if
%% one of the elements in the SuffixList matches the R1 suffix.
%% This is generally step2 in scandinavian snowball stemmers.
%%--------------------------------------------------------------------
remove_last_on_suffix_match(Word, R1, SuffixList) ->
    IsSuffixFound = lists:any(fun(X) -> lists:suffix(X, R1) end, SuffixList),
    if
	IsSuffixFound =:= true ->
	    {ok, remove_last(Word), remove_last(R1)};
	true ->
	    {ok, Word, R1}
    end.

%%====================================================================
%% Internal functions
%%====================================================================



%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).

adjustR1_test() ->
    ?assertEqual("urn", adjustR1("return", "turn")),
    ?assertEqual("return", adjustR1("return", "return")),
    ?assertEqual("urn", adjustR1("return", "urn")).

match_suffix_test() ->
    L = ["ling", "ing", "es"],
    ?assertMatch({ok, "ing"}, match_suffix(L, "fishing")),
    ?assertMatch({nomatch}, match_suffix(L, "fish")).

remove_suffix_test() ->
    ?assertMatch({ok, "fisk", "k"}, remove_suffix("fiskdel", "kdel", "del")).


remove_last_test() ->
    ?assertEqual("fis", remove_last("fisk")),
    ?assertEqual("", remove_last("k")).


remove_trailing_s_ending_test() ->
    %% Swedish s-endings
    SE = ["b", "c", "d", "f", "g", "h", "j", "k", "l",
	  "m", "n", "o", "p", "r", "t", "v", "y"],
    ?assertMatch({ok, "ddws", "ddws"}, remove_trailing_s_ending("ddws","ddws", SE)),
    ?assertMatch({ok, "ddl", "l"}, remove_trailing_s_ending("ddls","ls", SE)),
    ?assertMatch({ok, "bestemmel", "temmel"}, remove_trailing_s_ending("bestemmels","temmels", SE)).

remove_last_on_suffix_match_test() ->
    L = ["gd", "dt", "gt", "kt"],
    ?assertMatch({ok, "frisk", "frisk"}, remove_last_on_suffix_match("friskt","friskt", L)),
    ?assertMatch({ok, "frisk", "sk"}, remove_last_on_suffix_match("frisk","sk", L)),
    ?assertMatch({ok, "goog", "g"}, remove_last_on_suffix_match("googd","gd", L)),
    ?assertMatch({ok, "goog", "g"}, remove_last_on_suffix_match("googt","gt", L)),
    ?assertMatch({ok, "good", "d"}, remove_last_on_suffix_match("goodt","dt", L)).



is_vowel_test() ->
    %% Swedish vowel check
    SwedishVowels = "aeiouy\x{E4}\x{E5}\x{F6}",
    ?assert(is_vowel($a, SwedishVowels)),
    ?assert(is_vowel($e, SwedishVowels)),
    ?assert(is_vowel($i, SwedishVowels)),
    ?assert(is_vowel($o, SwedishVowels)),
    ?assert(is_vowel($u, SwedishVowels)),
    ?assert(is_vowel($y, SwedishVowels)),
    ?assert(is_vowel(16#E4, SwedishVowels)), % ä
    ?assert(is_vowel(16#E5, SwedishVowels)), % å  
    ?assert(is_vowel(16#F6, SwedishVowels)), % ö
    ?assertNot(is_vowel($x, SwedishVowels)),
    ?assertNot(is_vowel($b, SwedishVowels)),

    %% Danish vowel check
    DanishVowels = "aeiouy\x{E6}\x{F8}\x{E5}",
    ?assert(is_vowel($a, DanishVowels)),
    ?assert(is_vowel($e, DanishVowels)),
    ?assert(is_vowel($i, DanishVowels)),
    ?assert(is_vowel($o, DanishVowels)),
    ?assert(is_vowel($u, DanishVowels)),
    ?assert(is_vowel($y, DanishVowels)),
    ?assert(is_vowel(16#E6, DanishVowels)),
    ?assert(is_vowel(16#F8, DanishVowels)),
    ?assert(is_vowel(16#E5, DanishVowels)),
    ?assertNot(is_vowel($x, DanishVowels)),
    ?assertNot(is_vowel($b, DanishVowels)).
    
-endif.

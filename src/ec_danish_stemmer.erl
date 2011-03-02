%%%-------------------------------------------------------------------
%%% File    : ec_danish_stemmer.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Implements a Danish stemmer based on the snowball
%%% See:  http://snowball.tartarus.org/algorithms/danish/stemmer.html
%%%
%%% Created :  1 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_danish_stemmer).

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
    regions(LowerWord).

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
    R2 = r1(R1),
    {ok, R1, R2}.

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

%% R2 is not used by the danish stemmer
%%r2(_Word) ->
%%    "".



is_vowel(Character) ->
    %% Classifies the character as being a vowel or not a vowel 
    %% where not a vowel is a consonant in this context
    V = lists:member(Character, "aeiouyæøå"),
    V.


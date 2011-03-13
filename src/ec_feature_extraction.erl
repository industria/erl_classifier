%%%-------------------------------------------------------------------
%%% File    : ec_feature_extraction.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Implements feature extractions.
%%%
%%% Created : 13 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_feature_extraction).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([features/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: features(Language, Document) -> Frequency distribution
%% Description: Extract features from a Document returning them
%% in the form of a frequency distribution.
%% Language is an atom representing the language, 
%% only danish is supported for this version.
%%--------------------------------------------------------------------
features(danish, Document) when is_binary(Document) ->
    DP = ec_document_processing:remove_punctuation(Document),
    DN = ec_document_processing:normalize_whitespace(DP),
    Terms = ec_tokenizer:word_tokenize(DN),
    FD = ec_frequency_distribution:create(Terms),
    FD.

%%====================================================================
%% Internal functions
%%====================================================================


%%====================================================================
%% Testing
%%====================================================================

-ifdef(TEST).

features_swedish_test() ->
    ?assertError(function_clause, features(swedish, << >>)).

features_danish_test() ->
    {ok, Document} = file:read_file("test/documents/polsport.txt"),
    FD = features(danish, Document).
%%    SFD = lists:keysort(1, FD),
%%    ?assertEqual([], SFD).


%% features_danish_test() ->
%%     {ok, Document} = file:read_file("test/documents/polsport.txt"),
%%     {ok, Words} = file:read_file("test/documents/polsport_words.txt"),
%%     WordListSplit = re:split(Words, "\n"),
%%     WordList = [ X || X <- WordListSplit, 0 < byte_size(X)],
%%     NoPunctuation = ec_document_processing:remove_punctuation(Document),
%%     Normalized = ec_document_processing:normalize_whitespace(NoPunctuation),
%%     Terms = word_tokenize(Normalized),
%%     WordsSorted = lists:sort(WordList),
%%     TermsSorted = lists:sort(Terms),
%%     ?assertEqual(WordsSorted, TermsSorted).
%%     ?assert(false).
-endif.

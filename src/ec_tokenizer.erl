%%%-------------------------------------------------------------------
%%% File    : ec_tokenizer.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Implement tokenizers
%%%
%%% Created : 13 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_tokenizer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([word_tokenize/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: word_tokenize(Document) -> [Binary]
%% Description: Tokenizes word using space as separator
%%--------------------------------------------------------------------
word_tokenize(Document) when is_binary(Document) ->
    word_t_termlist(Document, [], << >>).


%%====================================================================
%% Internal functions
%%====================================================================
word_t_termlist(<< 16#20, T/binary>>, Terms, << >>) ->
    word_t_termlist(T, Terms, << >>);

word_t_termlist(<< 16#20, T/binary>>, Terms, Acc) ->
    NewTerms = [Acc | Terms],
    word_t_termlist(T, NewTerms, << >>);

word_t_termlist(<< H, T/binary>>, Terms, Acc) ->
    NewAcc = << Acc/binary, H >>,
    word_t_termlist(T, Terms, NewAcc);

word_t_termlist(<< >>, Terms, << >>) ->
    Terms;

word_t_termlist(<< >>, Terms, Acc) ->
    [Acc | Terms].

%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).

word_tokenize_simple_test() ->
    Terms = word_tokenize(<<"This is a list of terms"/utf8>>),
    ETerms = [<<"This"/utf8>>, <<"is"/utf8>>, <<"a"/utf8>>,
	      <<"list"/utf8>>, <<"of"/utf8>>, <<"terms"/utf8>>],
    ?assertEqual(ETerms, lists:reverse(Terms)).

word_tokenize_extra_spc_start_test() ->
    Terms = word_tokenize(<<"    This is a list of terms"/utf8>>),
    ETerms = [<<"This"/utf8>>, <<"is"/utf8>>, <<"a"/utf8>>,
	      <<"list"/utf8>>, <<"of"/utf8>>, <<"terms"/utf8>>],
    ?assertEqual(ETerms, lists:reverse(Terms)).

word_tokenize_extra_spc_internal_test() ->
    Terms = word_tokenize(<<"This is a list  of terms"/utf8>>),
    ETerms = [<<"This"/utf8>>, <<"is"/utf8>>, <<"a"/utf8>>,
	      <<"list"/utf8>>, <<"of"/utf8>>, <<"terms"/utf8>>],
    ?assertEqual(ETerms, lists:reverse(Terms)).

word_tokenize_extra_spc_end_test() ->
    Terms = word_tokenize(<<"This is a list of terms  "/utf8>>),
    ETerms = [<<"This"/utf8>>, <<"is"/utf8>>, <<"a"/utf8>>,
	      <<"list"/utf8>>, <<"of"/utf8>>, <<"terms"/utf8>>],
    ?assertEqual(ETerms, lists:reverse(Terms)).


word_tokenize_polsport_test() ->
    {ok, Document} = file:read_file("test/documents/polsport.txt"),
    {ok, Words} = file:read_file("test/documents/polsport_words.txt"),
    WordListSplit = re:split(Words, "\n"),
    WordList = [ X || X <- WordListSplit, 0 < byte_size(X)],
    NoPunctuation = ec_document_processing:remove_punctuation(Document),
    Normalized = ec_document_processing:normalize_whitespace(NoPunctuation),
    Terms = word_tokenize(Normalized),
    WordsSorted = lists:sort(WordList),
    TermsSorted = lists:sort(Terms),
    ?assertEqual(WordsSorted, TermsSorted).
    
-endif.

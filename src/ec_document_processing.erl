%%%-------------------------------------------------------------------
%%% File    : ec_document_processing.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Document processing functions, support library.
%%%
%%% Created : 10 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_document_processing).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([normalize/1, 
	 remove_punctuation/1, 
	 normalize_whitespace/1, 
	 to_lowercase/1,
	 term_length_in_range/3
	]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: normalize(Document) -> Document
%% Description: Basically runs the document procssing chain 
%% remove_punctuation -> normalize_whitespace -> to_lowercase
%% but implements it as a one pass, instead of three
%%--------------------------------------------------------------------
normalize(Document) when is_binary(Document) ->
    << 
       <<(
	   string:to_lower(
	     normalize_char(X)
	    ) 
	 )/utf8>> 
       || <<X/utf8>> <= Document 
    >>.
%%--------------------------------------------------------------------
%% Function: remove_punctuation(Document) -> Document
%% Description: Remove the punctuation from the input Document
%% Punctuation is defined as: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
%%--------------------------------------------------------------------
remove_punctuation(Document) when is_binary(Document) ->
    << <<X>> || <<X>> <= Document, not punctuation(<<X>>) >>.

%%--------------------------------------------------------------------
%% Function: punctuation_to_space(Document) -> Document
%% Description: Replace punctuation with space in Document.
%% Punctuation is defined as: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
%%--------------------------------------------------------------------
punctuation_to_space(Document) when is_binary(Document) ->
    << 
       <<(
	   normalize_punctuation_character(X)
	 )/utf8>> 
       || <<X/utf8>> <= Document 
    >>.

%%--------------------------------------------------------------------
%% Function: normalize_whitespace(Document) -> Document
%% Description: Changes whitespace control characters to a space
%%--------------------------------------------------------------------
normalize_whitespace(Document) when is_binary(Document) ->
    << <<(normalize_whitespace_character(X))>> || <<X>> <= Document >>.

%%--------------------------------------------------------------------
%% Function: to_lowercase(Document) -> Document
%% Description: Changes the document to lowercase.
%%--------------------------------------------------------------------
to_lowercase(Document) when is_binary(Document) ->    
    << <<(string:to_lower(X))/utf8>> || <<X/utf8>> <= Document >>.

%%--------------------------------------------------------------------
%% Function: term_length_in_range(Term, Min, Max) -> boolean()
%% Description: Check is the length of the given term is within the
%% range given by minimum and maximum arguments.
%%--------------------------------------------------------------------
term_length_in_range(Term, Min, Max) ->
    Length = length(unicode:characters_to_list(Term, utf8)),
    ((Min =< Length) and (Length =< Max)).

%%====================================================================
%% Internal functions
%%====================================================================
punctuation(X) when is_binary(X)->
    %% Punctuation defined as  !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
    ((<<16#21>> =< X andalso X =< <<16#2F>>) orelse
     (<<16#3A>> =< X andalso X =< <<16#40>>) orelse
     (<<16#5B>> =< X andalso X =< <<16#60>>) orelse
     (<<16#7B>> =< X andalso X =< <<16#7E>>)).

normalize_punctuation_character(X) when ((16#21 =< X andalso X =< 16#2F) orelse
					 (16#3A =< X andalso X =< 16#40) orelse
					 (16#5B =< X andalso X =< 16#60) orelse
					 (16#7B =< X andalso X =< 16#7E)) ->
    16#20;
normalize_punctuation_character(X) ->
    X.

normalize_whitespace_character(X) when (16#09 =< X andalso X =< 16#0D) ->
    16#20;
normalize_whitespace_character(X) ->
    X.


normalize_char(X) when ((16#21 =< X andalso X =< 16#2F) orelse
					 (16#3A =< X andalso X =< 16#40) orelse
					 (16#5B =< X andalso X =< 16#60) orelse
					 (16#7B =< X andalso X =< 16#7E)) ->
    16#20;
normalize_char(X) when (16#09 =< X andalso X =< 16#0D) ->
    16#20;
normalize_char(X) ->
    X.

%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).

normalize_test() ->
    Doc = <<"Remove, the.\tpunctuation TOLOWER \x{C6}bler"/utf8>>,
    Expected = <<"remove  the  punctuation tolower \x{E6}bler"/utf8>>,
    ?assertEqual(Expected, normalize(Doc)).


remove_punctuation_test() ->
    Doc = <<"Remove, the. punctuation"/utf8>>,
    Expected = <<"Remove the punctuation"/utf8>>,
    ?assertEqual(Expected, remove_punctuation(Doc)).

punctuation_to_space_test() ->
    Doc = <<"Remove, the. punctuation"/utf8>>,
    Expected = <<"Remove  the  punctuation"/utf8>>,
    ?assertEqual(Expected, punctuation_to_space(Doc)).

normalize_whitespace_test() ->
    Document = <<"fish \tcat\n"/utf8>>,
    Normalized = normalize_whitespace(Document),
    ?assertEqual(<<"fish  cat "/utf8>>, Normalized).

punctuation_test() ->
    ?assert(punctuation(<<"!"/utf8>>)),
    ?assertNot(punctuation(<<"a"/utf8>>)).

to_lowercase_test() ->
    ?assertEqual(<<"\x{E6}bler"/utf8>>, to_lowercase(<<"\x{C6}bler"/utf8>>)).


term_length_in_range_test() ->
    T = <<"fishy"/utf8>>,
    ?assert(term_length_in_range(T, 1, 5)),
    ?assertNot(term_length_in_range(T, 1, 3)),
    ?assertNot(term_length_in_range(T, 10, 25)).

-endif.

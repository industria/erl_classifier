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
-export([remove_punctuation/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: remove_punctuation
%% Description: Remove the punctuation from the input Document
%% Punctuation is defined as: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
%%--------------------------------------------------------------------
remove_punctuation(Document) when is_binary(Document) ->
    << <<X>> || <<X>> <= Document, (not is_punctuation(<<X>>)) >>.
    
%%====================================================================
%% Internal functions
%%====================================================================
is_punctuation(X) ->
    %% Punctuation defined as  !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
    ((<<16#21>> =< X andalso X =< <<16#2F>>) orelse
     (<<16#3A>> =< X andalso X =< <<16#40>>) orelse
     (<<16#5B>> =< X andalso X =< <<16#60>>) orelse
     (<<16#7B>> =< X andalso X =< <<16#7E>>)).

    

%%====================================================================
%% Testing
%%====================================================================
-ifdef(TEST).

remove_punctuation_test() ->
    Doc = <<"Remove, the. punctuation"/utf8>>,
    Expected = <<"Remove the punctuation"/utf8>>,
    ?assertEqual(Expected, remove_punctuation(Doc)).

is_punctuation_test() ->
    ?assert(is_punctuation(<<"!"/utf8>>)),
    ?assertNot(is_punctuation(<<"a"/utf8>>)).

-endif.

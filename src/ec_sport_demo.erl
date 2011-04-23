%%%-------------------------------------------------------------------
%%% File    : ec_sport_demo.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Demo of classifier using the politiken.dk sport
%%% documents in test/documents
%%%
%%% Created :  3 Apr 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_sport_demo).

%% API
-export([train/0]).

-define(DOCPATH, "test/documents").
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Functiobn: 
%% Description:
%%--------------------------------------------------------------------
train() ->
    timer:start(),
    {ok, Files} = file:list_dir(?DOCPATH),
    lists:foreach(fun(File) ->
			 [Prefix | Number] = string:tokens(File, "_"),
			 Usable = class_start(Prefix),
			 if 
			     Usable =:= true ->
				 Class = list_to_atom(Prefix),
				 DocFile = string:join([?DOCPATH, File], "/"),
				 io:fwrite("~p ~w ~p~n", [Prefix, Number, DocFile]),
				 {ok, Doc} = file:read_file(DocFile),
				 R = ec_coach:train(Class, Doc),
				 io:fwrite("~p ~n", [R]),
				 timer:sleep(250);
			     true -> 0
			 end
		 end, Files).


%%====================================================================
%% Internal functions
%%====================================================================
class_start(P) ->
    ("f1" =:= P) orelse ("fodbold" =:= P) orelse ("cykling" =:= P).
    

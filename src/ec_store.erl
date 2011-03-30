%%%-------------------------------------------------------------------
%%% File    : ec_store.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Handles storage for the classifier.
%%%
%%% Created : 30 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_store).

%% API
-export([init_tables/0, delete_tables/0, new_id/1]).

-record(ids, {id_name, id}).

-record(terms, {term, term_id}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
init_tables() ->
    create_tables(),
    init_ids_table().

delete_tables() ->
    mnesia:delete_table(ids),
    mnesia:delete_table(terms).

new_id(Idname) ->
    Trans = fun() ->
	case mnesia:wread({ids, Idname}) of
	    [ID] ->
		NewId = ID#ids.id + 1,
		UpdatedID = ID#ids{id = NewId},
		mnesia:write(UpdatedID),
		NewId;
	    _ ->
		mnesia:abort("id_name not found")
	end
    end,
    mnesia:transaction(Trans).

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================
create_tables() ->
    mnesia:create_table(ids, [{type, set},
			      {disc_copies, [node()]},
			      {attributes, record_info(fields, ids)}
			     ]),
    mnesia:create_table(terms, [{type, set},
				{disc_copies, [node()]},
				{attributes, record_info(fields, terms)}
			       ]),
    mnesia:wait_for_tables([ids, terms], 60000).

init_ids_table() ->
    Id_terms = #ids{id_name=terms, id=0},
    mnesia:transaction(fun() ->
			       mnesia:write(Id_terms)
		       end).
			   

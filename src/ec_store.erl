%%%-------------------------------------------------------------------
%%% File    : ec_store.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Handles storage for the classifier.
%%%
%%% Created : 30 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_store).

%% API
-export([init_tables/0, delete_tables/0, term/1, 
	 update_term_class_frequency/3]).

-record(ids, {table, id}).

-record(terms, {term, term_id}).

-record(term_class_frequency, {term_id, class, count}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
init_tables() ->
    create_tables().

delete_tables() ->
    mnesia:delete_table(ids),
    mnesia:delete_table(terms),
    mnesia:delete_table(term_class_frequency).

%%--------------------------------------------------------------------
%% Function: term(Term) -> TermId 
%% Description: Return id for the Term, 
%%              the Term is added ti terms table if it doesn't exist
%%--------------------------------------------------------------------
term(Term) ->
    case mnesia:dirty_read({terms, Term}) of
	[T] ->
	    T#terms.term_id;
	_ ->
	    %% Create a new Term entry
	    Trans = fun() ->
		TermId = mnesia:dirty_update_counter({ids, term_id}, 1),
		TermEntry = #terms{term = Term, term_id = TermId},
		mnesia:write(TermEntry),
		TermId
	    end,
	    {atomic, Id} = mnesia:transaction(Trans),
	    Id
    end.

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
update_term_class_frequency(TermId, Class, Count) ->
    1.
%%====================================================================
%% Internal functions
%%====================================================================
create_tables() ->
    mnesia:create_table(ids, 
    			[{type, set},
    			 {disc_copies, [node()]},
    			 {attributes, record_info(fields, ids)}
    			]),
    mnesia:create_table(terms, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, terms)}
			]),
    mnesia:create_table(term_class_frequency, 
			[{type, bag},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, term_class_frequency)}
			]),

    mnesia:wait_for_tables([terms, term_class_frequency], 60000).

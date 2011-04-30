%%%-------------------------------------------------------------------
%%% File    : ec_store.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Handles storage for the classifier.
%%%
%%% Created : 30 Mar 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_store).

%% API
-export([init_tables/0, change_to_disc_copies/0, delete_tables/0]).

%% Final exports are collected below in one export of each area
-export([stopword_update/0, is_stopword/2]).
-export([term_id/1, new_term/1]).
-export([doc_freq_update/2]).
-export([term_freq_update/3]).
-export([doc_freq/1]).
-export([term_freq/2]).
-export([update_class_vocabulary/3, vocabulary_size/0, vocabulary_size/1]).
-record(ids, {table, id}).

-record(terms, {term, term_id}).

%% Contain document counts for a class
%% match_count + complement_count is the total number of documents
%% for a class.
-record(doc_class_freq, {class, match_count, complement_count}).

%% Contain the match count and complement count for a specific
%% class term combination.
-record(term_class_freq, {term_class, match_count, complement_count}).

%% Contain match count and complement count for a class representing
%% the class vocabulary size.
-record(class_vocabulary, {class, match_count, complement_count}).

%% Mnesia tables must have at least one extra attribute in addition
%% to the key, hence the dummy attribute in the record definition.
-record(stopwords, {language_term, dummy = unused}).

%%====================================================================
%% API
%%====================================================================


%%--------------------------------------------------------------------
%% Function: is_stopword(Language, Term) -> boolean
%% Description: Check for the {Language, Term} key indicating 
%% that the term is defined as a stopword for the Language.
%% Note: Applications should use the API in ec_stopwords this module
%% should be considered internal.
%%--------------------------------------------------------------------
is_stopword(Language, Term) ->
    Key =  {Language, Term},
    case mnesia:dirty_read(stopwords, Key) of
	[_] ->
	    true;
	_ ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function: stopword_update()
%% Description: Update the stopwords table with the stopwords found
%% in stopword directory.
%%--------------------------------------------------------------------
stopword_update() ->
    %% [ { lang, [words] } ]
    StopwordLists = ec_stopwords:stopword_lists(),
    lists:foreach(
      fun({Language, Terms}) ->
	      lists:foreach(
		fun(Term) ->
			R = #stopwords{language_term={Language, Term}},
			mnesia:dirty_write(stopwords, R)
		end, Terms)
      end, StopwordLists),
    ok.


%%--------------------------------------------------------------------
%% Function: term_id(Term) -> {ok, TermId} | unknown
%% Description: Lookup the id of a term.
%%--------------------------------------------------------------------
term_id(Term) when is_binary(Term) ->
    case mnesia:dirty_read({terms, Term}) of
	[Terms] -> 
	    {ok, Terms#terms.term_id};
	_ -> 
	    unknown
    end.

%%--------------------------------------------------------------------
%% Function: new_term(Term) -> {ok, TermId}
%% Description: Add a new term to the vocabulary.
%%--------------------------------------------------------------------
new_term(Term) when is_binary(Term) ->
    TermId = mnesia:dirty_update_counter({ids, term_id}, 1),
    TermRecord = #terms{term = Term, term_id = TermId},
    mnesia:dirty_write(TermRecord),
    {ok, TermId}.

%%--------------------------------------------------------------------
%% Function: doc_freq_update(Class, Match) -> ok
%% Description: Update the document frequency for a given class.
%% Match is a boolean indicating class match or class complement.
%%--------------------------------------------------------------------
doc_freq_update(Class, Match) ->
    R = case mnesia:dirty_read(doc_class_freq, Class) of
	    [DCF] ->
		update_doc_freq(DCF, Match);
	    _ ->
		new_doc_freq(Class, Match)
    end,
    mnesia:dirty_write(doc_class_freq, R).


new_doc_freq(Class, true) ->
    #doc_class_freq{class = Class, match_count = 1, complement_count = 0};
new_doc_freq(Class, false) ->
    #doc_class_freq{class = Class, match_count = 0, complement_count = 1}.

update_doc_freq(DF, true) ->
    C = DF#doc_class_freq.match_count + 1,
    DF#doc_class_freq{match_count = C};
update_doc_freq(DF, false) ->
    C = DF#doc_class_freq.complement_count + 1,
    DF#doc_class_freq{complement_count = C}.


%%--------------------------------------------------------------------
%% Function: Term_freq_update(Class, Match, FreqDist) -> ok
%% Description: Update the term frequency for a given class.
%% Match is a boolean indicating class match or class complement.
%%--------------------------------------------------------------------
term_freq_update(Class, Match, FreqDist) ->
    lists:foreach(
      fun({TermId, Count}) ->
	      Key = {Class, TermId},
	      R = case mnesia:dirty_read(term_class_freq, Key) of
		      [TCF] ->
			  update_term_freq(TCF, Count, Match);
		      _ ->
			  new_term_freq(Key, Count, Match)
		  end,
	      mnesia:dirty_write(term_class_freq, R)
      end, FreqDist),
    ok.

new_term_freq(ClassTerm, Count, true) ->
    #term_class_freq{term_class = ClassTerm,
		     match_count = Count,
		     complement_count = 0};
new_term_freq(ClassTerm, Count, false) ->
    #term_class_freq{term_class = ClassTerm,
		     match_count = 0,
		     complement_count = Count}.

update_term_freq(TF, Count, true) ->    
    C = TF#term_class_freq.match_count + Count,
    TF#term_class_freq{match_count = C};
update_term_freq(TF, Count, false) ->
    C = TF#term_class_freq.complement_count + Count,
    TF#term_class_freq{complement_count = C}.


%%--------------------------------------------------------------------
%% Function: doc_freq(Class) -> {ok, Match, Complement} 
%% Description: Get the document frequency for a class.
%%--------------------------------------------------------------------
doc_freq(Class) ->
    case mnesia:dirty_read(doc_class_freq, Class) of
	[Freq] ->
	    {ok, Freq#doc_class_freq.match_count, Freq#doc_class_freq.complement_count};
	_ ->
	    {ok, 0, 0}
    end.


%%--------------------------------------------------------------------
%% Function: term_freq(Class, TermId) -> {ok, Match, Complement} 
%% Description: Get the term frequency for a class.
%%--------------------------------------------------------------------
term_freq(Class, TermId) ->
    case mnesia:dirty_read(term_class_freq, {Class, TermId}) of
	[Freq] ->
	    {ok, Freq#term_class_freq.match_count, Freq#term_class_freq.complement_count};
	_ ->
	    {ok, 0, 0}
    end.

%%--------------------------------------------------------------------
%% Function: update_class_vocabulary(Class, Count, Match)
%% Description: Update the class vocabulary size. The table updated
%% is a precalculation table helping to answer the function
%% vocabulary_size/1 without scanning the term_class_freq table.
%%--------------------------------------------------------------------
update_class_vocabulary(Class, Count, Match) ->
    R = case mnesia:dirty_read(class_vocabulary, Class) of
	    [CV] ->
		update_class_voc(CV, Count, Match);
	    _ ->
		new_class_voc(Class, Count, Match)
	end,
    mnesia:dirty_write(class_vocabulary, R).
		     


new_class_voc(Class, Count, true) ->
    #class_vocabulary{class = Class, match_count = Count, complement_count = 0};
new_class_voc(Class, Count, false) ->
    #class_vocabulary{class = Class, match_count = 0, complement_count = Count}.

update_class_voc(ClassVoc, Count, true) ->
    NewCount = ClassVoc#class_vocabulary.match_count + Count, 
    ClassVoc#class_vocabulary{match_count = NewCount};
update_class_voc(ClassVoc, Count, false) ->
    NewCount = ClassVoc#class_vocabulary.complement_count + Count,
    ClassVoc#class_vocabulary{complement_count = NewCount}.

%%--------------------------------------------------------------------
%% Function: vocabulary_size(Class) -> {ok, Match, Complement} 
%% Description: Get the vocabulary size of a given class.
%% Note: This is a rather expensive way of doing this. For the actual
%% application of this is constant values unless a document is trained
%% so one way is to calculate a vocabulary size only when training is
%% activated.
%%--------------------------------------------------------------------
vocabulary_size(Class) ->
    case mnesia:dirty_read(class_vocabulary, Class) of
	[S] ->
	    {ok, S#class_vocabulary.match_count, S#class_vocabulary.complement_count};
	_ ->
	    {ok, 0, 0}
    end.

%%--------------------------------------------------------------------
%% Function: vocabulary_size() -> {ok, Count} 
%% Description: Get vocabulary size.
%% Vocabulary size is the size of the terms table because all 
%% two-class classifiers see all terms as either match or complement.
%%--------------------------------------------------------------------
vocabulary_size() ->
    S = mnesia:table_info(terms, size),
    {ok, S}.

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
init_tables() ->
    create_tables().

%% Swich the ram_copies tables to disc_copies
change_to_disc_copies() ->
    mnesia:change_table_copy_type(terms, node(), disc_copies),
    mnesia:change_table_copy_type(doc_class_freq, node(), disc_copies),
    mnesia:change_table_copy_type(class_vocabulary, node(), disc_copies),
    mnesia:change_table_copy_type(term_class_freq, node(), disc_copies).


delete_tables() ->
    mnesia:delete_table(ids),
    mnesia:delete_table(terms),
    mnesia:delete_table(doc_class_freq),
    mnesia:delete_table(class_vocabulary),
    mnesia:delete_table(term_class_freq),
    mnesia:delete_table(stopwords).


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
			 {ram_copies, [node()]},
			 {attributes, record_info(fields, terms)}
			]),
    mnesia:create_table(doc_class_freq, 
			[{type, set},
			 {ram_copies, [node()]},
			 {attributes, record_info(fields, doc_class_freq)}
			]),

    mnesia:create_table(class_vocabulary, 
			[{type, set},
			 {ram_copies, [node()]},
			 {attributes, record_info(fields, class_vocabulary)}
			]),

    mnesia:create_table(term_class_freq, 
			[{type, set},
			 {ram_copies, [node()]},
			 {attributes, record_info(fields, term_class_freq)}
			]),
    mnesia:create_table(stopwords, 
			[{type, set},
			 {disc_copies, [node()]},
			 {attributes, record_info(fields, stopwords)}
			]),

    mnesia:wait_for_tables([ids, 
			    terms, 
			    doc_class_freq,
			    class_vocabulary,
			    term_class_freq, 
			    stopwords], 60000).



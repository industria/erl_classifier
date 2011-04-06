%%%-------------------------------------------------------------------
%%% File    : ec_classifier.erl
%%% Author  : James Lindstorff <james@ind-w510>
%%% Description : Classifier - classify documents
%%%
%%% Created :  5 Apr 2011 by James Lindstorff <james@ind-w510>
%%%-------------------------------------------------------------------
-module(ec_classifier).

-behaviour(gen_server).

%% API
-export([start_link/0, class/1, classify/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% Function: class(Document) -> atom() 
%% Description: Get the class of the document.
%%--------------------------------------------------------------------
class(Document) when is_binary(Document) ->
    gen_server:call(?SERVER, {class, Document}).


%%--------------------------------------------------------------------
%% Function: classify(Document) -> [ {atom(), measure} ] 
%% Description: Get a list of classes and measure for the document.
%%--------------------------------------------------------------------
classify(Document) when is_binary(Document) ->
    gen_server:call(?SERVER, {classify, Document}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({classify, Document}, _From, State) ->
    FD = ec_feature_extraction:features(danish, Document),
    Classes = ec_store:classes(),
    B = length(Classes), %% number of classes
    Ndocs = ec_store:ndocuments(),
    CP = lists:foldl(fun(Class, AccIn) ->
			     Nc = ec_store:ndocuments_in_class(Class),
			     Pc = Nc / Ndocs,
			     %% Term_in_class / Term_in_all_classes
			     Ptc = lists:foldl(fun({Term, TermCount}, TAccIn) ->
						       TermId = ec_store:term_id(Term),
						       Tct = ec_store:term_class_frequency(TermId, Class),
						       Tctm = ec_store:term_frequency(TermId),
						       Pcd = (Tct + 1) / (Tctm + B),
						       %% The pow is used because the document
						       %% is represented by a frequency 
						       %% distribution and not just a vector
						       %% with duplicates
						       PcdTimes = math:pow(Pcd, TermCount),
						       TAccIn * PcdTimes
					       end, 1, FD),
			     Pcd = Pc * Ptc,
			     [{Class, Pc, Ptc, Pcd} | AccIn]
		     end, [], Classes),
    Reply = CP,
    {reply, Reply, State};

handle_call({class, Document}, _From, State) ->
    %% This is basically classify with an argmax on the list
    %% Do classify first and factor it list calculation
    %% call that and do the argmax over it.
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------



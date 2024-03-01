%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(controller). 
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-include("controller.hrl").
-include("controller.resource_discovery").

-include("specs.hrl").




%% API
-export([
%	 set_wanted_stated/
%	 delete_cluster/1, 	
%	 deploy_application/2, 
%	 remove_application/2,
%	 which_applications/0
	 % - which_applications_host(HostName) ->[{ApplicationId,Node}]
         % - which_applications_node(Node) ->[{ApplicationId,Node}]
	]).

%% OaM 
-export([
%	 wanted_state_info/0,
%	 is_wanted_state/0,
%	 get_deployment_info/0,
%	 get_deployment_info/1,
%	 missing_applications/0,
%	 running_applications/0,
%	 running_applications_on_host/0,
%	 running_applications_on_host/1,
%	 all_connected_workers/0,
%	 all_connected_controllers/0,
%	 all_connected/0,
	 
	 
%	 get_state/0
	]).

%% admin

-export([
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		mode
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Checks and returns all running applications on all nodes on the
%% cluster  
%% 
%% @end
%%--------------------------------------------------------------------
-spec which_applications() -> 
	  ApplicationsRunning::term() | {error, Error :: term()}.
which_applications() ->
    gen_server:call(?SERVER,{which_applications},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Deploy an application ApplicationId on host HostName  
%% 
%% @end
%%--------------------------------------------------------------------
-spec deploy_application(ApplicationId::string(),HostName::string()) -> 
	  {ok,WorkerNode::node()} | {error, Error :: term()}.
deploy_application(ApplicationId,HostName) ->
    gen_server:call(?SERVER,{deploy_application,ApplicationId,HostName},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Deploy an application ApplicationId on host HostName  
%% 
%% @end
%%--------------------------------------------------------------------
-spec remove_application(ApplicationId::string(),WorkerNode::node()) -> 
	  ok | {error, Error :: term()}.
remove_application(ApplicationId,WorkerNode) ->
    gen_server:call(?SERVER,{remove_application,ApplicationId,WorkerNode},infinity).
%%--------------------------------------------------------------------
%% @doc
%% creates a cluster with cookie CookieStr and starts kubelet on the hosts
%% and kubelet creates the number of workers NumWorkers  
%% It's assumed that kubelet beams are in ~/kubelet/ebin 
%% 
%% @end
%%--------------------------------------------------------------------
-spec new_cluster(ClusterId::string(),HostNameNumWorkers::term()) -> ok | 
	  {error, Error :: term()}.
new_cluster(ClusterId,HostNameNumWorkers) ->
    gen_server:call(?SERVER,{new_cluster,ClusterId,HostNameNumWorkers},infinity).

%%--------------------------------------------------------------------
%% @doc
%% stops all kubeletes and deletes their dirs 
%% 
%% @end
%%--------------------------------------------------------------------
-spec delete_cluster(ClusterId::string()) -> ok | 
	  {error, Error :: term()}.
delete_cluster(ClusterId) ->
    gen_server:call(?SERVER,{delete_cluster,ClusterId},infinity).

%%--------------------------------------------------------------------
%% @doc
%%  
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
kill()->
    gen_server:call(?SERVER, {kill},infinity).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    io:format("Environment ~p~n",[?ENVIRONMENT]),

    
%    ?LOG_NOTICE("Server started ",[?MODULE]),
    {ok, #state{
	    mode=?ENVIRONMENT
	    
	   },0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

    
handle_call({new_cluster,ClusterId,HostNameNumWorkers}, _From, State) 
  when State#state.mode==test ->
  
    Result=try lib_control:new_cluster(HostNameNumWorkers) of
	       {ok,CreateResult}->
		   {ok,CreateResult};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       error:Reason:Stacktrace->
		   {error,Reason,Stacktrace,?MODULE,?LINE};
	       throw:Reason:Stacktrace->
		   {throw,Reason,Stacktrace,?MODULE,?LINE};
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,KubeletList}->
		  io:format("CreateResult ~p~n",[{KubeletList,?MODULE,?LINE}]),
		  NewState=State,
		  {ok,KubeletList};
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  {error,ErrorEvent}
	  end,
    {reply, Reply, NewState};
 

%%--------------------------------------------------------------------



handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({stop}, State) ->
    
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info(timeout, State) ->
    io:format("timeout ~p~n",[{State#state.mode,?MODULE,?LINE}]),
    ok=initial_trade_resources(),
    
    {noreply, State};


handle_info(Info, State) ->
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
initial_trade_resources()->
    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-?LocalResourceTuples],
    [rd:add_target_resource_type(TargetType)||TargetType<-?TargetTypes],
    rd:trade_resources(),
    timer:sleep(3000),
    ok.

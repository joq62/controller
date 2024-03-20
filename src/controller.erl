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
	 reconciliate/0,
	 add_application/1,
	 delete_application/1,

	 deploy_application/1,
	 remove_application/1


%	 add_application/2, 
%	 delete_application/2,
%	 create_worker/1,
%	 delet_worker/1,
	 
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
	 read_wanted_state/0,
	 read_deployment_info/0,
	 which_applications/0,
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
	 
	 
	 read_state/0
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
	

% data
% 
% DeploymentInfo=#{application_id,
%                  app,
%		 node=>Node,
%		 nodename=>NodeName,
%		 node_id=>Id,
%		 time=>{date(),time()}
%                state=>scheduled|loaded| started|stopped|unloaded           

%              
% ApplicationInfo=#{application_id=>ApplicationId,
%		      app=>ApplicationIdApp,
%		      time=>{date(),time()}},
%                     
%  WorkerInfo=#{
%		 node=>Node,
%		 nodename=>NodeName,
%		 node_id=>Id,
%		 time=>{date(),time()}
%          
%		},


        

-record(state, {
		deployment_id,
		deployment_info
	       }).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% This a loop that starts after the interval ReconcilationInterval 
%% The loop checks what to start or stop 
%% 
%% @end
%%--------------------------------------------------------------------
-spec reconciliate() -> 
	  ok | {error, Error :: term()}.
reconciliate() ->
    gen_server:cast(?SERVER,{reconciliate}).

%%--------------------------------------------------------------------
%% @doc
%% Add application with ApplicationId to be deployed 
%% 
%% @end
%%--------------------------------------------------------------------
-spec add_application(ApplicationId::string()) -> 
	  ok | {error, Error :: term()}.
add_application(ApplicationId) ->
    gen_server:call(?SERVER,{add_application,ApplicationId},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Delete application ApplicationId from the deployment list 
%% 
%% @end
%%--------------------------------------------------------------------
-spec delete_application(ApplicationId::string()) -> 
	  ok | {error, Error :: term()}.
delete_application(ApplicationId) ->
    gen_server:call(?SERVER,{delete_application,ApplicationId},infinity).


%%--------------------------------------------------------------------
%% @doc
%%   
%% 
%% @end
%%--------------------------------------------------------------------
-spec read_deployment_info() -> 
	  DeploymentInfoList::term() | {error, Error :: term()}.
read_deployment_info() ->
    gen_server:call(?SERVER,{read_deployment_info},infinity).

%%--------------------------------------------------------------------
%% @doc
%%   
%% 
%% @end
%%--------------------------------------------------------------------
-spec read_wanted_state() -> 
	  WantedApplicationIds::term() | {error, Error :: term()}.
read_wanted_state() ->
    gen_server:call(?SERVER,{read_wanted_state},infinity).

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
-spec deploy_application(ApplicationId::string()) -> 
	  {ok,WorkerNode::node()} | {error, Error :: term()}.
deploy_application(ApplicationId) ->
    gen_server:call(?SERVER,{deploy_application,ApplicationId},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Deploy an application ApplicationId on host HostName  
%% 
%% @end
%%--------------------------------------------------------------------
-spec remove_application(ApplicationId::string()) -> 
	  ok | {error, Error :: term()}.
remove_application(ApplicationId) ->
    gen_server:call(?SERVER,{remove_application,ApplicationId},infinity).

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
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec read_state() -> 
	  State :: term().
read_state()-> 
    gen_server:call(?SERVER, {read_state},infinity).


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
     
    ?LOG_NOTICE("Server started ",[?MODULE]),
    {ok, #state{
	    deployment_id=undefined,
	    deployment_info=[]
	    
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


handle_call({add_application,ApplicationId}, _From, State) ->
    DeploymentInfo=#{
		     application_id=>ApplicationId,
		     app=>na,
		     node=>na,
		     nodename=>na,
		     node_id=>na,
		     time=>na,
		     state=>scheduled
		    },
    NewState=State#state{deployment_info=[DeploymentInfo|State#state.deployment_info]},
    Reply=ok,
    {reply, Reply, NewState};

handle_call({delete_application,ApplicationId}, _From, State) ->
    R=[DeploymentInfo||DeploymentInfo<-State#state.deployment_info,
		       ApplicationId==maps:get(application_id,DeploymentInfo)],
    Reply=case R of
	      []->
		  NewState=State,
		  {error,["Application doesnt exists ",ApplicationId]};
	      [DeploymentInfo|_]->
		  UpdatedDeploymentInfo=maps:put(state,delete,DeploymentInfo),
		  NewState=State#state{deployment_info=[UpdatedDeploymentInfo|lists:delete(DeploymentInfo,State#state.deployment_info)]},
		  ok
	  end,
    {reply, Reply, NewState};
%%--------------------------------------------------------------------------    
handle_call({deploy_application,ApplicationId}, _From, State) ->
    Result=try lib_controller:deploy_application(ApplicationId) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,DeploymentInfo}->
		%  io:format("DeploymentInfo ~p~n",[{DeploymentInfo,?MODULE,?LINE}]),
		  NewState=State#state{deployment_info=[DeploymentInfo|State#state.deployment_info]},
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  DeploymentInfo=#{
				   application_id=>ApplicationId,
				   app=>na,
				   node=>na,
				   nodename=>na,
				   node_id=>na,
				   time=>{date(),time()},
				   state=>scheduled
				  },
		  NewState=State#state{deployment_info=[DeploymentInfo|State#state.deployment_info]},
		  ErrorEvent
	  end,
    {reply, Reply, NewState};

handle_call({remove_application,ApplicationId}, _From, State) ->
    DeploymentInfoList=State#state.deployment_info,
    Result=try lib_controller:remove_application(ApplicationId,DeploymentInfoList) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,UpdatedDeploymentInfoList}->
	%	  io:format("UpdatedDeploymentInfoList ~p~n",[{UpdatedDeploymentInfoList,?MODULE,?LINE}]),
		  NewState=State#state{deployment_info=UpdatedDeploymentInfoList},
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  ErrorEvent
	  end,
    {reply, Reply, NewState};

handle_call({new_deployment,DeploymentId}, _From, State)
  when State#state.deployment_id==undefined ->
    Result=try lib_control:new_deployment(DeploymentId) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    Reply=case Result of
	      {ok,WantedState}->
		  io:format("WantedState ~p~n",[{WantedState,?MODULE,?LINE}]),
		  NewState=State,
		  ok;
	      ErrorEvent->
		  io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
		  NewState=State,
		  ErrorEvent
	  end,
    {reply, Reply, NewState};
 
handle_call({new_deployment,DeploymentId}, _From, State)->
    Reply={error,["Deployment is already deployed ",State#state.deployment_id]},
    {reply, Reply, State};


%%--------------------------------------------------------------------
handle_call({read_deployment_info}, _From, State) ->
    Reply=State#state.deployment_info,
    {reply, Reply, State};


handle_call({read_wanted_state}, _From, State) ->
    Reply=[maps:get(application_id,DeploymentInfo)||DeploymentInfo<-State#state.deployment_info],
    {reply, Reply, State};

handle_call({read_state}, _From, State) ->
    Reply=State,
    {reply, Reply, State};

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


handle_cast({reconciliate}, State) ->
    io:format(" ~p~n",[{?FUNCTION_NAME,?MODULE,?LINE}]),
    DeploymentInfoList=State#state.deployment_info,
    Result=try lib_reconciliate:start(?Interval,DeploymentInfoList) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    case Result of
	{ok,UpdatedDeploymentInfoList}->
	    io:format("UpdatedDeploymentInfoList ~p~n",[{UpdatedDeploymentInfoList,?MODULE,?LINE}]),
	    NewState=State#state{deployment_info=UpdatedDeploymentInfoList},
	    ok;
	ErrorEvent->
	    io:format("ErrorEvent ~p~n",[{ErrorEvent,?MODULE,?LINE}]),
	    NewState=State,
	    ErrorEvent
    end,
    {noreply, NewState};

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


handle_info({nodedown,Node}, State) ->
    io:format("nodedown,Node  ~p~n",[{Node,?MODULE,?LINE}]),
    DeploymentInfoList=State#state.deployment_info,
    Result=try lib_controller:clean_up(Node,DeploymentInfoList) of
	       {ok,R}->
		   {ok,R};
	       {error,Reason}->
		   {error,Reason}
	   catch
	       Event:Reason:Stacktrace ->
		   {Event,Reason,Stacktrace,?MODULE,?LINE}
	   end,
    case Result of
	{ok,UpdatedDeploymentInfoList}->
%	    io:format("UpdatedDeploymentInfoList ~p~n",[{UpdatedDeploymentInfoList,?MODULE,?LINE}]),
	    NewState=State#state{deployment_info=UpdatedDeploymentInfoList},
	    ok;
	ErrorEvent->
	    R2=[DeploymentInfo||DeploymentInfo<-DeploymentInfoList,
			       Node==maps:get(node,DeploymentInfo)],
	    case R2 of
		[]->
		     NewState=State;
		[DeploymentInfo]->
		    ApplicationId=maps:get(application_id,DeploymentInfo),
		    DeploymentInfo=#{
				     application_id=>ApplicationId,
				     app=>na,
				     node=>na,
				     nodename=>na,
				     node_id=>na,
				     time=>na,
				     state=>scheduled
				    },
		    NewState=State#state{deployment_info=[DeploymentInfo|State#state.deployment_info]};
		_ ->
		    NewState=State
			
	    end
    end,
    {noreply, NewState};


handle_info(timeout, State) ->
%    io:format("timeout State ~p~n",[{State,?MODULE,?LINE}]),
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

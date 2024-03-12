%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_controller).
  

 
  
%% API
-export([
	 deploy_application/1,
	 remove_application/2	 
	]).

-export([

	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Creates a new workernode , load and start infra services (log and resource discovery)
%% and  the wanted application ApplicationId
%% @end
%%--------------------------------------------------------------------
deploy_application(ApplicationId)->

    %% Get ApplicationId info , crash if doesnt exists
    {ok,ApplicationIdPaths}=rd:call(catalog,get_application_paths,[ApplicationId],5000),
    {ok,ApplicationIdApp}=rd:call(catalog,get_application_app,[ApplicationId],5000),
  
    %% Create new worker node
    {ok,WorkerInfo}=lib_worker_controller:create_worker(ApplicationId),
    WorkerNode=maps:get(node,WorkerInfo),
    
    %% Load and start log
    {ok,LogPaths}=rd:call(catalog,get_application_paths,["log"],5000),
    {ok,LogApp}=rd:call(catalog,get_application_app,["log"],5000),
    [rpc:call(WorkerNode,code,add_patha,[Path],5000)||Path<-LogPaths],
    ok=rpc:call(WorkerNode,application,load,[LogApp],5000),
    ok=rpc:call(WorkerNode,application,start,[LogApp],5000),
    pong=rpc:call(WorkerNode,LogApp,ping,[],5000),

    %% Load and start resource discovery
    {ok,RdPaths}=rd:call(catalog,get_application_paths,["resource_discovery"],5000),
    {ok,RdApp}=rd:call(catalog,get_application_app,["resource_discovery"],5000),
    [rpc:call(WorkerNode,code,add_patha,[Path],5000)||Path<-RdPaths],
    ok=rpc:call(WorkerNode,application,load,[RdApp],5000),
    ok=rpc:call(WorkerNode,application,start,[RdApp],5000),
    pong=rpc:call(WorkerNode,RdApp,ping,[],5000),
    
    %% Load and start ApplicationId and start as permanent so if it crashes the node crashes
    [rpc:call(WorkerNode,code,add_patha,[Path],5000)||Path<-ApplicationIdPaths],
    ok=rpc:call(WorkerNode,application,load,[ApplicationIdApp],5000),
    ok=rpc:call(WorkerNode,application,start,[ApplicationIdApp,permanent],5000),
    pong=rpc:call(WorkerNode,ApplicationIdApp,ping,[],5000),
    
    %% All good
    ApplicationInfo=#{application_id=>ApplicationId,
		      app=>ApplicationIdApp,
		      time=>{date(),time()}},
    DeploymentInfo=[{application_info,ApplicationInfo},
		    {worker_info,WorkerInfo}],
    {ok,DeploymentInfo}.
	

%%--------------------------------------------------------------------
%% @doc
%% Creates a new workernode , load and start infra services (log and resource discovery)
%% and  the wanted application ApplicationId
%% @end
%%--------------------------------------------------------------------
remove_application(ApplicationId,DeploymentInfoList)->
    
    %% Get DeploymetInfo for an deployment with ApplicationId, crash if doesnt exists
    [{ApplicationInfo,WorkerInfo}|_]=[{ApplicationInfo,WorkerInfo}||[{application_info,ApplicationInfo},{worker_info,WorkerInfo}]<-DeploymentInfoList,
				ApplicationId==maps:get(application_id,ApplicationInfo)],
    
    %% stop monitoring the node
    WorkerNode=maps:get(node,WorkerInfo),
    erlang:monitor_node(WorkerNode,false),
    
    %% stop  ApplicationId
    ApplicationIdApp=maps:get(app,ApplicationInfo),
    ok=rpc:call(WorkerNode,application,stop,[ApplicationIdApp],5000),
    ok=rpc:call(WorkerNode,application,unload,[ApplicationIdApp],5000),
    
    %% stop  resource discovery
    {ok,RdApp}=rd:call(catalog,get_application_app,["resource_discovery"],5000),
    ok=rpc:call(WorkerNode,application,stop,[RdApp],5000),
    ok=rpc:call(WorkerNode,application,unload,[RdApp],5000),
    
    %% stop log
    {ok,LogApp}=rd:call(catalog,get_application_app,["log"],5000),
    ok=rpc:call(WorkerNode,application,stop,[LogApp],5000),
    ok=rpc:call(WorkerNode,application,unload,[LogApp],5000),
    
    
    %% stope slave node
    slave:stop(WorkerNode),
    
    %% All good
    DeploymentInfo=[{application_info,ApplicationInfo},
		    {worker_info,WorkerInfo}],
    {ok,DeploymentInfo}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Application  part Start
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Cluster part Start
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------

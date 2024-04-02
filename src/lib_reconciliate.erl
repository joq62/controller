%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 11 Jan 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_reconciliate).
   
-include("controller.hrl").
 
  
%% API
-export([
	 start/1
	]).

-export([
	 wanted_applications/0,
	 active_applications/0,
	 applications_to_start/0,
	 applications_to_stop/0

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
start(DeploymentInfoList)->
  %  io:format("DeploymentInfoList ~p~n",[{?MODULE,?LINE,DeploymentInfoList}]),
    %% Check which Nodes that are running
    ActiveWorkerNodesOnThisHost=get_active_workers(),
 %   io:format("ActiveWorkerNodesOnThisHost ~p~n",[{ActiveWorkerNodesOnThisHost,?MODULE,?LINE}]),
    %%
    ActiveNodeApplicationApp=get_active_applications_nodes(ActiveWorkerNodesOnThisHost),
 %   io:format("ActiveNodeApplicationApp ~p~n",[{ActiveNodeApplicationApp,?MODULE,?LINE}]),
    %
    UpdatedDeploymentInfoListToStart=get_applications_to_start(ActiveNodeApplicationApp,DeploymentInfoList),
 %   io:format("UpdatedDeploymentInfoListToStart ~p~n",[{UpdatedDeploymentInfoListToStart,?MODULE,?LINE}]),

    UpdatedDeploymentInfoListToStartStop=get_applications_to_stop(ActiveNodeApplicationApp,UpdatedDeploymentInfoListToStart),
 %   io:format("UpdatedDeploymentInfoListToStartStop ~p~n",[{UpdatedDeploymentInfoListToStartStop,?MODULE,?LINE}]),

     UpdatedDeploymentInfoListDeploy=deploy_applications(UpdatedDeploymentInfoListToStartStop),
 %   io:format("UpdatedDeploymentInfoListDeploy ~p~n",[{UpdatedDeploymentInfoListDeploy,?MODULE,?LINE}]),

    UpdatedDeploymentInfoList=remove_applications(UpdatedDeploymentInfoListDeploy),
  %  io:format("UpdatedDeploymentInfoList ~p~n",[{UpdatedDeploymentInfoList,?MODULE,?LINE}]),
    {ok,UpdatedDeploymentInfoList}.
  
%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
applications_to_stop()->
    WantedApplications=wanted_applications(),
    ActiveApplications=active_applications(),
    applications_to_stop(WantedApplications,ActiveApplications).

applications_to_stop([],[])->
    [];
applications_to_stop(WantedApplications,[])->
    [];
applications_to_stop([],ActiveApplications)->
    ActiveApplications;
applications_to_stop([WantedApplicationFile|T],ActiveApplications)->
    case lists:keyfind(WantedApplicationFile,2,ActiveApplications) of
	{WorkerNode,FileName}->
	    NewActiveApplications=lists:delete(	{WorkerNode,FileName},ActiveApplications);
	false ->
	    NewActiveApplications=ActiveApplications
    end,
    applications_to_stop(T,NewActiveApplications).


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
applications_to_start()->
    WantedApplications=wanted_applications(),
    ActiveApplications=active_applications(),
    applications_to_start(WantedApplications,ActiveApplications).

applications_to_start(WantedApplications,ActiveApplications)->
    applications_to_start(WantedApplications,ActiveApplications,[]).

applications_to_start([],_ActiveApplications,Acc)->
    Acc;
applications_to_start([WantedApplicationFile|T],ActiveApplications,Acc)->
    case lists:keyfind(WantedApplicationFile,2,ActiveApplications) of
	{WorkerNode,FileName}->
	    NewActiveApplications=lists:delete(	{WorkerNode,FileName},ActiveApplications),
	    NewAcc=Acc;
	false ->
	    NewActiveApplications=ActiveApplications,
	    NewAcc=[WantedApplicationFile|Acc]
    end,
    applications_to_start(T,NewActiveApplications,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
wanted_applications()->
    {ok,FileNames}=rd:call(deployment,all_filenames,[],3*5000),
    get_wanted_applications(FileNames).

get_wanted_applications(FileNames)->
    get_wanted_applications(FileNames,[]).

get_wanted_applications([],Acc)->
    Acc;
get_wanted_applications([FileName|T],Acc)->
    {ok,[Map]}=rd:call(deployment,read_file,[FileName],3*5000),
    {ok,HostName}=net:gethostname(),
    R=[ApplicationFileName||{ApplicationFileName,WantedHostName}<-maps:get(deployments,Map),
		      HostName==WantedHostName],
    NewAcc=lists:append(R,Acc),
    get_wanted_applications(T,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
active_applications()->
    ActiveNodes=get_active_workers(),
    AllApps=all_apps(),
    node_filenames(ActiveNodes,AllApps).
    
node_filenames(ActiveNodes,AllApps)->   
    node_filenames(ActiveNodes,AllApps,[]).
node_filenames([],_AllApps,Acc)->
    Acc;
node_filenames([WorkerNode|T],AllApps,Acc)->
    ApplicationInfo=rpc:call(WorkerNode,application,which_applications,[],5000),
    NodeFileNames=check_apps(ApplicationInfo,WorkerNode,AllApps),
    NewAcc=lists:append(NodeFileNames,Acc),
    node_filenames(T,AllApps,NewAcc).


check_apps(ApplicationInfo,WorkerNode,AllApps)->
    check_apps(ApplicationInfo,WorkerNode,AllApps,[]).

check_apps([],_,_,Acc)->
    Acc;
check_apps([{App,_,_}|T],WorkerNode,AllApps,Acc)->
    NewAcc=case lists:member(App,AllApps) of
	       false->
		   Acc;
	       true ->
		   {ok,FileName}=rd:call(catalog,which_filename,[App],3*5000),
		   [{WorkerNode,FileName}|Acc]
	   end,
    check_apps(T,WorkerNode,AllApps,NewAcc).
				
all_apps()->
    {ok,FileNames}=rd:call(catalog,all_filenames,[],3*5000),
    all_apps(FileNames).
all_apps(FileNames)->
    all_apps(FileNames,[]).		
all_apps([],Acc)->	   
    Acc;
all_apps([FileName|T],Acc) ->
    NewAcc=case lists:member(FileName,?InfraApplicationFileNames) of
	       false->
		   {ok,[Map]}=rd:call(catalog,read_file,[FileName],3*5000),
		   App=maps:get(app,Map),
		   [App|Acc];
	       true->
		   Acc
	   end,
    all_apps(T,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_active_workers()->
    {ok,ThisHostName}=net:gethostname(),
    ActiveWorkerNodesOnThisHost=[Node||Node<-[node()|nodes()],
				 {ok,ThisHostName}==rpc:call(Node,net,gethostname,[],5000)],
    ActiveWorkerNodesOnThisHost.


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
deploy_applications([])->
    [];

deploy_applications(DeploymentInfoList)->
    deploy_applications(DeploymentInfoList,[]).
    

deploy_applications([],Acc)->
    Acc;
deploy_applications([DeploymentInfo|T],Acc)->
    ApplicationId=maps:get(application_id,DeploymentInfo),
    UpdatedDeploymentInfo=case maps:get(state,DeploymentInfo) of
			      started->
				  DeploymentInfo;
			      delete ->
				  DeploymentInfo;
			      scheduled ->
				  {ok,DeploymentInfoDeploy}=lib_controller:deploy_application(ApplicationId),
				  DeploymentInfoDeploy
			  end,	
    NewAcc=[UpdatedDeploymentInfo|Acc],
    deploy_applications(T,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
remove_applications([])->
    [];

remove_applications(DeploymentInfoList)->
    remove_applications(DeploymentInfoList,[]).
    

remove_applications([],UpdatedDeploymentInfoList)->
    UpdatedDeploymentInfoList;
remove_applications([DeploymentInfo|T],Acc)->
    NewAcc=case maps:get(state,DeploymentInfo) of
	       started->
		   [DeploymentInfo|Acc];
	       scheduled ->
		   [DeploymentInfo|Acc];
	       delete ->
		   ok=lib_controller:remove_application(DeploymentInfo),
		   Acc
	   end,	
    remove_applications(T,NewAcc).
    




%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
get_active_applications_nodes(ActiveWorkerNodesOnThisHost)->

    AllApplicationIds=[{ApplicationId,rd:call(catalog,app,[ApplicationId],5000)}||
			  ApplicationId<-rd:call(catalog,get_all_ids,[],5000)],
    ActiveNodeApplicationApp=get_applications(ActiveWorkerNodesOnThisHost,AllApplicationIds),
    ActiveNodeApplicationApp.


get_applications(ActiveWorkerNodesOnThisHost,AllApplicationIds)->
    get_applications(ActiveWorkerNodesOnThisHost,AllApplicationIds,[]).
get_applications([],_AllApplicationIds,Acc)->
    Acc;
get_applications([WorkerNode|T],AllApplicationIds,Acc)->
    NewAcc=case rpc:call(WorkerNode,application,which_application,[],5000) of
	       {badrpc,Reason}->
		   io:format("Should be log ~p~n",[{Reason,?MODULE,?LINE}]),
		   Acc;
	       RunningApplications->
		   NodeApplicationApp=[{WorkerNode,ApplicationId,App}||{ApplicationId,App}<-AllApplicationIds,
								 lists:keymember(App,1,RunningApplications)],
		   lists:append(NodeApplicationApp,Acc)
	   end,
    get_applications(T,AllApplicationIds,NewAcc).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


get_applications_to_start([],UpdatedDeploymentInfoList)->
    UpdatedDeploymentInfoList;									

get_applications_to_start(DeploymentInfoList,ActiveApplicationsAndNodes)->
    NumWantedApplications=calculate_wanted_applications(DeploymentInfoList),
    NumRunningApplications=calculate_running_applications(ActiveApplicationsAndNodes),
    NumMissingApplications= missing(NumWantedApplications,NumRunningApplications),
    UpdatedDeploymentInfoStart=missing_deployment_info(NumMissingApplications),
    lists:append(UpdatedDeploymentInfoStart,DeploymentInfoList).
	

%%------------------  -----------------------------------------------
%% @doc
%% Stop Criteria : ActiveApplicationsAndNodes, DeploymentInfoList
%%       1. ActiveApplicationsAndNodes =/=[] , DeploymentInfoList == []
%%       2. length(DeploymentInfoList) > lenght(ActiveApplicationsAndNodes) 
%%       3. ActiveApplicationsAndNodes not member of DeploymentInfoList(ApplicationId,WorkerNode) not member 
%% @end
%%--------------------------------------------------------------------
get_applications_to_stop(ActiveApplicationsAndNodes,[])->
    UpdateDeploymentInfoList=[#{
				application_id=>ApplicationId,
				app=>App,
				node=>WorkerNode,
				nodename=>na,
				node_id=>na,
				time=>na,
				state=>delete
			       }||{WorkerNode,ApplicationId,App}<-ActiveApplicationsAndNodes],
    UpdateDeploymentInfoList;

get_applications_to_stop(ActiveApplicationsAndNodes,DeploymentInfoList)->
    xapplications_to_stop(ActiveApplicationsAndNodes,DeploymentInfoList).

xapplications_to_stop([],UpdatedDeploymentInfoList)->
    UpdatedDeploymentInfoList;									
  
xapplications_to_stop([{WorkerNode,ApplicationId,App}|T],DeploymentInfoList)->
    R=[ApplicationId||DeploymentInfo<-DeploymentInfoList,
		      ApplicationId=/=maps:get(application_id,DeploymentInfo),
		      WorkerNode=/=maps:get(node,DeploymentInfo)],
    UpdateDeploymentInfoList=case R of
				 []->
				     DeploymentInfoList;
				 [ApplicationId] ->
				     DeploymentInfo=#{
						      application_id=>ApplicationId,
						      app=>App,
						      node=>WorkerNode,
						      nodename=>na,
						      node_id=>na,
						      time=>na,
						      state=>delete
						     },
				     [DeploymentInfo|DeploymentInfoList]
			     end,
    xapplications_to_stop(T,UpdateDeploymentInfoList).
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
calculate_running_applications(ActiveApplicationsAndNodes)->
    calculate_running_applications(ActiveApplicationsAndNodes,[]).

calculate_running_applications([],Acc) ->
    Acc;
calculate_running_applications([{_,ApplicationId,_}|ActiveApplicationsAndNodes],Acc)->
    Num=erlang:lenght([ZApplicationId||{_WorkerNode,ZApplicationId,_App}<-ActiveApplicationsAndNodes]),
    UpdatedActiveApplicationsAndNodes=[{WorkerNode,XApplicationId,App}||{WorkerNode,XApplicationId,App}<-ActiveApplicationsAndNodes,
									ApplicationId=/=XApplicationId],
    NewAcc=[{ApplicationId,Num}|Acc],
    calculate_running_applications(UpdatedActiveApplicationsAndNodes,NewAcc).
		     
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
calculate_wanted_applications(DeploymentInfoList)->
    calculate_wanted_applications(DeploymentInfoList,[]).

calculate_wanted_applications([],Acc)->
    Acc;
calculate_wanted_applications([DeploymentInfo|DeploymentInfoList],Acc)->
    ApplicationId=maps:get(application_id,DeploymentInfo),
    Num=erlang:lenght([ApplicationId||M<-DeploymentInfoList,
				      ApplicationId==maps:get(application_id,M)]),
    UpdatedInfoList=[M||M<-DeploymentInfoList,
			ApplicationId=/=maps:get(application_id,M)],
    NewAcc=[{ApplicationId,Num}|Acc],
    calculate_wanted_applications(UpdatedInfoList,NewAcc).



		     
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
missing(NumWantedApplications,NumRunningApplications)->		     
    missing(NumWantedApplications,NumRunningApplications,[]).

missing([],_,Acc)->
    Acc;
missing([{ApplicationId,Num}|T],NumRunningApplications,Acc)->
    
    NewAcc=case lists:keyfind(ApplicationId,1,NumRunningApplications) of
	       false ->
		   [{ApplicationId,Num}|Acc];
	       {ApplicationId,Xnum}->
		   Diff=Num-Xnum,
		   if
		       Diff>0 ->
			   [{ApplicationId,Diff}|Acc];
		       true->
			   Acc
		   end
	   end,
    missing(T,NumRunningApplications,NewAcc).
		

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
missing_deployment_info(NumMissingApplications)->
    missing_deployment_info(NumMissingApplications,[]).

missing_deployment_info([],Acc)->
    Acc;
missing_deployment_info([{ApplicationId,Num}|T],Acc) ->
    DeploymentInfoList=deployment_info(ApplicationId,Num,[]),
    NewAcc=lists:append(DeploymentInfoList,Acc),
    missing_deployment_info(T,NewAcc).

deployment_info(_ApplicationId,0,Acc)->
    Acc;
deployment_info(ApplicationId,N,Acc) ->
    DeploymentInfo=#{
		     application_id=>ApplicationId,
		     app=>na,
		     node=>na,
		     nodename=>na,
		     node_id=>na,
		     time=>na,
		     state=>scheduled
		    },
    NewAcc= [DeploymentInfo|Acc],
    deployment_info(ApplicationId,N-1,NewAcc).

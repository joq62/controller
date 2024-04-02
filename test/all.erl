%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
 
-export([start/0]).

-include("controller.hrl").


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


-define(DeploymentRepoDir,"deployment_specs_test").
-define(DeploymentGit,"https://github.com/joq62/deployment_specs_test.git").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
%    ok=init_test(),
    ok=controller_test:start(),
 %   ok=reconciliate_test:start(),
 %   ok=destructive_test:start(),
    io:format("Test OK !!! ~p~n",[?MODULE]),
%    timer:sleep(1000),
%    init:stop(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
init_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    [
     "adder.application",
     "adder.application",
     "adder.application",
     "divi.application",
     "divi.application"
    ]=lists:sort(lib_reconciliate:wanted_applications()),
    []=lib_reconciliate:active_applications(),
     [
     "adder.application",
     "adder.application",
     "adder.application",
     "divi.application",
     "divi.application"
    ]=lists:sort(lib_reconciliate:applications_to_start()),
    []=lists:sort(lib_reconciliate:applications_to_stop()),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=application:start(log),
    pong=log:ping(),
    ok=application:start(rd),
    pong=rd:ping(),

    file:del_dir_r(?MainLogDir),
    file:make_dir(?MainLogDir),

    ok=application:start(git_handler),
    pong=git_handler:ping(),
    ok=application:start(catalog),
    pong=catalog:ping(),
    ok=application:start(deployment),
    pong=deployment:ping(),

    ok=application:start(controller),
    pong=controller:ping(),
 

    [rd:add_local_resource(ResourceType,Resource)||{ResourceType,Resource}<-[]],
    [rd:add_target_resource_type(TargetType)||TargetType<-[log,rd,catalog,deployment,adder,divi]],
    rd:trade_resources(),
    timer:sleep(3000),
    [
     {catalog,{catalog,'controller_a@c50'}},
     {deployment,{deployment,'controller_a@c50'}},
     {git_handler,{git_handler,'controller_a@c50'}}
    ]=lists:sort(rd:get_all_resources()),

  %  ok=deployment:update_repo_dir(?DeploymentRepoDir),
  %  ok=deployment:update_git_path(?DeploymentGit),

  %  timer:sleep(3000),
    
    ok.

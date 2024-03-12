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
-module(controller_test).      
 
-export([start/0]).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

 
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    
    ok=setup(),
    ok=deploy_remove_normal(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
%    timer:sleep(1000),
%    init:stop(),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
deploy_remove_normal()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    
    {error,[eexists_resources]}=rd:call(adder,adder,ping,[],5000),
    ok=controller:deploy_application("adder"),
    pong=rd:call(adder,adder,ping,[],5000),
    42=rd:call(adder,adder,add,[20,22],5000),

    %% Remove the only one
    ok=controller:remove_application("adder"),
    {error,[eexists_resources]}=rd:call(adder,adder,ping,[],5000),
    {error,[eexists_resources]}=rd:call(adder,adder,add,[20,22],5000),

    %% Add two 
    ok=controller:deploy_application("adder"),
    ok=controller:deploy_application("adder"),
    42=rd:call(adder,adder,add,[20,22],5000),

    %% Remove the  one
    ok=controller:remove_application("adder"),
    42=rd:call(adder,adder,add,[20,22],5000),

  %% Remove the  last
    ok=controller:remove_application("adder"),
    {error,[eexists_resources]}=rd:call(adder,adder,add,[20,22],5000),
    
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    pong=controller:ping(),
    [rd:add_target_resource_type(TargetType)||TargetType<-[adder,divi]],
    rd:trade_resources(),
    timer:sleep(3000),
    ok.

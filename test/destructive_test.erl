%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Test crashes and recovery 
%%% 1.1 Node crash -> application shall be restarted
%%% 1.2 Node crashes -> application shall not be started
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(destructive_test).      
 
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
    ok=node_crash1(),
  %  ok=add_delete(),
  %  ok=add_load_start_delete_stop_unload(),
%    ok=check_monitoring(),
%    ok=deploy_remove_normal(),

    io:format("Test OK !!! ~p~n",[?MODULE]),
  %  timer:sleep(1000),
  %  init:stop(),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: 
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
node_crash1()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% Empty system 
    []=controller:read_deployment_info(),
    
    %% Add "adder"
    ok=controller:add_application("adder"),
    ok=controller:reconciliate(),
    timer:sleep(3000), 
    [{adder,A1}]=rd:fetch_resources(adder),
    42=rpc:call(A1,adder,add,[20,22],5000),
    [started]=[maps:get(state,M)||M<-controller:read_deployment_info()],
    %% Kill node
    rpc:call(A1,init,stop,[],5000),
    timer:sleep(2000),
    {badrpc,_}=rpc:call(A1,adder,add,[20,22],5000),
    [scheduled]=[maps:get(state,M)||M<-controller:read_deployment_info()],

    %% 
    ok=controller:reconciliate(),
    timer:sleep(3000), 
    [{adder,A2}]=rd:fetch_resources(adder),
    42=rpc:call(A2,adder,add,[20,22],5000),
    [started]=[maps:get(state,M)||M<-controller:read_deployment_info()],

    %% 
    ok=controller:delete_application("adder"),
    [delete]=[maps:get(state,M)||M<-controller:read_deployment_info()],
    ok=controller:reconciliate(),
    timer:sleep(3000), 
    []=controller:read_deployment_info(),
    
    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
application_crash()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),


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

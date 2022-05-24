-module(chat_supervisor).

-behaviour(supervisor).

-export([stop/0,start_link/0]).
-export([init/1]).

start_link() ->
	{ok,Pid} = supervisor:start_link({global,?MODULE},?MODULE,[]),
	unlink(Pid).
stop() ->
	chat_operator:stop(),
	chat_client:stop(),
	chat_server:stop().

init([]) ->
	io:format(" ~p (~p) starting... ~n",[{local,?MODULE},self()]),
	RestartStrategy = one_for_all,
	MaxRestart = 10,
	MaxSecondsBetweenRestart = 5,
	Flags = {RestartStrategy,MaxRestart,MaxSecondsBetweenRestart},
	Restart = permanent,
	Shutdown = infinity,
	Type = worker,
	ServerSpec = {chat_server_id,{chat_server,start_link,[]},Restart,Shutdown,Type,[chat_server]},
	ClientSpec = {chat_client_id,{chat_client,start_link,[]},Restart,Shutdown,Type,[chat_client]},
	OperatorSpec = {chat_operator_id,{chat_operator,start_link,[]},Restart,Shutdown,Type,[chat_operator]},
	{ok,{Flags,[ServerSpec,ClientSpec,OperatorSpec]}}.
-module(chat_supervisor).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	{ok,Pid} = supervisor:start_link({global,?MODULE},?MODULE,[]),
	unlink(Pid).

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
	{ok,{Flags,[ServerSpec,ClientSpec]}}.
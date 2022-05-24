-module(chat_operator).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/0,get_app_names/0,get_logs/1,get_logs/2,get_logs/3]).
%%api
start_link()->
	gen_server:start_link({global,?MODULE},?MODULE,[],[]).


get_app_names() ->
	gen_server:cast({global,chat_server},{apps}).

get_logs(AppName) ->
	gen_server:cast({global,chat_server},{logs,AppName}).
get_logs(AppName,MessageType) ->
	gen_server:cast({global,chat_server},{logs,AppName,MessageType}).
get_logs(AppName,MessageType,N) ->
	gen_server:cast({global,chat_server},{logs,AppName,MessageType,N}).
%%%%
init([]) ->
	process_flag(trap_exit,true),
	io:format(" ~p (~p) starting... ~n",[{local,?MODULE},self()]),
	{ok,[]}.
	




handle_call(_Req,_From,State) ->
	{reply,ok,State}.

	
handle_cast(_Req,State) ->
	{noreply,State}.

handle_info(Info,State) ->
	{noreply,Info,State}.

terminate(_reason,_state) ->
	io:format("Terminating ~p~n",[{local,?MODULE}]),
	ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
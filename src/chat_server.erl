-module(chat_server).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([start_link/0,stop/0]).
%%api
start_link()->
	gen_server:start_link({global,?MODULE},?MODULE,[],[]).

stop()->
	gen_server:cast({global,?MODULE},stop).




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
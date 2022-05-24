-module(chat_client).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3,send/1,subscribe/1,unsubscribe/1]).
-export([start_link/0,stop/0]).
%%api
start_link()->
	gen_server:start_link({global,?MODULE},?MODULE,[],[]).

stop()->
	gen_server:cast({global,?MODULE},stop).


send(Msg) ->
	gen_server:cast({global,?MODULE},{send,Msg}).

subscribe(Username) ->
	gen_server:cast({global,?MODULE},{subscribe,Username}).
unsubscribe(Username) ->
	gen_server:cast({global,?MODULE},{unsubscribe,Username}).

%%%%
init([]) ->
	process_flag(trap_exit,true),
	{ok,Socket} = gen_tcp:connect({127,0,0,1},9000,[binary,{active,true}]),
	io:format(" ~p (~p) starting... ~n",[{local,?MODULE},self()]),
	{ok,Socket}.
	

	
handle_call(_Req,_From,State) ->
	{reply,ok,State}.

handle_cast({send,Msg},State) ->
	gen_tcp:send(State,Msg),
	{noreply,State};

handle_cast({unsubscribe,Username},State) ->
	gen_tcp:send(State,"Unsubscribe" ++ "|" ++ Username),
	{noreply,State};

handle_cast({subscribe,Username},State) ->
	gen_tcp:send(State,"Subscribe" ++ "|" ++ Username),
	{noreply,State};
	
handle_cast(_Req,State) ->
	{noreply,State}.

handle_info(Info,State) ->
	{noreply,Info,State}.

terminate(_reason,_state) ->
	io:format("Terminating ~p~n",[{local,?MODULE}]),
	ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
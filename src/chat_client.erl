-module(chat_client).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3,send/3,subscribe/2,unsubscribe/1]).
-export([start_link/0,get_time/0,crash/2]).
%%api
start_link()->
	gen_server:start_link({global,?MODULE},?MODULE,[],[]).

get_time() ->
	{{Year,Month,Day},{Hour,Min,Sec}} = calendar:universal_time(),
	string:join([Day,Month,Year,Hour,Min,Sec],", ").

send(Username,AppName,Msg) ->
	gen_server:cast({global,?MODULE},{send,Username,AppName,Msg}).

subscribe(Username,AppName) ->
	gen_server:cast({global,?MODULE},{subscribe,Username,AppName}).
unsubscribe(Username) ->
	gen_server:cast({global,?MODULE},{unsubscribe,Username}).
crash(Username,AppName) ->
	gen_server:cast({global,chat_server},{crashServer,Username,AppName}).

%%%%
init([]) ->
	process_flag(trap_exit,true),
	{ok,Socket} = gen_tcp:connect({127,0,0,1},9000,[binary,{active,true}]),
	io:format(" ~p (~p) starting... ~n",[{local,?MODULE},self()]),
	{ok,Socket}.
	   

	
handle_call(_Req,_From,State) ->
	{reply,ok,State}.

handle_cast({send,Username,AppName,Msg},State) ->
	
	gen_tcp:send(State,"send" ++ Username ++ "|" ++ AppName ++ "|" ++ "TRACE" ++ "|" ++ Msg),
	{noreply,State};

handle_cast({unsubscribe,Username},State) ->
	gen_tcp:send(State,"Unsubscribe" ++ "|" ++ Username),
	{noreply,State};

handle_cast({subscribe,Username,AppName},State) ->
	gen_tcp:send(State,"Subscribe" ++ "|" ++ Username ++ "|" ++ AppName),
	{noreply,State};
	
handle_cast(_Req,State) ->
	{noreply,State}.

handle_info(Info,State) ->
	{noreply,Info,State}.

terminate(Reason,_state) ->
	db:store_message({"chat_client",calendar:universal_time(),"WARRNING", Reason}),
	io:format("Terminating ~p~n",[{local,?MODULE}]),
	ok.

code_change(_OldVsn,State,_Extra) ->
	{ok,State}.
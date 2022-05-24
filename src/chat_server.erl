-module(chat_server).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([acceptState/1,handler/1,validate/1]).
-export([start_link/0,stop/0]).
-define(Port,9000).
%%api
start_link()->
	gen_server:start_link({global,?MODULE},?MODULE,[],[]).

stop()->
	gen_server:cast({global,?MODULE},stop).




%%%%
init([]) ->
	process_flag(trap_exit,true),
	io:format(" ~p (~p) starting... ~n",[{local,?MODULE},self()]),
	db:initDB(),
	Pid = spawn_link(fun() -> 
	{ok,LSocket} = gen_tcp:listen(?Port,[binary,{active,false}]),
	spawn(fun() -> acceptState(LSocket) end),
	timer:sleep(infinity)
	end),
	{ok,Pid}.

acceptState(LSocket) ->	
	{ok,ASocket} = gen_tcp:accept(LSocket),
	spawn(fun() -> acceptState(LSocket) end),
	handler(ASocket).

handler(ASocket) ->
	inet:setopts(ASocket,[{active,once}]),
	receive
		{tcp,ASocket,BinaryMsg} ->
			validate(BinaryMsg),
		handler(ASocket)
	end.
	

validate(BinaryMsg) ->
	io:format("~p~n",[BinaryMsg]),
	ListOfStrings = string:tokens(binary_to_list(BinaryMsg),"|"),
	io:format("~p~n",[ListOfStrings]),
	[Head|Tail] = ListOfStrings,
	[Value] = Tail,
	case Head of
		"Subscribe" -> 	gen_server:call({global,?MODULE},{subscribe,Value});
		"Unsubscribe" -> gen_server:call({global,?MODULE},{unsubscribe,Value});
		_ -> "Treba raditi validaciju poruke koja se salje, je li user subscribed..."
	end.

handle_call({subscribe,Username},_From,State) ->

	Accounts = db:get_users(),
	case lists:member(Username,Accounts) of
			true ->
				io:format("User: ~p is alrdy subscribed. ~n", [Username]),
				{reply,ok,State};
			false ->
				db:store_user(Username),
				io:format("User: ~p is subscribed now. ~n",[Username]),
				{reply,ok,State}
		end;

handle_call({unsubscribe,Username},_From,State) ->
	Accounts = db:get_users(),
	case lists:member(Username,Accounts) of
			true ->
				db:delete_user(Username),
				io:format("User: ~p is unsubscribed now. ~n", [Username]),
				{reply,ok,State};
			false ->
				io:format("User: ~p is not subscribed. ~n",[Username]),
				{reply,ok,State}
		end;

	
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
-module(chat_server).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([acceptState/1,handler/1,validate/1,is_user_subscribed/1,send/1,validate_message/1,send_message/1]).
-export([start_link/0]).
-define(Port,9000).
%%api
start_link()->
	gen_server:start_link({global,?MODULE},?MODULE,[],[]).




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
		{tcp,ASocket,<<"send",X/binary>>} ->
			send(X),
		handler(ASocket);
		{tcp,ASocket,BinaryMsg} ->
			validate(BinaryMsg),
			handler(ASocket)
		
	end.
	

send(BinaryMsg) ->
	io:format("~p~n",[BinaryMsg]),
	ListOfStrings = string:tokens(binary_to_list(BinaryMsg),"|"),
	io:format("~p~n",[ListOfStrings]),
	case is_user_subscribed(hd(ListOfStrings)) of
		true ->
			case validate_message(tl(ListOfStrings)) of
				true -> 
					send_message(ListOfStrings);
				_->
					io:format("Not valid format!~n")
			end;
		false -> 
			io:format("User is not subscribed ~n")
	end.
	
send_message(Message) ->
	case lists:nth(3,tl(Message)) of
		"ERROR" ->
			gen_server:call({global,?MODULE},{send,hd(Message),list_to_tuple(tl(Message))});
		"WARNING" ->
			gen_server:call({global,?MODULE},{send,hd(Message),list_to_tuple(tl(Message))});
		"INFO" ->
			gen_server:call({global,?MODULE},{send,hd(Message),list_to_tuple(tl(Message))});
		"TRACE" ->
			gen_server:call({global,?MODULE},{send,hd(Message),list_to_tuple(tl(Message))});
		_ -> 
			io:format("Not valid type of message!~n")
	end.
			
		

validate_message(Message) ->
	case length(Message) of
		4 -> 
			true;
		_->
			false
	end.


validate(BinaryMsg) ->
	io:format("~p~n",[BinaryMsg]),
	ListOfStrings = string:tokens(binary_to_list(BinaryMsg),"|"),
	io:format("~p~n",[ListOfStrings]),
	[Head|_Tail] = ListOfStrings,
	case Head of
		"Subscribe" -> 	gen_server:call({global,?MODULE},{subscribe,lists:last(ListOfStrings)});
		"Unsubscribe" -> gen_server:call({global,?MODULE},{unsubscribe,lists:last(ListOfStrings)});
		_ -> io:format("Not valid format!~n")
	end.
	
is_user_subscribed(Username) ->
	Accounts = db:get_users(),
	case lists:member(Username,Accounts) of
			true ->
				true;
			false ->
				false
			
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


	
handle_call({send,Username,Message},_From,State) ->
	db:store_message_for_user(Username,Message),
	io:format("Message: ~p stored by user: ~p",[Message,Username]),
	{reply,ok,State};
	
handle_call(_Req,_From,State) ->
	{reply,ok,State}.
	
handle_cast({apps},State)->
	Apps = db:get_apps(),
	io:format("~p~n",[Apps]),
	{noreply,State};
	
handle_cast({logs,AppName},State)->
	Logs = db:get_mesagges(AppName),
	io:format("~p~n",[Logs]),
	{noreply,State};

handle_cast({logs,AppName,Type},State)->
	Logs = db:get_mesagges(AppName,Type),
	io:format("~p~n",[Logs]),
	{noreply,State};

handle_cast({logs,AppName,Type,N},State)->
	Logs = db:get_last_messages(AppName,Type,N),
	io:format("~p~n",[Logs]),
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
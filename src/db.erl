-module(db).
-export([store_message_for_user/2,get_users/0,get_messages/1,initDB/0,deleteDB/0,deleteDB2/0,get_apps/0,get_messages/2,get_messages/0,generate/1,generate_id/0,delete/0]).
-export([get_last_messages/3,get_last_n_messages/3,delete_user/1,get_user_by_username/1,user_in_app/2,store_user/2,get_users_in_app/0,store_message/1]).
-record(users,{username,message = [],app}).
-record(messages,{id,app_name,time_stamp,type,value}).
-include_lib("stdlib/include/qlc.hrl").
store_user(Name, App) ->
	io:format("Mnesia: ~p ~p ~n",[Name, App]),
	AF = fun()->
		mnesia:write(#users{username=Name,app=App})
	end,
	mnesia:transaction(AF).

get_user_by_username(Username) ->
		AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(users),
		X#users.username =:=  Username]),
		Results = qlc:e(Query),
		lists:map(fun(Item) -> Item#users.username end,Results)
	end,
	{atomic,Users} = mnesia:transaction(AF),
	Users.
store_message({App_name,Time_stamp,Type,Value}) ->
	AF = fun() ->
		mnesia:write(#messages{id=generate_id(),app_name = App_name,time_stamp=Time_stamp,type=Type,value=Value})
	end,
	mnesia:transaction(AF).
	
user_in_app(Username,App) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(users),
		X#users.username =:= Username,
		X#users.app =:= App]),
		
		Results = qlc:e(Query),
		lists:map(fun(Item) -> Item#users.username end,Results)
	end,
	{atomic,User} = mnesia:transaction(AF),
	User.	

delete_user(Name) ->
	AF = fun() ->
		Query = qlc:q([ X || X <- mnesia:table(users),
		X#users.username =:= Name]),
	Results = qlc:e(Query),
	F = fun() ->
		lists:foreach(fun(Result) -> mnesia:delete_object(Result) end,Results)
		end,
		mnesia:transaction(F)
	end,
	mnesia:transaction(AF).


store_message_for_user(Username,Msg) ->

	{App_name,Time_stamp,Type,Value} = Msg,
	AF = fun() ->
		Query = qlc:q([ X || X <- mnesia:table(users),
		X#users.username =:= Username]),
	User = qlc:e(Query),
	[{users,_Usrnme,Messages,_App}] = User,
	mnesia:write(#messages{id=generate_id(),app_name = App_name,time_stamp=Time_stamp,type=Type,value=Value}),
	mnesia:write(#users{username=Username,message=[Msg|Messages]})
	end,
	mnesia:transaction(AF).


	
get_users() ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(users)]),
		Results = qlc:e(Query),
		lists:map(fun(Item) -> Item#users.username end,Results)
	end,
	{atomic,Users} = mnesia:transaction(AF),
	Users.

get_users_in_app() ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(users)]),
		Results = qlc:e(Query),
		lists:map(fun(Item) -> {Item#users.username,Item#users.app} end,Results)
	end,
	{atomic,Users} = mnesia:transaction(AF),
	Users.
	
	
get_messages() ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(messages)]),
		
		Results = qlc:e(Query),
		lists:map(fun(Item) -> {Item#messages.id,Item#messages.app_name,Item#messages.time_stamp,Item#messages.type,Item#messages.value} end,Results)
	end,
	{atomic,Messages} = mnesia:transaction(AF),
	Messages.	
	
	
get_messages(App_name) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(messages),
		X#messages.app_name =:= App_name]),
		
		Results = qlc:e(Query),
		lists:map(fun(Item) -> {Item#messages.app_name,Item#messages.time_stamp,Item#messages.type,Item#messages.value} end,Results)
	end,
	{atomic,Messages} = mnesia:transaction(AF),
	Messages.

get_messages(App_name,Type) ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(messages),
		X#messages.app_name =:= App_name,
		X#messages.type =:= Type]),
		
		Results = qlc:e(Query),
		lists:map(fun(Item) -> {Item#messages.app_name,Item#messages.time_stamp,Item#messages.type,Item#messages.value} end,Results)
	end,
	{atomic,Messages} = mnesia:transaction(AF),
	Messages.

get_apps() ->
	AF = fun() ->
		Query = qlc:q([X || X <- mnesia:table(messages)]),
		
		Results = qlc:e(Query),
		lists:map(fun(Item) -> Item#messages.app_name end,Results)
	end,
	{atomic,Apps} = mnesia:transaction(AF),
	Apps.
	
	
deleteDB() ->
	AF = fun() ->
		Query = qlc:q([ X || X <- mnesia:table(users)]),
	Results = qlc:e(Query),
	F = fun() ->
		lists:foreach(fun(Result) -> mnesia:delete_object(Result) end,Results)
		end,
		mnesia:transaction(F)
	end,
	mnesia:transaction(AF).


deleteDB2() ->
	AF = fun() ->
		Query = qlc:q([ X || X <- mnesia:table(messages)]),
	Results = qlc:e(Query),
	F = fun() ->
		lists:foreach(fun(Result) -> mnesia:delete_object(Result) end,Results)
		end,
		mnesia:transaction(F)
	end,
	mnesia:transaction(AF).
	
	
generate_id() ->
	Messages = get_messages(),
	generate(Messages).
	
	

generate(List) ->
	case List of
		[] -> 1;
		_ -> A = lists:max(List),
			{Id,_app,_time,_type,_value} = A,
			Id+1
	end.
	
get_last_messages(App_name,Type,N)->
	Messages = get_messages(App_name,Type),
	get_last_n_messages(N,Messages,[]).

get_last_n_messages(0,_Messages,LastMsgs) ->
	LastMsgs;
	
get_last_n_messages(N,Messages,LastMsgs) ->
	Last = lists:max(Messages),
	get_last_n_messages(N-1,lists:delete(Last,Messages),[Last|LastMsgs]).


	

initDB() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(type,users),
		mnesia:table_info(type,messages)
		
	catch
		exit: _->
			mnesia:create_table(users,[{attributes,record_info(fields, users)},
			{type,set},
			{disc_copies,[node()]}]),
			
			mnesia:create_table(messages,[{attributes,record_info(fields, messages)},
			{type,set},
			{disc_copies,[node()]}])
			
	end.
	
delete() ->
	mnesia:delete_table(users),
	mnesia:delete_table(messages),
	ok.
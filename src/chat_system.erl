-module(chat_system).
-behaviour(application).

-export([start/2,stop/1,start/0,stop/0]).

start() ->
	application:start(chat_system).

start(normal,[]) ->
	chat_supervisor:start_link().

stop() ->
	application:stop(?MODULE).

stop(_State) ->
	ok.
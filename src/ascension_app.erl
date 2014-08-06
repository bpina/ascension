-module(ascension_app).

-behaviour(application).

-export([start/2, stop/1, init_db/0]).
-export([traverse_table_and_show/1]).

-include("ascension_records.hrl").

start(_StartType, _StartArgs) ->
  init_db(),
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowlib),
  ok = application:start(cowboy),
  ok = application:start(gproc),
  ok = application:start(mnesia),

  Dispatch = cowboy_router:compile([
    {'_', [
      {"/api/groups/[:id]", ascension_groups_api_handler, []},
      {"/api/sessions/[:id]", ascension_sessions_api_handler, []},
      {"/:csid", ascension_websocket_handler, []}
    ]}
  ]),

  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [{env, [{dispatch, Dispatch}]}]),

  ascension_chat_handler:start_link(),
  ascension_message_handler:start_link(),
  ascension_sup:start_link().

stop(_State) ->
    ok.

init_db() ->
  Nodes = [node()],

  io:format("creating schema on: ~p~n", [Nodes]),

  application:start(mnesia),

  CreateSchemaResult = mnesia:create_schema(Nodes),

  io:format("create schema resul: ~p~n", [CreateSchemaResult]),

  Result = mnesia:create_table(rooms, [{attributes, record_info(fields, rooms)},
                              {disc_copies, Nodes}]),

  io:format("create table result: ~p ~n", [Result]),

  Result2 = mnesia:create_table(chat_sessions, [
                              {attributes, record_info(fields, chat_sessions)},
                              {disc_copies, Nodes}]),

  io:format("create table result: ~p ~n", [Result2]),

  Result3 = mnesia:create_table(groups, [{attributes, record_info(fields, groups)},
                              {disc_copies, Nodes}]),

  io:format("create table result: ~p ~n", [Result3]),

  application:stop(mnesia).

traverse_table_and_show(Table_name)->
    Iterator =  fun(Rec,_)->
                    io:format("~p~n",[Rec]),
                    []
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false -> 
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.

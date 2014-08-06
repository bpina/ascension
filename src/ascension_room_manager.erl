-module(ascension_room_manager).

-include("ascension_records.hrl").

-export([find_room/1, create_room/2, join_room/2]).

find_room(Name) ->
  RoomId = crypto:hmac(sha, "ascension", Name),
  io:format("finding room: ~p~n", [RoomId]),

  F = fun() ->
    Rooms = mnesia:read({rooms, RoomId}),
    case Rooms of
      [Room|_] ->
        {ok, Room};
      _None ->
        not_found
    end
  end,

  mnesia:activity(transaction, F).

create_room(SessionId, Name) ->
  RoomId = crypto:hmac(sha, "ascension", Name),

  F = fun() ->
    mnesia:write(#rooms{id=RoomId, name=Name, sessions=[SessionId]})
  end,

  mnesia:activity(transaction, F).

join_room(SessionId, Name) ->
  Room = find_room(Name),

  io:format("joining room: ~p ~p~n", [Name,Room]),


  case Room of 
    {ok, {rooms, RoomId, RoomName, Sessions}} ->
      io:format("got room match"),
      F = fun() ->
        mnesia:write(#rooms{id=RoomId, name=RoomName, sessions=Sessions ++ [SessionId]})
      end,

      io:format("returning transaction"),
      mnesia:activity(transaction, F);
   _None ->
      not_found
  end.


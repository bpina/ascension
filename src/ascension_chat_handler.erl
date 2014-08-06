-module(ascension_chat_handler).

-export([start_link/0]).
-export([join_room/2]).
-export([notify_error/2]).
-export([message_room/2]).
-export([init/1, handle_cast/2, terminate/2]).
-export([send_reply/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join_room(UserData, UserState) ->
  gen_server:cast(?MODULE, {join_room, UserData, UserState}).

notify_error(UserState, Message) ->
  gen_server:cast(?MODULE, {error, UserState, Message}).

message_room(UserData, UserState) ->
  gen_server:cast(?MODULE, {message_room, UserData, UserState}).

init(_Args) ->
  {ok, undefined_state}.

send_reply(SessionId, JsonMessage) ->
  gproc:send({p, l, SessionId}, {self(), SessionId, JsonMessage}).

handle_cast({error, UserState, Message}, State) ->
  #{ session_id := SessionId } = UserState,

  ErrorMessage = {[{status, <<"error">>}, {message, list_to_binary(Message)}]},
  JsonMessage = jiffy:encode(ErrorMessage), 
  
  io:format("reporting error: ~p~n", [Message]),
  send_reply(SessionId, JsonMessage),
  {noreply, State};

handle_cast({join_room, UserData, UserState}, State) ->
  #{ <<"room">> := RoomName } = UserData,
  #{ session_id := SessionId } = UserState,

  Result = case ascension_room_manager:find_room(RoomName) of
    {ok, _Room} -> ascension_room_manager:join_room(SessionId, RoomName);
    not_found -> ascension_room_manager:create_room(SessionId, RoomName)
  end,


  case Result of
    ok ->
      MessageText = lists:concat(["Joined Room: ", binary_to_list(RoomName)]),
      SuccessMessage = {[{event, <<"joined_room">>},{status, <<"success">>}, {message, list_to_binary(MessageText)}]},
      JsonMessage = jiffy:encode(SuccessMessage),

      send_reply(SessionId, JsonMessage);

    _Failure ->
      notify_error(UserState, "Could not join or create room")
  end,

  {noreply, State};

handle_cast({message_room, UserData, UserState}, State) ->
  #{ session_id := UserSessionId } = UserState,
  #{ <<"room">> := RoomName, <<"username">> := UserName, <<"message">> := Message } = UserData,

  UserMessage = {[{event, <<"message_room">>}, {body, {[{from, UserName}, {message, Message}]}}]},
  JsonMessage = jiffy:encode(UserMessage),
  
  Result = ascension_room_manager:find_room(RoomName),
  io:format("find room returned: ~p~n", [Result]),
  {ok, {rooms, _RoomId, _Name, Sessions}} = Result,

  case Result of
    {ok, {rooms, _RoomId, _Name, Sessions}} ->
        io:format("delivering to sessions: ~p~n", [Sessions]),
        [send_reply(RoomSessionId, JsonMessage) || RoomSessionId <- Sessions];
    not_found ->
        notify_error(UserState, "Could not message room: " ++ binary_to_list(RoomName))
  end,

  {noreply, State}.

terminate(Reason, State) ->
  io:format("chat handler terminiating: ~p ~p~n", [Reason, State]).

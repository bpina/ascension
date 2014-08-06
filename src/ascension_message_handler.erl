-module(ascension_message_handler).

-export([start_link/0]).
-export([handle_message/2]).
-export([init/1, handle_cast/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

handle_message(Message, UserState) ->
  gen_server:cast(?MODULE, {Message, UserState}).

init(_Args) ->
  {ok, undefined_state}.

handle_cast({Message, UserState}, State) ->
  io:format("handling message: ~p~n", [Message]),
  {ParsedMessage} = jiffy:decode(Message),  
  MessageMap = maps:from_list(ParsedMessage),
  #{ <<"body">> := {BodyString} } = MessageMap,
  BodyHash = maps:from_list(BodyString),

  case MessageMap of
    #{ <<"message">> := <<"join_room">> } ->
      io:format("got join room message"),
      ascension_chat_handler:join_room(BodyHash, UserState);
    #{ <<"message">> := <<"message_room">> } ->
      io:format("messaging to room"),
      ascension_chat_handler:message_room(BodyHash, UserState);
    _Ignore ->
      io:format("got bad message"),
      ascension_chat_handler:notify_error(UserState, "Unrecognized Message")
  end,

  {noreply, State}.

-module(ascension_session_manager).

-export([create_session/2]).
-export([find_session/1]).

-include("ascension_records.hrl").

create_session(ChatSessionId, UserId) ->
  ChatSession = #chat_sessions{id=ChatSessionId, user_id=UserId},

  F = fun() ->
    mnesia:write(ChatSession)
  end,

  mnesia:activity(transaction, F).

find_session(Id) ->
  F = fun(SessionId) ->
    case mnesia:read({chat_sessions, SessionId}) of
      [ChatSession|_] ->
        {ok, ChatSession};
      _ ->
        not_found
    end
  end,

  mnesia:activity(transaction, F, [Id]).

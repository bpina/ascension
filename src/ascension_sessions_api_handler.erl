-module(ascension_sessions_api_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([get_json/2]).
-export([put_json/2]).

-include("ascension_records.hrl").

init(_Transport, _Req, _State) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>,<<"json">>, []}, put_json}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>,<<"json">>, []}, get_json}], Req, State}.

put_json(Req, State) ->
  {ReqMethod, Req2} = cowboy_req:method(Req),
  {ok, ReqBody, Req3} = cowboy_req:body(Req2),

  case ReqMethod of
   <<"POST">> ->
      
     {ParsedBody} = jiffy:decode(ReqBody),
     Data = maps:from_list(ParsedBody),

     #{ <<"user_id">> := UserId } = Data,

     ChatSessionId = uuid:to_string(uuid:uuid5(uuid:uuid4(), binary_to_list(crypto:rand_bytes(32)))),

     io:format("got session id: ~s~n", [ChatSessionId]),

     ascension_session_manager:create_session(list_to_binary(ChatSessionId), UserId),

     {HostUrl, Req4} = cowboy_req:host_url(Req3),

     ResourceUrl = binary_to_list(HostUrl) ++ "/api/sessions/" ++ ChatSessionId,

     io:format("redirecting to: ~s~n", [ResourceUrl]),

     {{true, ResourceUrl}, Req4, State};

   _ ->
     {false, Req3, State}
  end.

get_json(Req, State) ->
  {ReqMethod, Req2} = cowboy_req:method(Req),
  {Id, Req3} = cowboy_req:binding(id, Req2),

  case {Id, ReqMethod} of
    {undefined, _Method} ->
      {false, Req2, State};

    {Id, <<"GET">>} ->
      {ok, {chat_sessions, SessionId, UserId}} = ascension_session_manager:find_session(Id),

      io:format("sending session id: ~s~n", [SessionId]),

      JsonMessage = jiffy:encode({[{<<"id">>, SessionId}, {user_id, UserId}]}),

      {JsonMessage, Req3, State};

    _ ->
      {false, Req2, State}
  end.

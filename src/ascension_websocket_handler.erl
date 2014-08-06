-module(ascension_websocket_handler).
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _options) -> 
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) -> 
  {Csid, Req2} = cowboy_req:binding(csid, Req),

  gproc:reg({p, l, Csid}),

  State = #{ session_id => Csid },

  Req3 = cowboy_req:compact(Req2),
  {ok, Req3, State}.

websocket_handle({text, Message}, Req, State) ->
  ascension_message_handler:handle_message(Message, State),

  {reply, {text, <<"Ack">>}, Req, State};

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, Message}, Req, State) ->
  {reply, {text, Message}, Req, State};

websocket_info({_PID, _SessionId, Message}, Req, State) ->
  {reply, {text, Message}, Req, State};

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

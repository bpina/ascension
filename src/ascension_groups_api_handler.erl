-module(ascension_groups_api_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([get_json/2]).
-export([post_json/2]).

-include("ascension_records.hrl").

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, post_json}], Req, State}.

post_json(Req, State) ->
  {ReqMethod, Req2} = cowboy_req:method(Req),
  {ok, ReqBody, Req3} = cowboy_req:body(Req2),

  case ReqMethod of
    <<"POST">> ->
      {JsonData} = jiffy:decode(ReqBody),
      Data = maps:from_list(JsonData),

      #{ <<"name">> := Name, <<"owner_id">> := OwnerId } = Data,

      GroupId = uuid:to_string(uuid:uuid5(uuid:uuid4(), binary_to_list(crypto:rand_bytes(32)))),

      ascension_group_manager:create_group(GroupId, OwnerId, Name),

      {ReqUrl, Req4} = cowboy_req:host_url(Req3),

      RedirectUrl = binary_to_list(ReqUrl) ++ "/api/groups/" ++ GroupId,

      {{true, RedirectUrl}, Req4, State};

    _ ->
      {false, Req3, State}
  end.



get_json(Req, State) ->
  {ReqMethod, Req2} = cowboy_req:method(Req),
  {Id, Req3} = cowboy_req:binding(id, Req2),

  case {Id, ReqMethod} of
    {undefined, _} ->
      {<<"nogood">>, Req3, State};

    {Id, <<"GET">>} ->
      {ok, Group} = ascension_group_manager:find_group(binary_to_list(Id)),

      JsonMessage = ascension_group_manager:group_to_json(Group),

      {JsonMessage, Req3, State};

    _ ->
      {<<"nogood">>, Req3, State}
  end.

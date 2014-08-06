-module(ascension_group_manager).

-export([create_group/3]).
-export([find_group/1]).
-export([group_to_json/1]).

-include("ascension_records.hrl").

create_group(GroupId, Name, OwnerId) ->
  NewGroup = #groups{id=GroupId, name=Name, owner_id=OwnerId},

  F = fun(Group) ->
    mnesia:write(Group)
  end,

  mnesia:activity(transaction, F, [NewGroup]).

find_group(Id) ->
  F = fun(GroupId) ->
    case mnesia:read({groups, GroupId}) of
      [Group|_] ->
        {ok, Group};
      _ ->
        not_found
    end
  end,

  mnesia:activity(transaction, F, [Id]).

group_to_json(Group) ->
  #groups{id=GroupId, name=Name, owner_id=OwnerId, users=Users} = Group,

  Data = {[{id, GroupId}, {name, Name}, {owner_id, OwnerId}, {users, Users}]},

  jiffy:encode(Data).

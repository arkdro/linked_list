-module(lst_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [
          {group, lst}
         ].

groups() ->
    [
     {lst, [], [
                new,
                insert,
                delete,
                lookup
               ]}
    ].

new(_Config) ->
    [] = lst:new().

insert(_Config) ->
    L0 = lst:new(),
    L1 = lst:insert(L0, 1),
    L1 = [1],
    L2 = lst:insert(L1, 2),
    L2 = [2,1],
    ok.

delete(_Config) ->
    ok.

lookup(_Config) ->
    ok.


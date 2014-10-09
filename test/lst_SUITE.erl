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
                is_empty,
                head,
                tail,
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
    [] = lst:delete([], 2),
    [2, 3] = lst:delete([1, 2, 3], 1),
    [1, 3] = lst:delete([1, 2, 3], 2),
    [1, 2] = lst:delete([1, 2, 3], 3),
    [1, 2, 3] = lst:delete([1, 2, 3], 4),
    [1, 2, 3] = lst:delete([1, 2, 2, 3], 2),
    [1, 2, 3, 5, 4] = lst:delete([1, 2, 3, 4, 5, 4], 4),
    ok.

lookup(_Config) ->
    false = lst:lookup([], 2),
    true = lst:lookup([1, 2, 3], 1),
    true = lst:lookup([1, 2, 3], 2),
    true = lst:lookup([1, 2, 3], 3),
    false = lst:lookup([1, 2, 3], 4),
    true = lst:lookup([1, 2, 2, 3], 2),
    false = lst:lookup([1, 2, 3, 4, 5, 4], 6),
    ok.

is_empty(_Config) ->
    true = lst:is_empty(lst:new()),
    false = lst:is_empty(lst:insert(lst:new(), 1)),
    ok.

head(_Config) ->
    ok.

tail(_Config) ->
    ok.


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
                concat,
                reverse_iter,
                is_empty,
                head,
                tail,
                is_member
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

is_member(_Config) ->
    false = lst:is_member([], 2),
    true = lst:is_member([1, 2, 3], 1),
    true = lst:is_member([1, 2, 3], 2),
    true = lst:is_member([1, 2, 3], 3),
    false = lst:is_member([1, 2, 3], 4),
    true = lst:is_member([1, 2, 2, 3], 2),
    false = lst:is_member([1, 2, 3, 4, 5, 4], 6),
    ok.

is_empty(_Config) ->
    true = lst:is_empty(lst:new()),
    false = lst:is_empty(lst:insert(lst:new(), 1)),
    ok.

head(_Config) ->
    try
        lst:head(lst:new()),
        erlang:error(should_not_happen)
    catch error:function_clause ->
            ok
    end,
    L1 = lst:insert(lst:new(), 1),
    L2 = lst:insert(L1, 2),
    1 = lst:head(L1),
    2 = lst:head(L2),
    ok.

tail(_Config) ->
    try
        lst:tail(lst:new()),
        erlang:error(should_not_happen)
    catch error:function_clause ->
            ok
    end,
    L1 = lst:insert(lst:new(), 1),
    L2 = lst:insert(L1, 2),
    L3 = lst:insert(L2, 3),
    [] = lst:tail(L1),
    [1] = lst:tail(L2),
    [2,1] = lst:tail(L3),
    ok.

concat(_Config) ->
    L1 = lst:insert(lst:new(), 1),
    L2 = lst:insert(L1, 2),
    L3 = lst:insert(L2, 3),
    L4 = lst:insert(L3, 4),
    [3,2,1, 2,1] = lst:concat(L3, L2),
    [4,3,2,1, 2,1] = lst:concat(L4, L2),
    [4,3,2,1] = lst:concat(L4, lst:new()),
    [3,2,1] = lst:concat(lst:new(), L3),
    ok.

reverse_iter(_Config) ->
    L1 = lst:insert(lst:new(), 1),
    L2 = lst:insert(L1, 2),
    L3 = lst:insert(L2, 3),
    L4 = lst:insert(L3, 4),
    [] = lst:reverse_iter(lst:new()),
    [1] = lst:reverse_iter(L1),
    [1,2,3] = lst:reverse_iter(L3),
    [1,2,3,4] = lst:reverse_iter(L4),
    ok.


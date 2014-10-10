-module(lp_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() -> [
          {group, lp}
         ].

groups() ->
    [
     {lp, [], [
                insert,
                %% delete,
                %% concat,
                %% reverse_iter,
                %% reverse_recur,
                %%is_empty,
                %% head,
                %% tail,
                %% is_member,
                new
               ]}
    ].

new(_Config) ->
    {Tab, Head} = lp:new(),
    1 = ets:info(Tab, size),
    %% ct:pal("tab: ~p", [ets:tab2list(Tab)]),
    Expected = lp:new_item(lp:terminal(), undefined),
    [{_, Expected}] = ets:lookup(Tab, Head),
    ok.

insert(_Config) ->
    L0 = lp:new(),
    {Tab, Head0} = L0,
    L1 = lp:insert(L0, 1),
    {_, Head1} = L1,
    L2 = lp:insert(L1, 2),
    {_, Head2} = L2,
    3 = ets:info(Tab, size),
    Expected2 = lp:new_item(2, Head1),
    [{_, Expected2}] = ets:lookup(Tab, Head2),
    Expected1 = lp:new_item(1, Head0),
    [{_, Expected1}] = ets:lookup(Tab, Head1),
    Expected0 = lp:new_item(lp:terminal(), undefined),
    [{_, Expected0}] = ets:lookup(Tab, Head0),
    ok.

delete(_Config) ->
    [] = lp:delete([], 2),
    [2, 3] = lp:delete([1, 2, 3], 1),
    [1, 3] = lp:delete([1, 2, 3], 2),
    [1, 2] = lp:delete([1, 2, 3], 3),
    [1, 2, 3] = lp:delete([1, 2, 3], 4),
    [1, 2, 3] = lp:delete([1, 2, 2, 3], 2),
    [1, 2, 3, 5, 4] = lp:delete([1, 2, 3, 4, 5, 4], 4),
    ok.

is_member(_Config) ->
    false = lp:is_member([], 2),
    true = lp:is_member([1, 2, 3], 1),
    true = lp:is_member([1, 2, 3], 2),
    true = lp:is_member([1, 2, 3], 3),
    false = lp:is_member([1, 2, 3], 4),
    true = lp:is_member([1, 2, 2, 3], 2),
    false = lp:is_member([1, 2, 3, 4, 5, 4], 6),
    ok.

is_empty(_Config) ->
    true = lp:is_empty(lp:new()),
    false = lp:is_empty(lp:insert(lp:new(), 1)),
    ok.

head(_Config) ->
    try
        lp:head(lp:new()),
        erlang:error(should_not_happen)
    catch error:function_clause ->
            ok
    end,
    L1 = lp:insert(lp:new(), 1),
    L2 = lp:insert(L1, 2),
    1 = lp:head(L1),
    2 = lp:head(L2),
    ok.

tail(_Config) ->
    try
        lp:tail(lp:new()),
        erlang:error(should_not_happen)
    catch error:function_clause ->
            ok
    end,
    L1 = lp:insert(lp:new(), 1),
    L2 = lp:insert(L1, 2),
    L3 = lp:insert(L2, 3),
    [] = lp:tail(L1),
    [1] = lp:tail(L2),
    [2,1] = lp:tail(L3),
    ok.

concat(_Config) ->
    L1 = lp:insert(lp:new(), 1),
    L2 = lp:insert(L1, 2),
    L3 = lp:insert(L2, 3),
    L4 = lp:insert(L3, 4),
    [3,2,1, 2,1] = lp:concat(L3, L2),
    [4,3,2,1, 2,1] = lp:concat(L4, L2),
    [4,3,2,1] = lp:concat(L4, lp:new()),
    [3,2,1] = lp:concat(lp:new(), L3),
    ok.

reverse_iter(_Config) ->
    L1 = lp:insert(lp:new(), 1),
    L2 = lp:insert(L1, 2),
    L3 = lp:insert(L2, 3),
    L4 = lp:insert(L3, 4),
    [] = lp:reverse_iter(lp:new()),
    [1] = lp:reverse_iter(L1),
    [1,2,3] = lp:reverse_iter(L3),
    [1,2,3,4] = lp:reverse_iter(L4),
    ok.

reverse_recur(_Config) ->
    L1 = lp:insert(lp:new(), 1),
    L2 = lp:insert(L1, 2),
    L3 = lp:insert(L2, 3),
    L4 = lp:insert(L3, 4),
    [] = lp:reverse_recur(lp:new()),
    [1] = lp:reverse_recur(L1),
    [1,2,3] = lp:reverse_recur(L3),
    [1,2,3,4] = lp:reverse_recur(L4),
    ok.


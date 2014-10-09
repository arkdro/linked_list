-module(lst).

-export([
         new/0,
         insert/2,
         delete/2,
         lookup/2
        ]).

new() ->
    [].

insert(L, Val) ->
    [Val | L].

delete(L, Val) ->
    delete(new(), L, Val).

delete(Start, [], _) ->
    reverse(Start);
delete(Start, [Val | T], Val) ->
    concat(reverse(Start), T);
delete(Start, [H | T], Val) ->
    delete(insert(Start, H), T, Val).

lookup(L, Val) ->
    false.

reverse(L) ->
    %% stub
    lists:reverse(L).

concat(L1, L2) ->
    %% stub
    L1 ++ L2.


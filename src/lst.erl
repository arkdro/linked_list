-module(lst).

-export([
        ]).

new() ->
    [].

insert(L, Val) ->
    [Val | L].

delete(L, Val) ->
    L.

lookup(L, Val) ->
    false.


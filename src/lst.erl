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
    L.

lookup(L, Val) ->
    false.


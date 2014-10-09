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

delete(Start, L, Val) ->
    case is_empty(L) of
        true ->
            reverse(Start);
        false ->
            delete_from_non_empty(Start, L, Val)
    end.

delete_from_non_empty(Start, L, Val) ->
    Head = head(L),
    Tail = tail(L),
    if Head =:= Val ->
            concat(reverse(Start), Tail);
       true ->
            delete(insert(Start, Head), Tail, Val)
    end.

lookup(L, Val) ->
    false.

reverse(L) ->
    %% stub
    lists:reverse(L).

concat(L1, L2) ->
    %% stub
    L1 ++ L2.

is_empty([]) ->
    true;
is_empty(_) ->
    false.

head([H | _]) ->
    H.

tail([_ | T]) ->
    T.


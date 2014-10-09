-module(lst).

-export([
         new/0,
         insert/2,
         delete/2,
         is_empty/1,
         head/1,
         tail/1,
         reverse/1,
         concat/2,
         is_member/2
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
            Head = head(L),
            Tail = tail(L),
            if Head =:= Val ->
                    concat(reverse(Start), Tail);
               true ->
                    delete(insert(Start, Head), Tail, Val)
            end
    end.

is_member(L, Val) ->
    case is_empty(L) of
        true ->
            false;
        false ->
            Head = head(L),
            Tail = tail(L),
            if Head =:= Val ->
                    true;
               true ->
                    is_member(Tail, Val)
            end
    end.

reverse(L) ->
    %% stub
    lists:reverse(L).

concat(L1, L2) ->
    R1 = reverse(L1),
    concat_priv(R1, L2).

concat_priv(R1, L2) ->
    case is_empty(R1) of
        true ->
            L2;
        false ->
            Head = head(R1),
            Tail = tail(R1),
            concat_priv(Tail, insert(L2, Head))
    end.

is_empty([]) ->
    true;
is_empty(_) ->
    false.

head([H | _]) ->
    H.

tail([_ | T]) ->
    T.


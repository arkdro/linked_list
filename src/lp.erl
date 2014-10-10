-module(lp).

-export([
         terminal/0,
         new_item/2,
         new/0,
         insert/2,
         delete/2,
         is_empty/1,
         head/1,
         tail/1,
         reverse/1,
         reverse_iter/1,
         reverse_recur/1,
         concat/2,
         is_member/2
        ]).

-define(ST, ?MODULE).
-record(i, {
          val :: term(), %% '$end' means terminal node
          next :: reference()
         }).

terminal() ->
    '$end_of_table'.

prepare() ->
    ets:new(?ST, [protected]).

clean(Tab) ->
    ets:delete(Tab).

new() ->
    Tab = prepare(),
    insert({Tab, undefined}, terminal()).

new_item(Val, Next) ->
    #i{val=Val, next=Next}.

insert({Tab, Head}, Val) ->
    New_head = make_ref(),
    Item = new_item(Val, Head),
    ets:insert(Tab, {New_head, Item}),
    {Tab, New_head}.

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
    reverse_iter(L).

reverse_iter(L) ->
    reverse_iter_priv(new(), L).

reverse_iter_priv(Acc, L) ->
    case is_empty(L) of
        true ->
            Acc;
        false ->
            Head = head(L),
            Tail = tail(L),
            reverse_iter_priv(insert(Acc, Head), Tail)
    end.

reverse_recur(L) ->
    case is_empty(L) of
        true ->
            new();
        false ->
            Head = head(L),
            Tail = tail(L),
            insert_to_end(reverse_recur(Tail), Head)
    end.

insert_to_end(L, Val) ->
    L2 = insert(new(), Val),
    concat(L, L2).

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

is_empty(L) ->
    Terminal = terminal(),
    case ets:lookup(?ST, L) of
        [{_, Terminal}] ->
            true;
        [_] ->
            false
    end.

head([H | _]) ->
    H.

tail([_ | T]) ->
    T.


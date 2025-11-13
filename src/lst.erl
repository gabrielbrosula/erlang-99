-module(lst).
-export([
    last/1, 
    penultimate/1,
    kth/2, 
    len/1, 
    reverse/1, 
    is_palindrome/1, 
    flatten/1,
    compress/1, 
    pack/1
]).

% P01
last([]) ->
    msg_empty_list();
last([A]) -> A;
last([_|T]) -> last(T).

% P02
penultimate([]) ->
    msg_empty_list();
penultimate([_]) ->
    io:format("Only one item in list");
penultimate([A,_]) -> A;
penultimate([_|T]) -> penultimate(T).

% P03
kth([], _) ->
    msg_empty_list();
kth(L, K) ->
    if 
        0 < K, K =< length(L) -> k_helper(L, K, 0);
        true -> io:format("Index out of bounds~n")
    end.

k_helper([H|T], K, C) ->
    if 
        K =:= C -> H;
        true -> k_helper(T, K, C+1)
    end.

% P04
len(L) -> len_helper(L, 0).

len_helper([], C) -> C;
len_helper([_|T], C) -> len_helper(T, C+1).

% P05
reverse(L) -> rev_helper(L, []).

rev_helper([], Acc) -> Acc; 
rev_helper([H|T], Acc) -> rev_helper(T,[H|Acc]).
    
% P06
is_palindrome(L) -> L =:= reverse(L).

% P07
flatten(L) -> flatten_helper(L, []).

flatten_helper([], Acc) -> Acc;
flatten_helper(L, Acc) ->
    [H|T]=L,
    if 
        is_list(H) -> 
            FlatHead = flatten_helper(H, []),
            flatten_helper(T, Acc ++ FlatHead);
        true ->
            flatten_helper(T, Acc ++ [H])
    end.

% P08
% 
% Prepend the last-known "different" element to the accumulator.
% Reverse the accumulator in the end.
compress(L) -> reverse(compress_helper(L, [])).

compress_helper([], _) -> [];
compress_helper([A], []) -> [A];
compress_helper([Head | Tail], []) -> compress_helper(Tail, [Head]);
compress_helper([A], Acc) -> 
    [HeadOfAcc | _] = Acc,
    if 
        A == HeadOfAcc -> Acc; 
        true -> [A | Acc]
    end;
compress_helper([Head | Tail], Acc) ->
    [HeadOfAcc|_] = Acc,
    if 
        Head == HeadOfAcc -> compress_helper(Tail, Acc);
        true -> compress_helper(Tail, [Head | Acc])
    end.

% P09
% 
% Prepend a sublist containing the different element to the accumulator,
% otherwise append new element to the head of the accumulator.
% Reverse the accumulator in the end.
pack(L) -> reverse(pack_helper(L, [])).

pack_helper([], _) -> [];
pack_helper([A], []) -> [[A]];
pack_helper([Head | Tail], []) -> pack_helper(Tail, [[Head]]);
pack_helper([A], Acc) -> 
    [[Elem] = HeadOfAcc | _] = Acc,
    if 
        A == Elem -> [HeadOfAcc ++ [A] | Acc]; 
        true -> [[A] | Acc]
    end;
pack_helper([Head | Tail], Acc) ->
    [[Elem] = HeadOfAcc|_] = Acc,
    if 
        % Append new element to head of accumulator
        Head == Elem -> pack_helper(Tail, [HeadOfAcc ++ [Head] | Acc]);

        % Prepend a sublist with the new element
        true -> pack_helper(Tail, [[Head] | Acc])
    end.














% General utility functions
msg_empty_list() -> io:format("Empty list~n").

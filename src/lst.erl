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
    pack/1,
    encode/1,
<<<<<<< HEAD
    encode_modified/1
=======
    encode_modified/1,
    decode/1,
    encode_direct/1,
    duplicate/1,
    duplicate_direct/1,
    duplicate_n/2,
    drop/2,
    split/2,
    slice/3
>>>>>>> main
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

pack_helper([], Acc) -> Acc;
pack_helper([A], []) -> [[A]];
pack_helper([Head | Tail], []) -> pack_helper(Tail, [[Head]]);
pack_helper([Head | Tail], Acc) ->
    [HeadOfAcc|Tacc] = Acc,
    [Elem|_] = HeadOfAcc,
    if 
        % Append new element to head of accumulator
        Head == Elem -> pack_helper(Tail, [[Head|HeadOfAcc] | Tacc]);

        % Prepend the sublist with the new element
        true -> pack_helper(Tail, [[Head] | Acc])
    end.

% P10
encode(L) -> [{len(X), Val} || [Val|_] = X <- pack(L)].


% P11
encode_modified(L) -> [ get_modified_result(Tuple) || Tuple <- encode(L)].
<<<<<<< HEAD

get_modified_result({1, Val}) -> Val;
get_modified_result({Count, Val}) -> {Count, Val}.
=======

get_modified_result({1, Val}) -> Val;
get_modified_result({Count, Val}) -> {Count, Val}.

% P12
decode(L) -> reverse(decode_helper(L, [])).

decode_helper([], Acc) -> Acc;
decode_helper([H|T], Acc) -> 
    {Count, Val} = H,
    NewAcc = construct_list(Count, Val, Acc),
    decode_helper(T, NewAcc).

construct_list(Count, Val, Acc) ->
    if 
        Count > 0 -> construct_list(Count - 1, Val, [Val|Acc]);
        true -> Acc
    end.

% P13
% 
% TODO: Implement direct solution for run-length encoding
encode_direct(_) -> 'Not implemented yet'.


% P14
duplicate(L) -> flatten([[X,X] || X <- L]).

duplicate_direct(L) -> reverse(dupe_helper(L, [])).

dupe_helper([], Acc) -> Acc;
dupe_helper([H|T], Acc) -> dupe_helper(T,[H|[H|Acc]]).

% P15
%
% TODO: Implement direct solution for duplicating n times 
duplicate_n(N, L) -> flatten([construct_list(N, Val, []) || Val <- L]).


% P16
drop(N, L) -> reverse(drop_helper(N, L, [], 1)).

drop_helper(_, [], Acc, _) -> Acc;
drop_helper(N, [H|T], Acc, Count) ->
    if 
        Count == N ->
            drop_helper(N, T, Acc, 1);
        true ->
            drop_helper(N, T, [H|Acc], Count + 1)
    end.
>>>>>>> main

% P17
split(FirstListLength, [_|_]=L) -> split_helper(FirstListLength, L, [], 0).

% Iterate the list L FirstListLength times to build the accumulator.
% Once you reach Count == FirstListLength, return [Acc, Rest of list at that point]
split_helper(FirstListLength, L, Acc, Count) when Count == FirstListLength ->
    [reverse(Acc),L];
split_helper(FirstListLength, [H|T], Acc, Count) when Count < FirstListLength ->
    split_helper(FirstListLength, T, [H|Acc], Count + 1).
    

% P18
slice(Start, End, _) when Start > End -> io:format("Invalid Arguments. Start is greater than End.~n");
slice(Start, End, L) -> reverse(slice_helper(Start, End, L, [], 0)).

slice_helper(_, End, _, Acc, Idx) when Idx == End -> Acc;
slice_helper(Start, End, [_|T], Acc, Idx) when Idx < Start -> 
    slice_helper(Start, End, T, Acc, Idx + 1);
slice_helper(Start, End, [H|T], Acc, Idx) when Idx >= Start, Idx < End ->
     slice_helper(Start, End, T, [H|Acc], Idx + 1).

% P19



% General utility functions
msg_empty_list() -> io:format("Empty list~n").

-module(recursion).

-export([factorial/1,
         tail_fac/1,
         fibonacci/1,
         fibonacci_tail/1,
         fib_tail/1,
         fib_list/1,
         gen_list/1,
         len/1,
         len_tail/1,
         reverse/1,
         tail_reverse/1,
         sublist/2,
         tail_sublist/2,
         zip/2,
         lenient_zip/2,
         lenient_zip_tail/2]).


% apparently, a float is an integer according to is_integer/1 ...
% solve with adding check for not float.
factorial(N) when is_integer(N), not is_float(N) ->
  if N =:= 0 -> 1;
     N > 0   -> N * factorial(N - 1);
     N < 0   -> "Error: Don't use negative numbers!"
  end.

% tail recursion of factorial
tail_fac(N) -> tail_fac(N,1).
tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc).


% get number in fibonnaci series (not using tail-recursion)
fibonacci(N) when is_integer(N), not is_float(N) ->
  if N =:= 1; N =:= 2 -> 1;
     N =:= 0          -> 0;
     N > 2            -> fibonacci(N-1) + fibonacci(N-2);
     N < 0            -> "Error: Don't use negative numbers!"
  end.


% helper function for tail recursive variant of fibonacci
 fibonacci_tail(A, B, N) when N >= 0 ->
   if N =:= 0 -> A;
      N > 0   -> fib_tail(B, A+B, N-1) % we count up to N+1 but return the value for N
   end.

% fibonacci with tail recursion (much faster for large numbers)
fibonacci_tail(N) when is_integer(N), not is_float(N), N >= 0 ->
  fibonacci_tail(0, 1, N).

% shorter way to write the above function:
fib_tail(A, _, 0) -> A;
fib_tail(A, B, N) -> fib_tail(B, A+B, N-1). % we count up to N+1 but return the value for N
fib_tail(N) when is_integer(N), not is_float(N), N >= 0 -> fib_tail(0, 1, N).


% helper function for the fibonacci list function
append_fib(List, N) ->
  if N =:= 0 -> List;
     N =/= 0 -> append_fib([fibonacci_tail(N) | List], N-1)
  end.

% returns a list of fibonacci numbers up to number N
fib_list(N) when is_integer(N), not is_float(N), N >= 0 ->
  append_fib([], N).


% generate a list with increasing values
gen_list(L, N) when N =:= 0 -> L;
gen_list(L, N) when N > 0 -> gen_list([N | L], N - 1);
gen_list(L, N) when N < 0 -> gen_list([N | L], N + 1).
gen_list(N) -> gen_list([], N). % public function


% Calculating the length of a list is easy!
% At first I tried to overcomplicate things with
% something like this (This does not work, I realized the idea was bad before I finished it):

% len(List, Count) when Count > 0 ->
%   if Count =:= 0 -> 0;
%      Count > 1   -> len(tl(List), Count + 1)
%   end.
%
% len(List) when is_list(List) ->
%   len(List, 0).

% All you need is this:
len([]) -> 0;
len([_|T]) -> 1 + len(T).

% Or, using tail recursion:
len_tail([], Count) -> Count;                   % helper function
len_tail([_|T], Count) -> len_tail(T, Count+1). % helper function
len_tail(L) when is_list(L) -> len_tail(L, 0).  % public function


% reverse a list (lists:reverse/1 does the same, but faster (thanks to being written in C))
reverse([]) -> [];
reverse([H|T]) -> reverse(T)++[H].

% reverse a list with tail recursion
tail_reverse(L) -> tail_reverse(L,[]). % public function
tail_reverse([],Acc) -> Acc;
tail_reverse([H|T],Acc) -> tail_reverse(T, [H|Acc]).

% return a subset of a list
% e.g. sublist([1,2,3,4,5], 3) will return [1,2,3]
sublist(_,0) -> [];
sublist([],_) -> [];
sublist([H|T],N) when N > 0 -> [H|sublist(T,N-1)].

% tail-recursive variant
% Note that this would return [3,2,1] if
% we did not reverse the list in the end.
tail_sublist(L, N) -> reverse(tail_sublist(L, N, [])). % public function
tail_sublist(_, 0, SubList) -> SubList;
tail_sublist([], _, SubList) -> SubList;
tail_sublist([H|T], N, SubList) when N > 0 -> tail_sublist(T, N-1, [H|SubList]).


% make a list of tuples based on two lists
% e.g. zip([a,b,c],[1,2,3]) will return [{a,1},{b,2},{c,3}]
zip([], []) -> [];
zip([X|Xs], [Y|Ys]) -> [{X, Y} | zip(Xs, Ys)].

% same as above, but it stops whenever one of the lists is done
% e.g. [a,b,c] and [1,2] would yield [{a,1},{b,2}]
lenient_zip([], _) -> [];
lenient_zip(_, []) -> [];
lenient_zip([X|Xs], [Y|Ys]) -> [{X, Y} | lenient_zip(Xs, Ys)].

% same as above, but with tail recursion
lenient_zip_tail([], _, L) -> L;
lenient_zip_tail(_, [], L) -> L;
lenient_zip_tail([X|Xs], [Y|Ys], L) -> lenient_zip_tail(Xs, Ys, [{X,Y} | L]).
lenient_zip_tail(X, Y) -> reverse(lenient_zip_tail(X, Y, [])).

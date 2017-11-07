-module(recursion).

-export([factorial/1,
         fibonacci/1,
         fibonacci_tail/1,
         fib_list/1]).


% apparently, a float is an integer according to is_integer/1 ...
% solve with adding check for not float.
factorial(N) when is_integer(N), not is_float(N) ->
  if N =:= 0 -> 1;
     N > 0   -> N * factorial(N - 1);
     N < 0   -> "Error: Don't use negative numbers!"
  end.


% get number in fibonnaci series (not using tail-recursion)
fibonacci(N) when is_integer(N), not is_float(N) ->
  if N =:= 1; N =:= 2 -> 1;
     N =:= 0          -> 0;
     N > 2            -> fibonacci(N-1) + fibonacci(N-2);
     N < 0            -> "Error: Don't use negative numbers!"
  end.


% helper function for tail recursive variant of fibonacci
fib_tail(A, B, N) when N >= 0 ->
  if N =:= 0 -> A;
     N > 0   -> fib_tail(B, A+B, N-1) % we count up to N+1 but return the value for N
  end.

% fibonacci with tail recursion (much faster for large numbers)
fibonacci_tail(N) when is_integer(N), not is_float(N), N >= 0 ->
  fib_tail(0, 1, N).


% helper function for the fibonacci list function
append_fib(List, N) ->
  if N =:= 0 -> List;
     N =/= 0 -> append_fib([fibonacci(N) | List], N-1)
  end.

% returns a list of fibonacci numbers up to number N
fib_list(N) when is_integer(N), not is_float(N), N >= 0 ->
  append_fib([], N).

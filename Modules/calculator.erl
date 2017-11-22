
-module(calculator).

-export([rpn/1,
         rpn_test/0]).

% Reverse Polish Notation (RPN) has the operator as a suffix. (ex: "1 + 1" becomes "1 1 +")
% "(1 + 2) * (3 + 4)" would then be written as: "1 2 + 3 4 + *"
% and "1 + 2 * (3 + 4)" as "1 2 3 4 + * +"   (polish logic best logic)
%
% This is useful because you can store elements in a stack (in Erlang, a list works just
% like a stack). And whenever an operator comes up, the elements are simply popped from
% the stack to perform the calculation.

% Note: In Erlang, a string is a list. Therefore "1 + 1" is interpreted as a list.

% calculator function
% example usage:
% rpn("1 2 + 3 4 + *"). = 21
rpn(L) when is_list(L) ->
  [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
  Res.

rpn("+", [N1,N2|S]) -> [N2+N1|S];
rpn("-", [N1,N2|S]) -> [N2-N1|S];
rpn("*", [N1,N2|S]) -> [N2*N1|S];
rpn("/", [N1,N2|S]) -> [N2/N1|S];
rpn("^", [N1,N2|S]) -> [math:pow(N2,N1)|S];
rpn("ln", [N|S])    -> [math:log(N)|S];
rpn("log10", [N|S]) -> [math:log10(N)|S];
rpn("sum", [N|S]) -> [lists:foldl(fun(X, Sum) -> X + Sum end, N, S)];  % sum of all values in the stack
rpn("prod", [N|S]) -> [lists:foldl(fun(X, Sum) -> X * Sum end, N, S)]; % product of all values in the stack
rpn(X, Stack) -> [read(X)|Stack].

read(N) ->
  case string:to_float(N) of
    {error,no_float} -> list_to_integer(N);
    {F,_} -> F
  end.

% Unit tests ensuring our calculator works correctly
rpn_test() ->
  5 = rpn("2 3 +"),
  87 = rpn("90 3 -"),
  -4 = rpn("10 4 3 + 2 * -"),
  -2.0 = rpn("10 4 3 + 2 * - 2 /"),
  ok = try
    rpn("90 34 12 33 55 66 + * - +")
  catch
    error:{badmatch,[_|_]} -> ok
  end,
  4037 = rpn("90 34 12 33 55 66 + * - + -"),
  8.0 = rpn("2 3 ^"),
  true = math:sqrt(2) == rpn("2 0.5 ^"),      % "true =" is necessary here because
  true = math:log(2.7) == rpn("2.7 ln"),      % you can't have a function call
  true = math:log10(2.7) == rpn("2.7 log10"), % as the left operand
  50 = rpn("10 10 10 20 sum"),
  10.0 = rpn("10 10 10 20 sum 5 /"),
  1000.0 = rpn("10 10 20 0.5 prod"),
  ok.

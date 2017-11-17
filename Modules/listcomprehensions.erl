
-module(listcomprehensions).

-compile(export_all).

% create a set of integers that are divisble by Val
get_div(List, Val) ->
  [X || X <- List, X rem Val == 0].

% remove all non-integers from a list and return the list of integers squared
% ex: get_int_squared([1,hello,100,boo,"boo",9]). return [1,10000,81]
get_int_sqared(List) ->
  [X*X || X <- List, is_integer(X)].

% return a list that is the symmetric difference of two lists
diff(List1, List2) ->
  [X || X <- List1 ++ List2, lists:member(X,List1) xor lists:member(X,List2)].

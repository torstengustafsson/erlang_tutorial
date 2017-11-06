-module(useless).

-export([add/2,
         add/3,
         hello/0,
         greet_and_add_two/1]).

% Generally not recommended, reduces readability of the code
% instead, use io:format("blabla") directly when using the method.
% Exception is the lists module, its functions are used frequently
% enough that we may want to import them directly.
-import(io, [format/1]).

% In Erlang, you may define any kind of attributes to your module.
% Even overwrite existing ones (like vsn, which is automatically generated otherwise)
% (vsn is simply a tag for the verion number of your module)
-a_cool_attribute("this is a useless attribute").


-define(MYMACRO, awesome).
-define(sub(X,Y), X-Y).

% Addition of two values
add(A,B) ->
  A + B.

% Addition of three values
add(A,B,C) ->
  D = A + B + C,             % useless code
  TempTuple = {?MYMACRO, D}, % that uses my macro!

  {_, Result} = TempTuple,   % we should use our useless variables to avoid compiler warnings
  Result.                    % return Result ( = A + B + C)

% Hello World.
% io:format/1 is the standard function used to output text.
hello() ->
  format("hello, world!~n").

% Combination of functions!
greet_and_add_two(X) ->
  hello(),
  add(X, 2).



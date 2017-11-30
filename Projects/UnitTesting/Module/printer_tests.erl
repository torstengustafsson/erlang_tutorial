
-module(printer_tests).

-include_lib("eunit/include/eunit.hrl").

-export([]).

-compile([export_all, nowarn_export_all]).

return_cool_test() ->
  ok = printer:print_cool().

return_cool_string_test() ->
  "cool" = printer:print_cool_string().

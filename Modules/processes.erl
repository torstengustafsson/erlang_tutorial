
-module(processes).

-export([double/1,
         double_process/0,
         wait/0,
         create/0,
         add_record/3,
         record_listener/0]).

-define(TIMEOUT, 1000).       % one second
-define(TIMEOUT_LONG, 60000). % one minute

% process that return an integer doubled
double(Val) ->
  io:format("doule: creating process...~n"),
  Pid = spawn(?MODULE, double_process, []), % create process
  Pid ! {self(), Val},                      % send message to process
  receive                                   % wait for response from process
    {SenderId, Value} ->
      io:format("double: result ~p received from process id ~p!~n", [Value, SenderId]),
      Value
    after ?TIMEOUT -> error(timeout)
  end.

double_process() ->
  io:format("double_process: process spawned!~n"),
  receive
    {SenderPid, Value} when is_integer(Value) ->
      io:format("double_process: value ~p received from process id ~p!~n", [Value, SenderPid]),
      SenderPid ! {self(), Value * 2};
    _          -> error(invalid_format)
    after ?TIMEOUT -> error(timeout)
  end.


% process that waits for timeout
wait() ->
  receive
    after ?TIMEOUT -> ok
  end.



%% process-based record system
%
% usage:
% Pid = processes:create().
% processes:add_record(Pid, record1, 1337).
%

-record(record1, {val, node}).
-record(record2, {val}).
-record(record3, {val}).

create() ->
  spawn(?MODULE, record_listener, []).

add_record(Pid, Record, Val) when Record == record1;
                                  Record == record2;
                                  Record == record3 ->
  case Record of
    record1 -> Pid ! {self(), #record1{val=Val, node=my_awesome_node}};
    record2 -> Pid ! {self(), #record2{val=Val}};
    record3 -> Pid ! {self(), #record3{val=Val}}
  end,
  receive
    Res -> io:format("add_record: Pid ~p updated record ~p with value ~p!~n", [Pid, Record, Res])
  after ?TIMEOUT -> error(timeout)
  end.

record_listener() ->
  io:format("record_listener: new process spawned~n"),
  receive
    {SenderPid, Message} when element(1, Message) == 'record1' ->
      io:format("add_to_record: record1 received from pid ~p!~n", [SenderPid]),
      Val = Message#record1.val,
      Node = Message#record1.node,
      io:format("record_listener: Val: ~p~n", [Val]),
      SenderPid ! Message#record1{val=Val, node=Node},
      record_listener();

    {SenderPid, Message} when element(1, Message) == 'record2' ->
      io:format("record_listener: record2 received from pid ~p!~n", [SenderPid]),
      Val = Message#record2.val,
      io:format("record_listener: Val: ~p~n", [Val]),
      SenderPid ! Message#record2{val=Val},
      record_listener();

    {SenderPid, Message} when element(1, Message) == 'record3' ->
      io:format("record_listener: record3 received from pid ~p!~n", [SenderPid]),
      Val = Message#record3.val,
      io:format("  Val: ~p~n", [Val]),
      SenderPid ! Message#record3{val=Val},
      record_listener();

    _ -> error(invalid_format)

  after ?TIMEOUT_LONG -> error(timeout)
  end.

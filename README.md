# Learn You Some Erlang

http://learnyousomeerlang.com/content

# Erlang quick reference card:

https://github.com/stolowski/Erlang-Quick-Reference-Card/blob/master/erlang-quickref.pdf

# Erlang coding rules:

http://www.erlang.se/doc/programming_rules.shtml

# Tips and Tricks:

## Tuple

    MyTuple = {1,2,3}.

## List:

    [1,2,3]


**add lists:** (-- to remove elements)

    [1,2,3] ++ [4,5].
      = [1,2,3,4,5]


[head | tail] where Head is first element and Tail is remaining ones.

example:

    MyList = [head, {point, {3,2}}, {point, {4,1}}].

    [Head | Tail] = MyList. % percent sign makes a comment. Assign Head to first element, and Tail the rest.

    Head = head. % this becomes a check instead of assignment, since Head is already defined. In this case, it passes.

    Tail = [{point, {3,2}}, {point, {4,1}}]. % check passes!

Note: Lists and tuples are implemented as linked lists!

Standard list operations: http://erldocs.com/18.0/stdlib/lists.html

## List Comprehensions:

    [Expression || GeneratorExp1, GenratorExp2, ... , Condition1, Condition2, ...]

example:

    [2*X || X <- [1,2,3,4], X rem 2 == 0].
      = [4,8]


**(can work like matrix multiplication, in some cases) example:**

(here '|' is used for new row, and ',' for new column)

(2|3) * (1,2) = (2,4|3,6)

and

    [X*Y || X <- [2,3], Y <- [1.2]].
      = [2, 4, 3, 6]

## Binary syntax

Note: Binary format is much more efficient than list format since values are stored aligned in memory!

    Pixel = <<45, 54, 65>>.

    <<R, G, B>> = Pixel.

    .<<X/integer-signed-little>> = <<-44>>. % here, X is an integer, it is signed, and it is of little-endian

Binary comprehension example:

    [X || <<X>> <= <<1,2,3,4,5>>, X rem 2 == 0].
      = [2,4]

example:

    Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.

    RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ].

and to do the opposite:

    BackToPixels = << <<R:8, G:8, B:8>> ||  {R,G,B} <- RGB >>.


## Modules

Save functions (and attributes - metadata about the module) to file.

'erlang' is the standard module which is automatically imported


## Anonymous functions

    MyFunction = fun(X, Y) -> X * Y end.

"MyFunction(2,4)." would then return 8

## Erlang standard library functions using higher-order functions:

all(Pred, List) return true if all elements match the predicate.

example:

* lists:all(fun(X) -> X > 0 end, [1,2,3]). (returns true)
* lists:all(fun(X) -> X > 0 end, [1,2,-3]). (returns false)

any(Pred, List) return true if any element match the predicate

append(List1, List2) combines List1 and List2

concat(Things) concatenate the text representations of the elements of Things

zip(List1, List2) makes one list of two-tuples out of List1 and List2

foreach(Fun, List) apply Fun(Elem) to each element in the list

filter(Pred, List) reurn a list with only the elements that matches Pred

## Erlang processes

built-in function spawn(Module, Function, Arguments) is used to spawn new processes.

spawn/3 returns a pid (process ID)

self() is used to get the pid of the current process

Messages can be sent between processes using: Pid ! Message (! is called the 'bang' operator)

Message can be of any valid Erlang data type

Each Erlang process has a 'mailbox' in which incoming message are stored in the order they are received

messages are retrieved from the mailbox using the 'receive' clause:

When executing the receive statement, the oldest message in the mailbox is used.

    receive
      Pattern1 when Guard1 -> exp1;
      Pattern2 when Guard2 -> exp2
    end.

if no clauses match, the process is suspended in the recieve statements intil a message is matched. (listen for event).

To handle timeout events:

    receive
      Pattern1 when Guard1 -> exp1;
      Pattern2 when Guard2 -> exp2
    after
      Timeout -> exp3
    end.

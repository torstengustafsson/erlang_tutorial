# Learn You Some Erlang

http://learnyousomeerlang.com/content

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

[Head | Tail] = MyList.

  Head = head

  Tail = [{point, {3,2}}, {point, {4,1}}]

Note: Lists and tuples are implemented as linked lists!

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

Pixel = <<45, 54, 65>>

<<R, G, B>> = Pixel

<<X/integer-signed-little>> = <<-44>>. (here, X is an integer, it is signed, and it is of little-endian)

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





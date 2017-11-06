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


##List Comprehensions:

[Expression || GeneratorExp1, GenratorExp2, ... , Condition1, Condition2, ...]

example:

[2*X || X <- [1,2,3,4], X rem 2 == 0].
  = [4,8]


**(works like matrix multiplication, in some cases) example:**
 here, '|' is used for new row, and ',' for new column
(2|3) * (1,2) = (2,4|3,6)

and

[X*Y || X <- [2,3], Y <- [1.2]].
  = [2, 4, 3, 6]


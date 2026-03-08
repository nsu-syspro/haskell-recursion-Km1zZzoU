[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/q23PoK3x)
# Haskell: Recursion

<img alt="points bar" align="right" height="36" src="../../blob/badges/.github/badges/points-bar.svg" />

<details>
<summary>Guidelines</summary>

## Guidelines

When solving the homework, strive to create not just code that works, but code that is readable and concise.
Try to write small functions which perform just a single task, and then combine those smaller
pieces to create more complex functions.

Don’t repeat yourself: write one function for each logical task, and reuse functions as necessary.

Don't be afraid to introduce new functions where you see fit.

### Sources

Each task has corresponding source file in [src](src) directory where you should implement the solution.

### Building

All solutions should compile without warnings with following command:

```bash
stack build
```

### Testing

You can and should run automated tests before pushing solution to GitHub via

```bash
stack test --test-arguments "-p TaskX"
```

where `X` in `TaskX` should be number of corresponding Task to be tested.

So to run all test for the first task you should use following command:

```bash
stack test --test-arguments "-p Task1"
```

You can also run tests for all tasks with just

```bash
stack test
```

### Debugging

For debugging you should use GHCi via stack:

```bash
stack ghci
```

You can then load your solution for particular task using `:load TaskX` command.

Here is how to load Task1 in GHCi:

```bash
$ stack ghci
ghci> :load Task1
[1 of 1] Compiling Task1 ( .../src/Task1.hs, interpreted )
Ok, one module loaded.
```

> **Note:** if you updated solution, it can be quickly reloaded in the same GHCi session with `:reload` command
> ```bash
> ghci> :reload
> ```

</details>

## Task 1 (4 points)

### Luhn algorithm

The first task is to implement [Luhn algorithm](https://en.wikipedia.org/wiki/Luhn_algorithm)
which is widely used to quickly detect errors or typos in various identification numbers.

Given a number (sequence of decimal digits) it computes [check digit](https://en.wikipedia.org/wiki/Check_digit)
based on the whole number. This digit can then be appended to the initial number to get a *valid* number.
Using this new number, anyone can validate it by dropping last digit, computing check digit again and comparing it
with the one that was dropped. If check digit is the same, then the number is considered *valid*.

This family of algorithms is typically used to detect *human errors*, for example when credit card number
is typed by hand online. However, it can only detect some subset of errors like changing single digit in the number.

Luhn algorithm in particular uses the following procedure to compute check digit
(assuming that the original check digit is already dropped):

1. Starting from least significant digit (right to left) double every second digit.
   If the value after doubling becomes greater than or equal to 10, then subtract 9 from it (or sum its digits).  
1. Sum all resulting digits.
1. The check digit is finally computed as `(10 - (s mod 10)) mod 10` where `s` is the sum computed on previous step.

**Example:** given number 3456

1. Its digits after doubling become `[3,8,5,12]`; the last value is
   greater than or equal to 10 so after subtraction of 9 digits become `[3,8,5,3]`
1. Sum of resulting digits `s = 3 + 8 + 5 + 3 = 19`
1. Check digit is `(10 - (19 mod 10)) mod 10 = 1`

So initial number 3456 with appended check digit becomes 34561.

### Task

You should implement the following functions:

- `validate` which checks whether the last digit is a valid check digit for the rest of the number
  ```haskell
  validate :: Integer -> Bool
  ```
  **Example:**
  ```haskell
  >>> validate 3456
  False
  >>> validate 34561
  True
  >>> validate 34562
  False
  ```
- `luhn` which computes check digit for given digits (without any initial check digit)
  ```haskell
  luhn :: [Int] -> Int
  ```
  **Example:**
  ```haskell
  >>> luhn [3,4,5,6]
  1
  ```

You probably have already figured out that `validate` should be implemented using `luhn` which
goes in line with principles of code reuse and logic separation.

However, do not attempt implementing `luhn` or `validate` in one go! Functional programming is
about splitting your programs into smaller simpler parts and combining (composing) them together
to obtain result.

More specifically, you should also implement following functions:

- `toDigits` which produces list of digits for given *positive* number;
  otherwise (for zero and negative numbers) returns empty list
  ```haskell
  toDigits :: Integer -> [Int]
  ```
  > **Note:** use function `fromIntegral` for conversion between any integral types.
  
  **Example:**
  ```haskell
  >>> toDigits 3456
  [3,4,5,6]
  >>> toDigits 0
  []
  >>> toDigits (-123)
  []
  ```
- `reverse` which reverses given list (note the polymorphic type signature!)
  ```haskell
  reverse :: [a] -> [a]
  ```
  **Example:**
  ```haskell
  >>> reverse [3,4,5,6]
  [6,5,4,3]
  ```
- `doubleEveryOther` which doubles every other digit starting from first one
  ```haskell
  doubleEveryOther :: [Int] -> [Int]
  ```
  **Example:**
  ```haskell
  >>> doubleEveryOther [6,5,4,3]
  [12,5,8,3]
  ```
- `normalize` which normalizes given number to single digit as described in original algorithm
  (if number is greater than or equal to 10 then subtracts 9 from it, otherwise keeps it as-is)
  ```haskell
  normalize :: Int -> Int
  ```
  **Example:**
  ```haskell
  >>> normalize 12
  3
  >>> normalize 1
  1
  ```
- `map` which applies given function to each element in given list yielding another list
  (note the polymorphic type signature!)
  ```haskell
  map :: (a -> b) -> [a] -> [b]
  ```
  **Example:**
  ```haskell
  >>> map (\x -> x * 2) [1,2,3,4]
  [2,4,6,8]
  ```
- `sum` which computes sum of given list of numbers
  ```haskell
  sum :: [Int] -> Int
  ```
  **Example:**
  ```haskell
  >>> sum [3,8,5,3]
  19
  >>> sum []
  0
  ```

> **Note:** do not hesitate to define other helper functions if you see the need to do so!  
> For example, computation of `(10 - (s mod 10)) mod 10` is a very good candidate for
> extraction to separate function.

With these functions defined, `luhn` and `validate` should be straightforward
to implement as their composition.

> This might seem strange or inefficient to compose a bunch of functions on lists
> instead of doing it all in one go (double, normalize and sum digits) in a single pass through the list.
> However, as we will see later, this is not really an issue due to Haskell's inherent laziness and
> robust optimizations that GHC performs.
>
> On the contrary such compositional way of designing programs often leads to more succinct and reliable implementation.

## Task 2 (4 points)

### Luhn mod N algorithm

The original Luhn algorithm only works with decimal digits, but can actually be
generalized to arbitrary character sets of size `N` in [Luhn mod N algorithm](https://en.wikipedia.org/wiki/Luhn_mod_N_algorithm)
as long as `N` is divisible by 2. For example, it can compute check digit (or rather check character) for hexadecimal numbers
or arbitrary alphanumeric strings.

The generalized mod `N` algorithm follows the original, with couple of changes
(again assuming check character is already dropped):

1. **(New)** Define mapping from each of `N` characters to number between 0 and `N`.
1. **(New)** Convert each character of given string to number via the mapping from previous step.
1. Starting from rightmost number and moving to the left, double every second digit.
   If the value after doubling becomes greater than or equal to `N`, then subtract `N-1` from it.  
1. Sum all resulting numbers.
1. The number corresponding to check character is finally computed as `(N - (s mod N)) mod N`
   where `s` is the sum computed on previous step.
1. **(New)** The actual character can be obtained by inverting the mapping from the first step.

### Task

Your goal is to implement this algorithm in the most general form for arbitrary base `N` and character type:

`luhnModN` which calculates check character number for given `N`, mapping from character type `a` to `Int`
and list of characters
```haskell
luhnModN :: Int -> (a -> Int) -> [a] -> Int
```

Using this generalized form you should be able to implement

- Original algorithm for decimal digits
  ```haskell
  luhnDec :: [Int] -> Int
  luhnDec = luhnModN 10 id
  ```
  > Note: `id :: a -> a` is standard identity function from Prelude.
  
  It should work exactly the same as `luhn` function defined previously in Task 1.

- Hexadecimal variant of algorithm
  ```haskell
  luhnHex :: [Char] -> Int 
  luhnHex = luhnModN 16 digitToInt
  ```
  where `digitToInt` (to be implemented) is a function that converts any hexadecimal character to corresponding
  ordinal between 0 and 16:
  ```haskell
  digitToInt :: Char -> Int
  ```
  **Example:**
  ```haskell
  >>> map digitToInt ['0'..'9']
  [0,1,2,3,4,5,6,7,8,9]
  >>> map digitToInt ['a'..'f']
  [10,11,12,13,14,15]
  >>> map digitToInt ['A'..'F']
  [10,11,12,13,14,15]
  ```
  > Note that it should support both lowercase letters and uppercase ones.
  > Also assume that characters outside of these ranges will not be passed to this function
  > (so either any number or error is fine).

Fill free to reuse any functions from Task 1 via importing them (there is an example in [src/Task2.hs](src/Task2.hs))
and introduce any helper functions if needed.

Finally using `luhnDec` and `luhnHex` define corresponding validation functions:

```haskell
validateDec :: Integer -> Bool
validateHex :: String  -> Bool
```

### Optional

Try to abstract common logic from the two validation functions into a polymorphic function `validate` by answering
the following questions:

1. What are common parts or patterns between `validateDec` and `validateHex` functions?
1. How many higher-order functions (if any) will `validate` function need to abstract these common parts?
1. What type signature could `validate` have?
1. Is it possible to simplify type signature of `validate`?

The goal is to introduce new function `validate` using which both `validateDec` and `validateHex`
could be implemented reducing duplication between them.

## Task 3 (2 points)

### Tower of Hanoi

![](https://upload.wikimedia.org/wikipedia/commons/0/07/Tower_of_Hanoi.jpeg)

The last task is to solve a classic problem [Tower of Hanoi](https://en.wikipedia.org/wiki/Tower_of_Hanoi).
This problem is usually formulated as a game or a puzzle with three pegs (let's call them `a`, `b` and `c`),
where on the first one there are stacked `n` disks with decreasing size (see image above).

The goal is to move all disks from peg `a` to peg `b` with following restrictions:

1. Only one disk can be moved at a time
1. A disk may only be moved to empty peg or on top of *larger* disk

There exist both iterative and recursive solutions of this problem.
But we will be interested in solving it recursively for this exercise.

Try to come up with recursive way to solve it on your own and then check out the general idea below or in
[wiki](https://en.wikipedia.org/wiki/Tower_of_Hanoi#Recursive_solution).

<details>
  <summary>Idea of recursive solution</summary>

### Basis

If `n = 1` then the solution is trivial --- just move the only disk from `a` to `b`.

### Recursive step

Otherwise we can use the third peg `c` as temporary storage:

1. Move `n - 1` disks from `a` to `c` (we can do it recursively for `n - 1`)
1. Move remaining largest disk from `a` to `b`
1. Move `n - 1` disks from `c` to `b` (again recursion)

</details>

### Task

> Note: for this task we introduced a couple of *type synonyms*:
> 
> ```haskell
> type Peg = String
> type Move = (Peg, Peg)
> ```
>
> Such synonyms are often used to simplify type signatures or
> to turn type signatures into sort of documentation for the function.

You will need to implement the following function `hanoi` which *recursively* solves this puzzle for
given positive integer `n` and names of the pegs:

```haskell
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
```

This function should return list of moves needed to move all `n` disks from first peg to the second one.

**Example:**

```haskell
>>> hanoi 2 "a" "b" "c"
[("a","c"),("a","b"),("c","b")]
```

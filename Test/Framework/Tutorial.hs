{-|

This module provides a short tutorial on how to use HTF.

Suppose you are writing a function for reversing a list:

@
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs
@

To test this function using HTF, you first create a new source file
with a @OPTIONS_GHC@ pragma in the first line.


> { -# OPTIONS_GHC -XTemplateHaskell -cpp -pgmPcpphs -optP --cpp -optP -include -optP HTF.h #- }

/Note:/ You need to remove the spaces between @{@ and @-@ as well as
between @-@ and @}@.
                                               
The following @import@ statements are also needed:

@
import System.Environment ( getArgs )
import Test.Framework
@

The actual unit tests and QuickCheck properties are defined like this:

@
$('tests' \"reverseTests\" [d|
  test_nonEmpty = do assertEqual [1] (myReverse [1])
                     assertEqual [3,2,1] (myReverse [1,2,3])
  test_empty = assertEqual ([] :: [Int]) (myReverse [])
  prop_reverse :: [Int] -> Bool
  prop_reverse xs = xs == (myReverse (myReverse xs))
 |])
@

The meta-level function 'tests' is called at compile-time. It takes the
name of the test and a sequence of declarations enclosed in brackets
@[d| ... |]@. Inside the brackets, all declarations starting with
@test_@ give raise to a unit test in the style of HUnit
(<http://hunit.sourceforge.net>), where the name of the unit test is
the name of the declaration with the @test_@ prefix stripped
off. Similarly, all declarations starting with @prop_@ denote
QuickCheck properties (<http://www.cs.chalmers.se/~rjmh/QuickCheck/>),
where the name of the property is the name of the declaration with the
@prop_@ prefix stripped off.

The 'tests' function then uses the test name \"reverseTests\" passed to
it to generate a new top-level declaration of name @reverseTests@. 
This top-level declaration
encapsulates the unit tests \"nonEmpty\" and \"empty\", and the QuickCheck
property \"reverse\" in a 'Test' datatype.

To run the tests, use HTF's 'runTestWithArgs' function, which
accepts a list of strings to select the tests to run.

@
main = 
    do args <- getArgs
       runTestWithArgs args reverseTests
@

Here is the skeleton of a @.cabal@ file which you may want to use to
compile the tests. The dependency on @cpphs@ is needed because HTF
relies on a preprocessor to attach location information to each
@assert...@ statement so as to provide exact information about which
assertion failed.

@
Name:          HTF-tutorial
Version:       0.1
Cabal-Version: >= 1.6
Build-type:    Simple

Executable tutorial
  Main-is: Tutorial.hs
  Build-depends: base, HTF, template-haskell
  Build-tools: cpphs>=1.8
@

Compiling the program just shown (you must include the code for
@myReverse@ as well), and then running the resulting program with no
further commadline arguments yields the following output:

> Main.reverseTests:test_nonEmpty
> *** Failed! assertEqual failed at Tutorial.hs:14
>  expected: [3,2,1]
>  but got:  [3]
> 
> Main.reverseTests:test_empty
> +++ OK
> 
> Main.reverseTests:reverse
> *** Failed! Falsifiable (after 3 tests):                  
> [0,0]
> Replay argument: "Just (1060394807 2147483396,2)"
> 
> * Tests:    2
> * Passed:   1
> * Failures: 1
> * Errors:   0

(To check only certain tests, you can pass commandline
arguments to the program: HTF then runs only those tests whose name
contain at least one of the commandline arguments as a substring.)

You see that the message for the first failure contains exact location
information, which is quite convenient. Moreover, for the QuickCheck
property @Main.reverseTests:reverse@, HTF also outputs a string
represenation of the random generator used to check the property. This
string representation can be used to replay the property.  (The replay
feature may not be useful for this simple example but it helps in more
complex scenarios).

To replay a property you simply use the string
representation of the generator to define a new QuickCheck property 
with custom arguments:

@
  prop_reverseReplay = 
    'withQCArgs' (\a -> a { 'replay' = 'read' \"Just (1060394807 2147483396,2)\" })
    prop_reverse
@

To finish this tutorial, we now give a correct definition for @myReverse@:

@
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
@

Running our tests again on the fixed definition then yields the
desired result:

> Main.reverseTests:test_nonEmpty
> +++ OK
> 
> Main.reverseTests:test_empty
> +++ OK
> 
> Main.reverseTests:reverse
> +++ OK, passed 100 tests.
> 
> Main.reverseTests:reverseReplay
> +++ OK, passed 100 tests.
> 
> * Tests:    4
> * Passed:   4
> * Failures: 0
> * Errors:   0

-}
module Test.Framework.Tutorial where

import Test.Framework

{-|

This module provides a short tutorial on how to use the HTF. It
assumes that you are using GHC for compiling your Haskell code. (It is
possible to use the HTF with other Haskell environments, only the steps
taken to invoke the custom preprocessor of the HTF may differ in
this case.)

Suppose you are writing a function for reversing lists:

@
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs
@

To test this function using the HTF, you first create a new source
file with a @OPTIONS_GHC@ pragma in the first line.

@
&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;
@

This pragma instructs GHC to run the source file through @htfpp@, the
custom preprocessor of the HTF.

The following @import@ statements are also needed:

@
import System.Environment ( getArgs )
import Test.Framework
@

The actual unit tests and QuickCheck properties are defined like this:

@
test_nonEmpty = do assertEqual [1] (myReverse [1])
                   assertEqual [3,2,1] (myReverse [1,2,3])

test_empty = assertEqual ([] :: [Int]) (myReverse [])

prop_reverse :: [Int] -> Bool
prop_reverse xs = xs == (myReverse (myReverse xs))
@

When @htfpp@ consumes the source file, it replaces the @assertEqual@
tokens (and other @assert@-like tokens, see
"Test.Framework.HUnitWrapper") with calls to
'assertEqual_', passing
the current location in the file as the first argument. Moreover, the
preprocessor collects all top-level definitions starting with @test_@
or @prop_@ in a test suite with name allHTFTests of type 'TestSuite'.

Definitions starting with @test_@
denote unit tests and must be of type 'Assertion'.
Definitions starting with @prop_@
denote QuickCheck properties and must be of type /T/ such that
/T/ is an instance of the type class 'Testable'.

To run the tests, use the 'runTestWithArgs' function, which
takes a list of strings and the test.

@
main =
    do args <- getArgs
       runTestWithArgs args reverseTests
@

Here is the skeleton of a @.cabal@ file which you may want to use to
compile the tests.

@
Name:          HTF-tutorial
Version:       0.1
Cabal-Version: >= 1.6
Build-type:    Simple

Executable tutorial
  Main-is: Tutorial.hs
  Build-depends: base, HTF
@

Compiling the program just shown (you must include the code for
@myReverse@ as well), and then running the resulting program with no
further commandline arguments yields the following output:

> Main:nonEmpty (Tutorial.hs:17)
> *** Failed! assertEqual failed at Tutorial.hs:18
>  expected: [3,2,1]
>  but got:  [3]
>
> Main:empty (Tutorial.hs:19)
> +++ OK
>
> Main:reverse (Tutorial.hs:22)
> *** Failed! Falsifiable (after 3 tests and 1 shrink):
> [0,0]
> Replay argument: "Just (847701486 2147483396,2)"
>
> * Tests:    3
> * Passed:   1
> * Failures: 2
> * Errors:   0

(To check only specific tests, you can pass commandline
arguments to the program: the HTF then runs only those tests whose name
contain at least one of the commandline arguments as a substring.)

You see that the message for the first failure contains exact location
information, which is quite convenient. Moreover, for the QuickCheck
property @Main.reverse@, the HTF also outputs a string
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

> Main:nonEmpty (Tutorial.hs:17)
> +++ OK
>
> Main:empty (Tutorial.hs:19)
> +++ OK
>
> Main:reverse (Tutorial.hs:22)
> +++ OK, passed 100 tests.
>
> Main:reverseReplay (Tutorial.hs:24)
> +++ OK, passed 100 tests.
>
> * Tests:    4
> * Passed:   4
> * Failures: 0
> * Errors:   0

The HTF also allows the definition of black box tests. See the documentation
of the "Test.Framework.BlackBoxTest" module for further information.

-}
module Test.Framework.Tutorial where

import Test.Framework

{- |
This module provides a short tutorial on how to use the HTF. It
assumes that you are using GHC for compiling your Haskell code. (It is
possible to use the HTF with other Haskell environments, only the steps
taken to invoke the custom preprocessor of the HTF may differ in
this case.)

We start with a simple example. Then we show how to use HTF to easily
collect test definitions from multiple modules and discuss
backwards-compatibility for projects already using `HUnit`. Finally,
we give a brief cookbook-like summary on how to setup your tests with HTF.

-}

module Test.Framework.Tutorial (

-- * A simple example
{- |

Suppose you are trying to write a function for reversing lists :

@
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs
@

To test this function using the HTF, you would first create a new source
file with a @OPTIONS_GHC@ pragma in the first line.

@
&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;
@

This pragma instructs GHC to run the source file through @htfpp@, the
custom preprocessor of the HTF.

The following @import@ statements are also needed:

@
import System.Environment ( getArgs )
import System.Exit ( exitWith )
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
the current location in the file as the first argument.
Moreover, the
preprocessor collects all top-level definitions starting with @test_@
or @prop_@ in a test suite of type 'TestSuite' and name @htf_@/M/@_thisModulesTests@,
where /M/ is the name of the current module with dots @.@ replaced by underscores @_@.
For your convenience, the preprocessor also defines the token @htf_thisModulesTests@
as a shorthand for the rather lengthy name @htf_@/M/@_thisModulesTests@.

Definitions starting with @test_@
denote unit tests and must be of type 'Assertion'.
Definitions starting with @prop_@
denote QuickCheck properties and must be of type /T/ such that
/T/ is an instance of the type class 'Testable'.

To run the tests, use the 'htfMain' function.

@
main = htfMain htf_thisModulesTests
@

Here is the skeleton of a @.cabal@ file which you may want to use to
compile the tests.

@
Name:          HTF-tutorial
Version:       0.1
Cabal-Version: >= 1.10
Build-type:    Simple

Executable tutorial
  Type:              exitcode-stdio-1.0
  Main-is:           Tutorial.hs
  Build-depends:     base == 4.*, HTF == 0.9.*
  Default-language:  Haskell2010
@

Compiling the program just shown (you must include the code for
@myReverse@ as well), and then running the resulting program with no
further commandline arguments yields the following output:

> [TEST] Main:nonEmpty (Tutorial.hs:17)
> assertEqual failed at Tutorial.hs:18
> * expected: [3, 2, 1]
> * but got:  [3]
> * diff:     [3, 2, 1]
> *** Failed!
>
> [TEST] Main:empty (Tutorial.hs:19)
> +++ OK
>
> [TEST] Main:reverse (Tutorial.hs:22)
> Falsifiable (after 7 tests and 4 shrinks):
> [0,0]
> Replay argument: "Just (1982441876 2147483392,6)"
> *** Failed!
>
> * Tests:    3
> * Passed:   1
> * Pending:  0
> * Failures: 2
> * Errors:   0
>
> * Failures:
>   * Main:nonEmpty (Tutorial.hs:17)
>   * Main:reverse (Tutorial.hs:22)

(To check only specific tests, you can pass commandline
arguments to the program: the HTF then runs only those tests whose name
contain at least one of the commandline arguments as a substring.)

You see that the message for the first failure contains exact location
information, which is quite convenient. Also, HTF provides a diff between
the expected and the given output. (For this simple example, a diff is kind
of useless, but with longer output strings, a diff allows you to identify
very quickly where the expected and the given results disagree.)

For the QuickCheck property @Main.reverse@, the HTF outputs a string
represenation of the random generator used to check the property. This
string representation can be used to replay the property.  (The replay
feature may not be useful for this simple example but it helps in more
complex scenarios).

To replay a property you simply use the string
representation of the generator to define a new QuickCheck property
with custom arguments:

@
prop_reverseReplay =
  'withQCArgs' (\\a -> a { 'replay' = 'read' \"Just (1060394807 2147483396,2)\" })
  prop_reverse
@

To finish this simple example, we now give a correct definition for @myReverse@:

@
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
@

Running our tests again on the fixed definition then yields the
desired result:

> [TEST] Main:nonEmpty (Tutorial.hs:17)
> +++ OK
>
> [TEST] Main:empty (Tutorial.hs:19)
> +++ OK
>
> [TEST] Main:reverse (Tutorial.hs:22)
> Passed 100 tests.
> +++ OK
>
> [TEST] Main:reverseReplay (Tutorial.hs:24)
> Passed 100 tests.
> +++ OK
>
> * Tests:    4
> * Passed:   4
> * Pending:  0
> * Failures: 0
> * Errors:   0

The HTF also allows the definition of black box tests. Essentially, black box
tests allow you to verify that the output of your program matches your expectations.
See the documentation of the "Test.Framework.BlackBoxTest" module for further information.

-}

-- * Test definitions in multiple modules

{- |

For testing real-world programs or libraries, it is often conventient to
split the tests into several modules. For example, suppose your library contains
of two modules @MyPkg.A@ and @MyPkg.B@, each containing test functions.
You can find a slightly extended of this scenario in the samples directory
of the HTF source tree, see <https://github.com/skogsbaer/HTF/tree/master/sample>.)

File @MyPkg/A.hs@

@
&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;
module MyPkg.A (funA, htf_thisModulesTests) where

import Test.Framework

funA :: Int -> Int
funA x = x + 1

test_funA1 = assertEqual (funA 41) 42

test_funA2 = assertEqual (funA 2) 3
@

File @MyPkg/B.hs@

@
&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;
module MyPkg.B (funB, htf_thisModulesTests) where

import Test.Framework

funB :: Int -> Int
funB x = x * 2

test_funB1 = assertEqual (funB 21) 42

test_funB2 = assertEqual (funB 0) 0
@

For module @MyPkg.A@, the @htfpp@ preprocessor collects the modules'
testcases into a variable @htf_MyPkg_A_thisModulesTests@ and defines a
preprocessor token @thisModulesTests@ as a shorthand for this variable.
Thus, to expose all HTF tests defined in @MyPkg.A@, we only
need to put @thisModulesTests@ into the export list. The same holds
analogously for module @MyPkg.B@.

To execute all tests defined in these two modules, you would create
a main module and import @MyPkg.A@ and @MyPkg.B@ with the special
import annotation @&#x7b;-&#x40; HTF_TESTS &#x40;-&#x7d;@. The effect of this annotation
is that the @htfpp@ preprocessor makes all test cases defined in
such modules imported
available in a variable called @htf_importedTests@. Thus, your
main module would look like this:

File @TestMain.hs@

@
&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;
module Main where

import Test.Framework
import &#x7b;-&#x40; HTF_TESTS &#x40;-&#x7d; MyPkg.A
import &#x7b;-&#x40; HTF_TESTS &#x40;-&#x7d; MyPkg.B

main = htfMain htf_importedTests
@

-}

-- * Backwards-compatibility with HUnit

{- |
The types of the various @assert@-like macros of the HTF are not backwards-compatible
with the corresponding functions of HUnit. This incompatibility is intentional, of course:
with HUnit, the programmer has to provide suitable location information by explicitly
passing a string argument to the @assert@-like functions, whereas HTF provides
location information implicitly through its pre-processor @htfpp@.

To simplify transition from HUnit to HTF, @htfpp@ provides a commandline flag
@--hunit@. This flag causes @htfpp@ to exand the assertion macros in a way compatible
with the types of the corresponding HUnit functions. For example, with the @--hunit@
flag being present, @assertEqual@ is exanded to
@'assertEqualVerbose_' ('makeLoc' \"filename\" line)@, whose type
@(Show a, Eq a) => String -> a -> a -> IO ()@ is compatible with
the type of HUnit's 'Test.HUnit.Base.assertEqual' function.

-}

-- * Summary

{- |

Here is a quick summary of how to write, collect, and execute your tests.
You should also have a look at the sample project at
<https://github.com/skogsbaer/HTF/tree/master/sample>.

-}

-- ** Writing tests

{- |

* Place @&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;@ at the top of your module.

* Put @htf_thisModulesTests@ into the export list of your module.

* Import @Test.Framework@.

* Prefix your unit tests with @test_@, see "Test.Framework.HUnitWrapper" for the assertions provided.

* Prefix your QuickCheck properties with @prop_@.

-}

-- ** Collecting and executing tests

{- |
* Place @&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;@ at the top of your module.

* Import @Test.Framework@.

* Import modules defining HTF tests with @import &#x7b;-&#x40; HTF_TESTS &#x40;-&#x7d; MyPkg.A@.

* Use @main = htfMain htf_importedTests@ to run all imported tests.

-}

) where

import Test.Framework
import qualified Test.HUnit.Base

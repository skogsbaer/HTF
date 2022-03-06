--
-- Copyright (c) 2005-2022   Stefan Wehr - http://www.stefanwehr.de
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA
--
{- |
This module provides a short tutorial on how to use the HTF. It
assumes that you are using GHC for compiling your Haskell code. (It is
possible to use the HTF with other Haskell environments, only the steps
taken to invoke the custom preprocessor of the HTF may differ in
this case.)

-}

module Test.Framework.Tutorial (

-- * Quickstart

{- |

For the impatient, we start with a brief cookbook-like summary and then go into details.
You should also have a look at the sample project at
<https://github.com/skogsbaer/HTF/tree/master/sample>.

-}

-- ** Writing tests

{- |

You should use the following skeleton for each module defining test cases:

@
&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;
module MyModule (
    -- some more exports here

    htf_thisModulesTests -- all test cases are automatically collected in htf_thisModulesTests

) where

import Test.Framework

-- Each top-level definition whose name starts with 'test_' defines a unit test.
test_nonEmpty :: Assertion
test_nonEmpty = do
  assertEqual [1] (reverse [1])
  assertEqual [3,2,1] (reverse [1,2,3])


-- Each top-level definition whose name starts with 'prop_' defines a quickcheck property.
prop_reverse :: [Int] -> Bool
prop_reverse xs = xs == (reverse (reverse xs))
@

For unit tests, see "Test.Framework.HUnitWrapper" for the assertions provided.

-}

-- ** Collecting and executing tests

{- |

Your main module collecting all tests should look like this:

@
&#x7b;-&#x23; OPTIONS_GHC -F -pgmF htfpp &#x23;-&#x7d;
module Main ( main ) where

-- Import modules defining HTF tests like this:
import &#x7b;-&#x40; HTF_TESTS &#x40;-&#x7d; MyModule

main :: IO ()
main = htfMain htf_importedTests -- all tests in modules imported via &#x7b;-&#x40; HTF_TESTS &#x40;-&#x7d; are available in htf_importedTests
@

Tests are then executed via @cabal test@ or @stack test@. You only need to add
the following snippet to your @.cabal@ file:

@
Test-Suite example
  Type:              exitcode-stdio-1.0
  Main-is:           Main.hs
  Build-depends:     base, HTF
  Default-language:  Haskell2010
@

Several commandline options are available for HTF tests. Use
@stack test --ta --help@ or @cabal test --test-options --help@ to see the list of options.
-}

-- * A simple example
{- |

We now explain everything in more detail.
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

The following @import@ statement is also needed:

@
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

When @htfpp@ consumes the source file, it
collects all top-level definitions starting with @test_@
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

Test-Suite tutorial
  Type:              exitcode-stdio-1.0
  Main-is:           Tutorial.hs
  Build-depends:     base, HTF
  Default-language:  Haskell2010
@

Compiling the program just shown (you must include the code for
@myReverse@ as well), and then running the resulting program with no
further commandline arguments yields the following output (colors had to
be omitted, so the diff output does not look very useful):

> [TEST] Main:nonEmpty (Tutorial.hs:17)
> assertEqual failed at Tutorial.hs:18
> * expected: [3, 2, 1]
> * but got:  [3]
> * diff:     [3, 2, 1]
> *** Failed! (0ms)
>
> [TEST] Main:empty (Tutorial.hs:19)
> +++ OK (0ms)
>
> [TEST] Main:reverse (Tutorial.hs:22)
> Falsifiable (after 6 tests and 5 shrinks):
> [0,0]
> Replay argument: "Just (200055706 2147483393,5)"
> *** Failed! (0ms)
>
> [TEST] Main:reverseReplay (Tutorial.hs:24)
> Falsifiable (after 1 test and 2 shrinks):
> [0,0]
> Replay argument: "Just (1060394807 2147483396,2)"
> *** Failed! (0ms)
>
> * Tests:    4
> * Passed:   1
> * Pending:  0
> * Failures: 3
> * Errors:   0
>
> * Failures:
>   * Main:reverseReplay (Tutorial.hs:24)
>   * Main:reverse (Tutorial.hs:22)
>   * Main:nonEmpty (Tutorial.hs:17)
>
> Total execution time: 4ms

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
> +++ OK (0ms)
>
> [TEST] Main:empty (Tutorial.hs:19)
> +++ OK (0ms)
>
> [TEST] Main:reverse (Tutorial.hs:22)
> Passed 100 tests.
> +++ OK (20ms)
>
> [TEST] Main:reverseReplay (Tutorial.hs:24)
> Passed 100 tests.
> +++ OK (4ms)
>
> * Tests:    4
> * Passed:   4
> * Pending:  0
> * Failures: 0
> * Errors:   0
>
> Total execution time: 28ms

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
of the HTF source tree, see <https://github.com/skogsbaer/HTF/tree/master/sample>.

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
preprocessor token @htf_thisModulesTests@ as a shorthand for this variable.
Thus, to expose all HTF tests defined in @MyPkg.A@, we only
need to put @htf_thisModulesTests@ into the export list. The same holds
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

-- * Machine-readable output

-- ** JSON

{- |

For better integration with your testing environment, HTF provides the ability to produce
machine-readable output in JSON format. The output is produced incrementally, line by line.

Here is a short example how the JSON output looks like,
for details see "Test.Framework.JsonOutput":


> {"test":{"flatName":"Main:nonEmpty","location":{"file":"Tutorial.hs","line":17},"path":["Main","nonEmpty"],"sort":"unit-test"},"type":"test-start"}
> ;;
> {"result":"pass","message":"","test":{"flatName":"Main:nonEmpty","location":{"file":"Tutorial.hs","line":17},"path":["Main","nonEmpty"],"sort":"unit-test"},"wallTime":0,"type":"test-end","location":null}
> ;;
> {"test":{"flatName":"Main:empty","location":{"file":"Tutorial.hs","line":19},"path":["Main","empty"],"sort":"unit-test"},"type":"test-start"}
> ;;
> {"result":"pass","message":"","test":{"flatName":"Main:empty","location":{"file":"Tutorial.hs","line":19},"path":["Main","empty"],"sort":"unit-test"},"wallTime":0,"type":"test-end","location":null}
> ;;
> {"test":{"flatName":"Main:reverse","location":{"file":"Tutorial.hs","line":22},"path":["Main","reverse"],"sort":"quickcheck-property"},"type":"test-start"}
> ;;
> {"result":"pass","message":"Passed 100 tests.","test":{"flatName":"Main:reverse","location":{"file":"Tutorial.hs","line":22},"path":["Main","reverse"],"sort":"quickcheck-property"},"wallTime":19,"type":"test-end","location":null}
> ;;
> {"test":{"flatName":"Main:reverseReplay","location":{"file":"Tutorial.hs","line":24},"path":["Main","reverseReplay"],"sort":"quickcheck-property"},"type":"test-start"}
> ;;
> {"result":"pass","message":"Passed 100 tests.","test":{"flatName":"Main:reverseReplay","location":{"file":"Tutorial.hs","line":24},"path":["Main","reverseReplay"],"sort":"quickcheck-property"},"wallTime":4,"type":"test-end","location":null}
> ;;
> {"failures":0,"passed":4,"pending":0,"wallTime":39,"errors":0,"type":"test-results"}
> ;;

Machine-readable ouput is requested by the @--json@ flag. You can specify a dedicated output file
using the @--output-file=@ option. On some platforms (e.g. Windows) it might not be possible to read
from the output file while the tests are running (due to file-locking). In this case, you might want
to use the @--split@ option. With this option, HTF writes each JSON message to a separate ouput file.
The name of the output file is derived from the name given with the @--output-file=@ flag by
appending an index (starting at 0) that is incremented for every message.

-}

-- ** XML

{- | Machine-readable output is also available as XML, following the specification of the JUnit
output format. XML-output is requested by the @--xml=OUTPUT_FILE@ flag.
See "Test.Framework.XmlOutput" for details.

-}
-- * Commandline options

{- |

Here is the list of commandline options for programs using @htfMain@:

@
USAGE: COMMAND [OPTION ...] PATTERN ...

  where PATTERN is a posix regular expression matching
  the names of the tests to run.

  -q          --quiet                     Only display errors.
  -n PATTERN  --not=PATTERN               Tests to exclude.
  -l          --list                      List all matching tests.
  -j[N]       --threads[=N]               Run N tests in parallel, default N=1.
              --shuffle=BOOL              Shuffle test order. Default: false
  -o FILE     --output-file=FILE          Name of output file.
              --json                      Output results in machine-readable JSON format (incremental).
              --xml=FILE                  Output results in junit-style XML format.
              --split                     Splits results in separate files to avoid file locking (requires -o/--output-file).
              --colors=BOOL               Use colors or not.
              --history=FILE              Path to the history file. Default: ./.HTF/<ProgramName>.history
              --fail-fast                 Fail and abort test run as soon as the first test fails.
              --sort-by-prev-time         Sort tests ascending by their execution of the previous test run (if available). Default: false
              --max-prev-ms=MILLISECONDS  Do not try to execute tests that had a execution time greater than MILLISECONDS in a previous test run.
              --max-cur-ms=MILLISECONDS   Abort a test that runs more than MILLISECONDS.
              --prev-factor=DOUBLE        Abort a test that runs more than DOUBLE times slower than in a previous run.
              --timeout-is-success        Do not regard a test timeout as an error.
              --repeat=NUMBER             Execute the tests selected on the command line NUMBER times.
  -h          --help                      Display this message.
@
-}
) where

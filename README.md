HTF - The Haskell Test Framework
================================

* Documentation:     http://hackage.haskell.org/package/HTF
* Source Repository: https://github.com/skogsbaer/HTF/
* Author:            Stefan Wehr (Homepage: http://www.stefanwehr.de, Email: <mail AT stefanwehr DOT de>)
* License: LGPL

Summary
-------

The Haskell Test Framework (HTF for short) lets you define unit tests
(http://hunit.sourceforge.net), QuickCheck properties
(http://www.cs.chalmers.se/~rjmh/QuickCheck/), and black box tests in an
easy and convenient way. The HTF uses a custom preprocessor that collects
test definitions automatically. Furthermore, the preprocessor allows the
HTF to report failing test cases with exact file name and line number
information.

The documentation of the Test.Framework.Tutorial module provides a
tutorial for the HTF.


Installation instructions:
--------------------------

* Install from http://hackage.haskell.org/ using `cabal install HTF`
* Install from source:
        $ git clone git@github.com:skogsbaer/HTF.git
        $ cd HTF
        $ cabal install

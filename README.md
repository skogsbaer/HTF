# HTF - The Haskell Test Framework

[![Build Status](https://travis-ci.org/skogsbaer/HTF.svg?branch=master)](https://travis-ci.org/skogsbaer/HTF)
[![Hackage](https://img.shields.io/hackage/v/HTF.svg)](http://hackage.haskell.org/package/HTF)

* Documentation:     http://hackage.haskell.org/package/HTF
* Source Repository: https://github.com/skogsbaer/HTF/
* Issue tracker:     https://github.com/skogsbaer/HTF/issues?state=open
* Author:            Stefan Wehr (http://www.stefanwehr.de)
* License: LGPL

## Summary

The Haskell Test Framework (HTF for short) lets you define unit tests
(http://hunit.sourceforge.net), QuickCheck properties
(http://www.cs.chalmers.se/~rjmh/QuickCheck/), and black box tests in an
easy and convenient way. HTF uses a custom preprocessor that collects
test definitions automatically. Furthermore, the preprocessor allows
HTF to report failing test cases with exact file name and line number
information. Additionally, HTF tries to produce highly readable output
for failing tests: for example, it colors and pretty prints expected and
actual results and provides a diff between the two values.

Look [here](http://hackage.haskell.org/packages/archive/HTF/latest/doc/html/Test-Framework-Tutorial.html)
for a short tutorial on HTF. The following slightly out-dated
[blog article](http://factisresearch.blogspot.de/2011/10/new-version-of-htf-with-diffs-colors.html)
demonstrates HTF's coloring, pretty-printing and diff functionality.

## Installation instructions

* Install from http://hackage.haskell.org/ using `cabal install HTF`
* Install from source:

        $ git clone git@github.com:skogsbaer/HTF.git
        $ cd HTF
        $ cabal install

## Getting started

* Read the [tutorial](http://hackage.haskell.org/packages/archive/HTF/latest/doc/html/Test-Framework-Tutorial.html).
* Experiment with the sample project located in the `sample` directory of HTF's source tree.

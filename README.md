# HTF - The Haskell Test Framework

[![Build Status](https://github.com/skogsbaer/HTF/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/skogsbaer/HTF/actions/workflows/haskell-ci.yml)
[![Hackage](https://img.shields.io/hackage/v/HTF.svg)](http://hackage.haskell.org/package/HTF)

* Documentation:     http://hackage.haskell.org/package/HTF
* Source Repository: https://github.com/skogsbaer/HTF/
* Issue tracker:     https://github.com/skogsbaer/HTF/issues?state=open
* Author:            Stefan Wehr (http://www.stefanwehr.de)
* License:           LGPL

## Summary

The Haskell Test Framework (HTF for short) lets you define and organize unit tests
(http://hackage.haskell.org/package/HUnit), QuickCheck properties
(https://hackage.haskell.org/package/QuickCheck), and black box tests in an
easy and convenient way. HTF uses a custom preprocessor that collects
test definitions automatically.

HTF produces highly readable output
for failing test cases: it provides exact file name and line number
information,
it colors and pretty prints expected and
actual results, and it displays a diff highlighting the mismatching parts.

## Getting started

* Read the [tutorial](http://hackage.haskell.org/packages/archive/HTF/latest/doc/html/Test-Framework-Tutorial.html).
* Experiment with the sample project located in the `sample` directory of HTF's source tree.

# Changelog

## 0.3.2.3
* Relaxed doctest upper bound.

## 0.3.2.2
* Relaxed criterion upper bound.

## 0.3.2.1
* Relaxed optparse-applicative upper bound.

## 0.3.2
* Fixed some dimensions minifying incorrectly.
* Fixed some Eq instances.
* <An+B> values data type modified to disallow invalid values. This makes the
  data type safer, also simplifying the Quickcheck Arbitrary instance.
* Improved test coverage.

## 0.3.1.3
* Added support for `@supports` rules, and a slight minification for them: it
  removes adjacent negations, i.e.: @supports not (not ...) gets turn into
  @supports ....
* Fixed a small bug with `:lang()` where spaces before the right parenthesis
  weren't being removed.
* Improved test coverage.

## 0.3.0.1
Initial release

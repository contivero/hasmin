Hasmin - A Haskell CSS Minifier
====
[![Build Status](https://travis-ci.org/contivero/hasmin.svg?branch=master)](https://travis-ci.org/contivero/hasmin)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Hasmin is a CSS minifier written entirely in Haskell. To use it as a library,
see [below](https://github.com/contivero/hasmin#library)

Aside from the usual techniques (e.g. whitespace removal, color minification,
etc.), the idea was to explore new possibilities, by implementing things
other minifiers weren't doing, or they were, but not taking full advantage of.

Also, the minifier implements some techniques that do nothing for minified
sizes, but attempt to improve post-compression sizes (at least when using
DEFLATE, i.e. gzip).

For a list of techniques, see [Minification Techniques](https://github.com/contivero/hasmin/wiki/Minification-Techniques).

## Building
To compile, just run `stack build`.

## Minifier Usage
Hasmin expects a path to the CSS file, and outputs the minified result to
stdout.

Every technique is enabled by default, except for:
 1. Escaped character conversions (e.g. converting `\2714` to `✔`, which can be enabled with `--convert-escaped-characters`)
 2. Dimension minifications (e.g. converting `12px` to `9pt`, which can be enabled with `--dimension-min`, or just `-d`)

These two are disabled mainly because they are—on average, not
always—detrimental for DEFLATE compression. When something needs to be
disabled, use the appropriate flag. Not every technique can be toggled yet, but
a good amount of them allow it.

Note: there is a problem in Windows when using the
`--convert-escaped-characters` flag to enable the conversion of escaped
characters. A workaround is changing the code page, which can be done by running
`chcp 65001` in the terminal (whether cmd, or cygwin).

## Library
The preferable way to use Hasmin as a library is to `import Hasmin`, as
exemplified in the module's documentation. That is currently the only module
that is sure to abide by [PVP](https://pvp.haskell.org/). Most other exposed
modules are so because tests need it. Definitions there may be changed anytime.
In case some internal is needed though, feel free to open an issue about it.

## Zopfli Integration
Hasmin uses bindings to Google's
[Zopfli library](https://en.wikipedia.org/wiki/Zopfli), allowing the
possibility to compress the result.

Since the output is a gzip file, it can be used for the web. It tipically
produces files 3~8% smaller than zlib, at the cost of being around 80 times
slower, so it is only a good idea if you don't need compression on the fly.


Zopfli is released under the Apache License, Version 2.0.

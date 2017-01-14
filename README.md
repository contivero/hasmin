Hasmin - A Haskell CSS minifier
====
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Hasmin is a CSS minifier written entirely in Haskell.

Aside from the usual techniques (e.g. whitespace removal, color minification,
etc.), the idea was to explore new possibilities, by implementing things
minifiers weren't doing (for example, property traits), or they were, but not
taking full advantage of them (e.g. gradient and transform-function
minification).

Also, the minifier implements some techniques that do nothing for minified
sizes, but attempt to improve post-compression sizes (at least when using
DEFLATE, i.e. gzip).

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
characters. A workaround is changing the code page, which can be done by
running `chcp 65001` in the terminal (whether cmd, or cygwin).

## Zopfli Integration
Hasmin uses bindings to Google's
[Zopfli library](https://en.wikipedia.org/wiki/Zopfli), allowing the
possibility to compress the result.

Since the output is a DEFLATE compatible stream (what gzip uses), it can be
used for the web.
It tipically produces files 3~8% smaller than zlib, at the cost of being around
80 times slower, so it is only a good idea if you don't need compression on the fly.


Zopfli is released under the Apache License, Version 2.0.

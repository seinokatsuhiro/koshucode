NAME
----------------------

koshucode — a notational system for relational model


DESCRIPTION
----------------------

This repository contains a computer program named
[`koshu`](baala/calculator/koshu.hs)
that processes data written in Koshucode.
Koshucode is a notational system for people and computers
who read, write, and calculate relational data.
This notation widely improves the data literacy of all human beings.

Koshucode can be regarded as a data calculation system
correspond to the numerical calculation system
consisting of the Hindu-Arabic numerals and
the arithmetic operators developed in Europe.

There are no released packages yet.
Please download a zipped repository file from GitHub.
Installation instructions are described
in [`INSTALL.md`](INSTALL.md).

*[Koshu](http://en.wikipedia.org/wiki/Kai_Province)*
is the name of area in Japan that is today
Yamanashi Prefecture.
*Koshu* is translated from "甲州" in Kanji characters.
The first character is "甲" (kō),
and the second is "州" (shū).
*Koshucode* corresponds to the Japanese word
"甲州記法" (kōshū kihō).



DOCUMENTATION
----------------------

User manual is not written yet.
Please see examples or elementary book.

* [Examples][example] excerpt from [The Relational Database Dictionary][dictionary] by C. J. Date
* [The ABC of Koshucode][abc-of-koshucode]
  (This book is written in Japanese)

`koshu` is implemented using Haskell.
There is [Haddock documentation] of the software libraries.
These libraries include Haskell modules for Koshucode.
As an example of `koshu`, [a table of module dependencies][DIR-RANK-ALL]
is calculated using [`dir-rank.k`][dir-rank].


BUGS
----------------------

There are many bugs and potentials.


[example]:                    https://github.com/seinokatsuhiro/koshucode-example/tree/master/dictionary
[dictionary]:                 http://shop.oreilly.com/product/9780596527983.do
[abc-of-koshucode]:           https://github.com/seinokatsuhiro/abc-of-koshucode/tree/master/draft/english
[Haddock documentation]:      https://seinokatsuhiro.github.io/koshucode/haddock/double.html
[DIR-RANK-ALL]:               baala/import/DIR-RANK-ALL.k
[dir-rank]:                   baala/import/dir-rank.k

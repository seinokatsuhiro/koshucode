INSTALL
=================================


Download
---------------------------------

Implementation of a `koshu` calculator
has not released as a software package.
Please download zipped repository file
[`koshucode-master.zip`](https://github.com/seinokatsuhiro/koshucode/archive/master.zip)
from GitHub.
This implementation is an open-source software
under the BSD3 license.

`koshucode-master.zip` contains source files
in a directry tree like the following.
Files `*.cabal` are Cabal files for building software.
Files `*.hs` are Haskell source files.
Files `*.k` are data itself and calculation in koshucode.

```
/ koshucode-master
   / baala
      / base
         * koshucode-baala-base.cabal
         / Koshucode / Baala / Base

      / core
         * koshucode-baala-core.cabal
         / Koshucode / Baala / Core

      / operator
         * koshucode-baala-operator.cabal
         / Koshucode / Baala / Op

      / content
         * koshucode-baala-content.cabal
         / Koshucode / Baala / Op
         / Koshucode / Baala / Type

      / calculator
         * koshucode-baala-calculator.cabal
         * koshu.hs
         / Koshucode / Baala / Toolkit

      / toolkit
         * koshucode-baala-toolkit.cabal
         * koshu-change.hs
         * koshu-rdf.hs
         * koshu-syntax.hs
         / Koshucode / Baala / Toolkit
```


Install Haskell
---------------------------------

This software is written in Haskell,
and built with the Cabal packaging system.
Before building koshucode libraries and executables,
please install the [Haskell Platform](http://www.haskell.org/platform/).
Haskell Platform constains the Haskell compiler and
related libraries and tools like Cabal.



Install koshu command
---------------------------------

There are six Cabal packages.

* `koshucode-baala-base`
* `koshucode-baala-core`
* `koshucode-baala-operator`
* `koshucode-baala-content`
* `koshucode-baala-calculator`
* `koshucode-baala-toolkit` (optional)

First, install `koshucode-baala-base` package.
Type `cabal install` under the `base` directory.
This package contains `Koshucode.Baala.Base` module.

``` sh
cd koshucode-master/baala/base
cabal install
```

Install `koshucode-baala-core` package.
This package contains `Koshucode.Baala.Core` module.
`Core` module provides the central functionality of the language.

``` sh
cd koshucode-master/baala/core
cabal install
```

Install `koshucode-baala-operator` and `koshucode-baala-content` packages.
These packages provides relational operators.

``` sh
cd koshucode-master/baala/operator
cabal install
cd ../content
cabal install
```

Install `koshucode-baala-calculator` package.
This package contains `koshu` command.

``` sh
cd koshucode-master/baala/calculator
cabal install
```

Optionally,
you can install `koshucode-baala-toolkit` package.
This package contains additional commands,
e.g., `koshu-change`, `koshu-rdf` and `koshu-syntax`.

``` sh
# optional
cd koshucode-master/baala/toolkit
cabal install
```


Invoke koshu command
---------------------------------

`koshu` command is a portable relational calculator.
This calculator reads data and formula in koshucode,
and writes results of the calculations.
The results are also written in koshucode.

``` sh
cd koshucode-example/dictionary
koshu DATA.k calc/N/natural-join.k
```

The order of files is not significant.
You can also type `koshu calc/N/natural-join.k DATA.k`.
With the `-i` option, `koshu` reads data from the standard input.

``` sh
koshu -i calc/N/natural-join.k < DATA.k
```

These commands calculate [`natural-join.k`][natural-join.k]
for input [`DATA.k`][DATA.k] and writes the following output data.

``` text
** -*- koshu -*-
**
**  INPUT
**    DATA.k
**    calc/N/natural-join.k
**

|-- SSP  /pno 'P6  /qty 100  /sno 'S1  /sname 'Smith  /status 20  /city 'London
|-- SSP  /pno 'P5  /qty 100  /sno 'S1  /sname 'Smith  /status 20  /city 'London
|-- SSP  /pno 'P4  /qty 200  /sno 'S1  /sname 'Smith  /status 20  /city 'London
|-- SSP  /pno 'P3  /qty 400  /sno 'S1  /sname 'Smith  /status 20  /city 'London
|-- SSP  /pno 'P2  /qty 200  /sno 'S1  /sname 'Smith  /status 20  /city 'London

|-- SSP  /pno 'P1  /qty 300  /sno 'S1  /sname 'Smith  /status 20  /city 'London
|-- SSP  /pno 'P2  /qty 400  /sno 'S2  /sname 'Jones  /status 10  /city 'Paris
|-- SSP  /pno 'P1  /qty 300  /sno 'S2  /sname 'Jones  /status 10  /city 'Paris
|-- SSP  /pno 'P2  /qty 200  /sno 'S3  /sname 'Blake  /status 30  /city 'Paris
|-- SSP  /pno 'P5  /qty 400  /sno 'S4  /sname 'Clark  /status 20  /city 'London

|-- SSP  /pno 'P4  /qty 300  /sno 'S4  /sname 'Clark  /status 20  /city 'London
|-- SSP  /pno 'P2  /qty 200  /sno 'S4  /sname 'Clark  /status 20  /city 'London

*** 12 judges

**
**  SUMMARY
**      12 judges on SSP
**      12 judges in total
**
```


[DATA.k]:          https://github.com/seinokatsuhiro/koshucode-example/blob/master/dictionary/DATA.k
[natural-join.k]:  https://github.com/seinokatsuhiro/koshucode-example/blob/master/dictionary/calc/N/natural-join.k

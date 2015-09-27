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

      / data
         * koshucode-baala-data.cabal
         / Koshucode / Baala / Data

      / core
         * koshucode-baala-core.cabal
         / Koshucode / Baala / Core

      / writer
         * koshucode-baala-writer.cabal
         / Koshucode / Baala / Writer

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
         * koshu-syntax.hs
         / Koshucode / Baala / Toolkit
```


Install Haskell
---------------------------------

This software is written in Haskell,
and built with the Cabal packaging system.
Before building koshucode libraries and executables,
please install the [Haskell Platform](http://www.haskell.org/platform/)
or [MinGHC](https://github.com/fpco/minghc) for Windows.
Haskell Platform constains the Haskell compiler and
related libraries and tools like Cabal.



Install koshu command
---------------------------------

There are eight Cabal packages.

 1. [`koshucode-baala-base`](baala/base/koshucode-baala-base.cabal)
 2. [`koshucode-baala-data`](baala/data/koshucode-baala-data.cabal)
 3. [`koshucode-baala-core`](baala/core/koshucode-baala-core.cabal)
 4. [`koshucode-baala-writer`](baala/writer/koshucode-baala-writer.cabal)
 5. [`koshucode-baala-operator`](baala/operator/koshucode-baala-operator.cabal)
 6. [`koshucode-baala-content`](baala/content/koshucode-baala-content.cabal)
 7. [`koshucode-baala-calculator`](baala/calculator/koshucode-baala-calculator.cabal)
     contains [`koshu.hs`](baala/calculator/koshu.hs)
 8. [`koshucode-baala-toolkit`](baala/toolkit/koshucode-baala-toolkit.cabal)

To build and install the `koshu` command inside sandbox, you can use
[`cabal-sandbox-install.sh`](baala/calculator/cabal-sandbox-install.sh).

``` sh
cd koshucode-master/baala/calculator
sh cabal-sandbox-install.sh
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

INSTALL
=================================


Download
---------------------------------

Implementation of a koshu processor
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
         / Koshucode
            / Baala
               / Base
  
      / operator
         * koshucode-baala-operator.cabal
         / Koshucode
            / Baala
               / Minimal
               / Vanilla
  
      / processor
         * koshucode-baala-processor.cabal
         * koshu.hs
         / Koshucode
            / Baala
               / Toolkit
         / example
            / dictionary
               * DATA.k
               * natural-join.k
  
      / toolkit
         * koshucode-baala-toolkit.cabal
         * koshu-change.hs
         * koshu-rdf.hs
         * koshu-syntax.hs
         / Koshucode
            / Baala
               / Toolkit
```


Install Haskell
---------------------------------

Before building koshucode libraries and executables,
please install the [Haskell Platform](http://www.haskell.org/platform/).
This software is written in Haskell,
and built with the Cabal packaging system.



Install koshu command
---------------------------------

Install `koshucode-baala-base` package.
Type `cabal install` under the `base` directory.
This package contains `Koshucode.Baala.Base` module.

``` sh
cd koshucode-master/baala/base
cabal install
```

Install `koshucode-baala-operator` package.
This package contains `Koshucode.Baala.Minimal`
and `Koshucode.Baala.Vanilla` modules.

``` sh
cd koshucode-master/baala/operator
cabal install
```

Install `koshucode-baala-processor` package.
This package contains `koshu` command
and `Koshucode.Baala.Toolkit` module.

``` sh
cd koshucode-master/baala/processor
cabal install
```

Optionally,
you can install `koshucode-baala-toolkit` package.
This package contains additional commands,
e.g., `koshu-change`, `koshu-syntax` and `koshu-rdf`.

``` sh
# optional
cd koshucode-master/baala/toolkit
cabal install
```


Invoke koshu command
---------------------------------

`koshu` command is a relational data processor.
This processor reads calculations and data in koshucode,
and writes results of the calculations.
The order of files is not significant.
You can type also `koshu natural-join.k DATA.k`.

``` sh
cd koshucode-master/baala/processor/example/dictionary
koshu DATA.k natural-join.k
```

With the `-i` option, `koshu` reads data file
from the standard input.

``` sh
koshu -i natural-join.k < DATA.k
```

These commands calculates
[`natural-join.k`](https://github.com/seinokatsuhiro/koshucode/blob/master/baala/processor/example/dictionary/natural-join.k)
for input [`DATA.k`](https://github.com/seinokatsuhiro/koshucode/blob/master/baala/processor/example/dictionary/DATA.k),
and writes the following output data.

```
|-- SSP /pno 'P6' /qty '100' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P5' /qty '100' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P4' /qty '200' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P3' /qty '400' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P2' /qty '200' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P1' /qty '300' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P2' /qty '400' /sno 'S2' /sname 'Jones' /status '10' /city 'Paris'
|-- SSP /pno 'P1' /qty '300' /sno 'S2' /sname 'Jones' /status '10' /city 'Paris'
|-- SSP /pno 'P2' /qty '200' /sno 'S3' /sname 'Blake' /status '30' /city 'Paris'
|-- SSP /pno 'P5' /qty '400' /sno 'S4' /sname 'Clark' /status '20' /city 'London'
|-- SSP /pno 'P4' /qty '300' /sno 'S4' /sname 'Clark' /status '20' /city 'London'
|-- SSP /pno 'P2' /qty '200' /sno 'S4' /sname 'Clark' /status '20' /city 'London'
```


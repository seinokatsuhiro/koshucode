INSTALL
=================================


Download
---------------------------------

Implementation of a Koshu processor
has not released as a software package.
Please download zipped repository file
[`koshucode-master.zip`](https://github.com/seinokatsuhiro/koshucode/archive/master.zip)
from GitHub.
This implementation is an open-source software
under the BSD3 license.


Install Haskell
---------------------------------

Before building koshucode libraries and commands,
please install the [Haskell Platform](http://www.haskell.org/platform/).
This software is written in Haskell,
and built with the Cabal packaging system.


Install koshu command
---------------------------------

Install `koshucode-baala-base` package.
Type `cabal install` under the `base` directory.
This package contains `Koshucode.Baala.Base` module.

``` sh
$ cd baala/base
$ cabal install
```

Install `koshucode-baala-operator` package.
This package contains `Koshucode.Baala.Minimall`
and `Koshucode.Baala.Vanilla` modules.

``` sh
$ cd baala/operator
$ cabal install
```

Install `koshucode-baala-toolkit` package.
This package contains `koshu` command
and `Koshucode.Baala.Toolkit` module.

``` sh
$ cd baala/toolkit
$ cabal install
```


Invoke koshu command
---------------------------------

`koshu` command is a relational data processor.
This processor reads calculations and data in koshucode,
and writes a result of calculation.
The Order of files is not significant.
You can type also `koshu natural-join.k DATA.k`.

``` sh
$ cd koshucode/baala/toolkit/example/dictionary
$ koshu DATA.k natural-join.k
```

With the `-i` option, `koshu` reads data file
from the standard input.

``` sh
$ koshu -i natural-join.k < DATA.k
```

These commands calculates `natural-join.k` for input `DATA.k`,
and writes a following output data.

```
|-- SSP /pno 'P6' /qty '100' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P5' /qty '100' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P4' /qty '200' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P3' /qty '400' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P2' /qty '200' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P1' /qty '300' /sno 'S1' /sname 'Smith' /status '20' /city 'London'
|-- SSP /pno 'P2' /qty '400' /sno 'S2' /sname 'Jones' /status '10' /city 'Pairs'
|-- SSP /pno 'P1' /qty '300' /sno 'S2' /sname 'Jones' /status '10' /city 'Pairs'
|-- SSP /pno 'P2' /qty '200' /sno 'S3' /sname 'Blake' /status '30' /city 'Pairs'
|-- SSP /pno 'P5' /qty '400' /sno 'S4' /sname 'Clark' /status '20' /city 'London'
|-- SSP /pno 'P4' /qty '300' /sno 'S4' /sname 'Clark' /status '20' /city 'London'
|-- SSP /pno 'P2' /qty '200' /sno 'S4' /sname 'Clark' /status '20' /city 'London'
```


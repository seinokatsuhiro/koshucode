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
Files `*.k` are data itself and calculation in Koshucode.

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
Before building Koshucode libraries and executables,
please install the [Haskell Platform](http://www.haskell.org/platform/)
or [MinGHC](https://github.com/fpco/minghc) for Windows.
Haskell Platform constains the Haskell compiler and
related libraries and tools like Cabal.



Install koshu command
---------------------------------

There are eleven Cabal packages.

 1. [`koshucode-baala-base`](baala/base/koshucode-baala-base.cabal)
 2. [`koshucode-baala-syntax`](baala/syntax/koshucode-baala-syntax.cabal)
 3. [`koshucode-baala-data`](baala/data/koshucode-baala-data.cabal)
 4. [`koshucode-baala-core`](baala/core/koshucode-baala-core.cabal)
 5. [`koshucode-baala-writer`](baala/writer/koshucode-baala-writer.cabal)
 6. [`koshucode-baala-rop-flat`](baala/rop-flat/koshucode-baala-rop-flat.cabal)
 7. [`koshucode-baala-rop-nested`](baala/rop-nested/koshucode-baala-rop-nested.cabal)
 8. [`koshucode-baala-rop-cox`](baala/rop-cox/koshucode-baala-rop-cox.cabal)
 9. [`koshucode-baala-cop`](baala/cop/koshucode-baala-cop.cabal)
 10. [`koshucode-baala-calculator`](baala/calculator/koshucode-baala-calculator.cabal)
     contains [`koshu.hs`](baala/calculator/koshu.hs)
 11. [`koshucode-baala-toolkit`](baala/toolkit/koshucode-baala-toolkit.cabal)

To build and install the `koshu` command inside sandbox, you can use
[`cabal-sandbox-install.sh`](baala/calculator/cabal-sandbox-install.sh).

``` sh
cd koshucode-master/baala/calculator
sh cabal-sandbox-install.sh
```



Calculation
---------------------------------

The `koshu` command is a portable data calculator.
This calculator reads calculation formula and data in Koshucode,
and writes result data of the calculation.
The result data are also written in Koshucode.

For example, dependency table for implementation modules
are calculated by the following command.

``` sh
cd koshucode-master/baala
koshu import/dir-rank.k base/data/IMPORT.k
```

The order of files is not significant.
You can also type:

``` sh
koshu base/data/IMPORT.k import/dir-rank.k
```

These commands calculate [`import/dir-rank.k`][dir-rank.k]
for input data [`base/data/IMPORT.k`][IMPORT.k]
and writes the following output data.

``` text
** -*- koshu -*-
**
**  INPUT
**    import/dir-rank.k
**    base/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Assoc" | [ ] ] [ 1 | "List" | [ ] ] [ 1 | "Pair" | [ ] ] [ 1 | "Snip" | [ ] ] [ 2 | "Order" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 4 | "IOPoint" | [ "Prelude" ] ] [ 4 | "TextTable" | [ "Prelude" ] ] [ 4 | "Utility" | [ "Prelude" ] ] [ 4 | "Write" | [ "Prelude" ] ] [ 5 | "CodePt" | [ "Prelude" ] ] [ 5 | "Http" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 7 | "Reason" | [ "Text" ] ] [ 8 | "Abortable" | [ "Prelude" | "Text" ] ] [ 8 | "Report" | [ "Prelude" | "Text" ] ] [ 10 | "Message" | [ "Abort" ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Base.Syntax"  /base-rank {= /rank /base /import-dir [ 10 | "Message" | [ "Abort" | "Prelude" | "Text" ] ] [ 11 | "Line" | [ "Abort" | "Prelude" | "Text" ] ] [ 11 | "Tree" | [ "Abort" | "Prelude" | "Text" ] ] [ 12 | "Clause" | [ "Prelude" | "Text" ] ] [ 12 | "Infix" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 13  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 3 | "Prelude" | [ ] ] [ 6 | "Text" | [ ] ] [ 9 | "Abort" | [ ] ] [ 11 | "Message" | [ ] ] [ 13 | "Syntax" | [ ] ] =}

|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 14 | "Base" | [ "Abort" | "Prelude" | "Syntax" | "Text" ] ] =}

*** 6 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                             /base-rank
  --------- -------------------------------- -----------------------------------------------------------------
  2         "Koshucode.Baala.Base.Prelude"   /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             0     "Class"       [ ]
                                             0     "Import"      [ ]
                                             1     "Assoc"       [ ]
                                             1     "List"        [ ]
                                             1     "Pair"        [ ]
                                             1     "Snip"        [ ]
                                             2     "Order"       [ ]
                                             
  5         "Koshucode.Baala.Base.Text"      /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             0     "Comment"     [ ]
                                             0     "Unicode"     [ ]
                                             4     "IOPoint"     [ "Prelude" ]
                                             4     "TextTable"   [ "Prelude" ]
                                             4     "Utility"     [ "Prelude" ]
                                             4     "Write"       [ "Prelude" ]
                                             5     "CodePt"      [ "Prelude" ]
                                             5     "Http"        [ "Prelude" ]
                                             
  10        "Koshucode.Baala.Base.Abort"     /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             7     "Reason"      [ "Text" ]
                                             8     "Abortable"   [ "Prelude" | "Text" ]
                                             8     "Report"      [ "Prelude" | "Text" ]
                                             10    "Message"     [ "Abort" ]
                                             
  12        "Koshucode.Baala.Base.Syntax"    /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             10    "Message"     [ "Abort" | "Prelude" | "Text" ]
                                             11    "Line"        [ "Abort" | "Prelude" | "Text" ]
                                             11    "Tree"        [ "Abort" | "Prelude" | "Text" ]
                                             12    "Clause"      [ "Prelude" | "Text" ]
                                             12    "Infix"       [ "Prelude" ]
                                             
  13        "Koshucode.Baala.Base"           /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             3     "Prelude"     [ ]
                                             6     "Text"        [ ]
                                             9     "Abort"       [ ]
                                             11    "Message"     [ ]
                                             13    "Syntax"      [ ]
                                             
  14        "Koshucode.Baala"                /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             14    "Base"        [ "Abort" | "Prelude" | "Syntax" | "Text" ]
                                             

=== rel

**
**  SUMMARY
**       6 judges on DIR-RANK
**       6 judges in total
**
```


[IMPORT.k]: baala/base/data/IMPORT.k
[dir-rank.k]: baala/import/dir-rank.k

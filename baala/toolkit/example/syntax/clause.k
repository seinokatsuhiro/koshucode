#!/usr/bin/env koshu-syntax --run
**
**  DESCRIPTION
**    Examples of all types of clauses.
**
**  USAGE
**    $ koshu-syntax clause.k
**

**  Affirmed judge
|-- A /x 0
      /y 1

**  Denied judge
|-X A /x 1
      /y 0

**  Affirmed relation
affirm B b

**  Denied relation
deny C c

**  Relmap
a   : source A /x /y
b   : a | cut /x
        | cut /y

**  Comment
**** a : source A /x /y
     a : source A /x /y
     a : source A /x /y

**  Unknown
xxx yyy
    zzz


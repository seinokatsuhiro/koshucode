#!/usr/bin/env koshu --run
**  -*- koshu -*-
**
**  DESCRIPTION
**    Example of double stroke delimiter.
**    Double stroke delimiters are used to
**    write one-liners of koshu command.
**
**  USAGE
**    koshu double-stroke.k
**    koshu-syntax double-stroke.k
**

|-- A /x 10 /y 20
|-- A /x 20 /y 30

|-- B /y 20 /z 0
|-- B /y 30 /z 10

**  Conventional
a : source A /x /y
b : source B /y /z
affirm C a | meet b

**  Double stroke
a : source A /x /y || b : source B /y /z || affirm D a | meet b

**  Packed
affirm E source A /x /y | meet (source B /y /z)


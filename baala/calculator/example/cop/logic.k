** -*- koshu -*-
**
**  DESCRIPTION
**    Examples for Koshucode.Baala.Vanilla.Cop.Logic
**
**  USAGE
**    koshu logic.k
**

|-- BOOLEAN  /x #true
|-- BOOLEAN  /x #false

bool : source BOOLEAN /x

a : bool | rename /a /x
b : bool | rename /b /x
c : a | meet b

affirm C c

affirm D
  c | add /and ( /a and /b )
          /or  ( /a or /b )
          /not ( not /a )


** -*- koshu -*-
**
**  DESCRIPTION
**    Examples for Koshucode.Baala.Vanilla.Cop.Arith
**
**  USAGE
**    koshu arith.k
**

|-- INT   /a int 2
|-- INT   /a int -3

a : source INT /a

affirm A a

affirm B
    a | add /add   ( /a + 10 )
            /minus ( /a - 10 )
            /mult  ( /a * 3 )
            /abs   ( abs /a )

affirm C
    a | add /add ( + /a 1 /a 2 )

affirm D
    a | add /abs ( abs [ /a ] )


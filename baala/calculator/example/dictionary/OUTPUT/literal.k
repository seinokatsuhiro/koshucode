** -*- koshu -*-
**  
**  INPUT
**    DATA.k
**    literal.k
**    

|-- INTEGER /literal 4
|-- INTEGER /literal -5
|-- CHAR /literal ABC
|-- CHAR /literal 'ABC DEF'
|-- BOOLEAN /literal #true

|-- BOOLEAN /literal #false
|-- TUPLE /literal { /sno S1 /pno P1 /qty 300 }
|-- TUPLE /literal { /sno S1 /pno P2 /qty 200 }
|-- TUPLE /literal { /sno S1 }
|-- TUPLE /literal { }

|-- RELATION /literal {| /sno /pno /qty | S1 P1 300 | S1 P2 200 |}
|-- RELATION /literal {| /sno /pno /qty | S1 P1 300 |}
|-- RELATION /literal {| /sno /pno /qty |}
|-- RELATION /literal {| |}
|-- RELATION /literal {| | |}

*** (15 judges)


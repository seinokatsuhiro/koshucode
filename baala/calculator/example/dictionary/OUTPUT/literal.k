** -*- koshu -*-
**  
**  INPUT
**    DATA.k
**    literal.k
**    

|-- INTEGER  /literal int 4
|-- INTEGER  /literal int -5
|-- CHAR  /literal ABC
|-- CHAR  /literal 'ABC DEF'
|-- BOOLEAN  /literal #true

|-- BOOLEAN  /literal #false
|-- TERMSET  /literal {| /sno S1 /pno P1 /qty 300 |}
|-- TERMSET  /literal {| /sno S1 /pno P2 /qty 200 |}
|-- TERMSET  /literal {| /sno S1 |}
|-- TERMSET  /literal {| |}

|-- RELATION  /literal [| /sno /pno /qty | S1 P1 300 | S1 P2 200 |]
|-- RELATION  /literal [| /sno /pno /qty | S1 P1 300 |]
|-- RELATION  /literal [| /sno /pno /qty |]
|-- RELATION  /literal [| |]
|-- RELATION  /literal [| | |]

**  (15 judges)


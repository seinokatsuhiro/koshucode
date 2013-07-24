** -*- koshu -*-
**  
**  INPUT
**    DATA.k
**    literal.k
**    

|-- INTEGER  /lit n 4
|-- INTEGER  /lit n -5
|-- CHAR  /lit ABC
|-- CHAR  /lit 'ABC DEF'
|-- CHAR  /lit q  #qq 'I am double quoted.' #qq

|-- BOOLEAN  /lit #true
|-- BOOLEAN  /lit #false
|-- TERMSET  /lit <| /sno S1 /pno P1 /qty n 300 |>
|-- TERMSET  /lit <| /sno S1 /pno P2 /qty n 200 |>
|-- TERMSET  /lit <| /sno S1 |>

|-- TERMSET  /lit <| |>
|-- RELATION  /lit {| /sno /pno /qty | S1 P1 300 | S1 P2 200 |}
|-- RELATION  /lit {| /sno /pno /qty | S1 P1 300 |}
|-- RELATION  /lit {| /sno /pno /qty |}
|-- RELATION  /lit {| |}

|-- RELATION  /lit {| | |}
**  (16 judges)


** -*- koshu -*-

|-- INTEGER   /lit n 4
|-- INTEGER   /lit n -5

|-- CHAR      /lit 'ABC'
|-- CHAR      /lit 'ABC DEF'
|-- CHAR      /lit '"I am double quoted."'

|-- BOOLEAN   /lit #true
|-- BOOLEAN   /lit #false

|-- TERMSET   /lit <| /sno S1  /pno P1  /qty n 300 |>
|-- TERMSET   /lit <| /sno S1  /pno P2  /qty n 200 |>
|-- TERMSET   /lit <| /sno S1 |>
|-- TERMSET   /lit <| |>

|-- RELATION  /lit {| /sno /pno /qty | S1 P1 300 | S1 P2 200 |}
|-- RELATION  /lit {| /sno /pno /qty | S1 P1 300 |}
|-- RELATION  /lit {| /sno /pno /qty |}
|-- RELATION  /lit {| |}        ** reldum
|-- RELATION  /lit {| | |}      ** reldee
|-- RELATION  /lit {| | | |}    ** reldee

affirm INTEGER   source INTEGER  /lit
affirm CHAR      source CHAR     /lit
affirm BOOLEAN   source BOOLEAN  /lit
affirm TERMSET   source TERMSET  /lit
affirm RELATION  source RELATION /lit


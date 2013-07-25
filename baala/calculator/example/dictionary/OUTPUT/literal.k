** -*- koshu -*-
**  
**  INPUT
**    DATA.k
**    literal.k
**    

|-- INTEGER  /lit 4
|-- INTEGER  /lit -5
|-- CHAR  /lit 'ABC #q
|-- CHAR  /lit 'ABCDEF #q
|-- CHAR  /lit ''I am double quoted.' #q

|-- BOOLEAN  /lit #true
|-- BOOLEAN  /lit #false
|-- SET  /lit {}
|-- SET  /lit {'ABC #q}
|-- SET  /lit {'ABC #q : 'DEF #q}

|-- SET  /lit {'ABC : 'DEF : {'G #q : 'HI #q}}
|-- TERMSET  /lit <|/sno 'S1 /pno 'P1 /qty 300|>
|-- TERMSET  /lit <|/sno 'S1 /pno 'P2 /qty 200|>
|-- TERMSET  /lit <|/sno 'S1|>
|-- TERMSET  /lit <||>

|-- RELATION  /lit {| /sno /pno /qty | 'S1 'P1 300 | 'S1 'P2 200 |}
|-- RELATION  /lit {| /sno /pno /qty | 'S1 'P1 300 |}
|-- RELATION  /lit {| /sno /pno /qty |}
|-- RELATION  /lit {| /sno /pno /qty | () () () |}
|-- RELATION  /lit {| /sno | () |}
**  (20 judges)

|-- RELATION  /lit {| |}
**  (21 judges)


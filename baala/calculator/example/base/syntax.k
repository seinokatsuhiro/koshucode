** -*- koshu -*-

affirm WORD    source WORD /x    | add /n ( length /x )
affirm SPECIAL source SPECIAL /x | add /n ( length /x )
affirm UNICODE source UNICODE /項目名 | add /n ( length /項目名 )

** word

|-- WORD  /x 'abc def    ** comment
|-- WORD  /x ''abc (def) ** not comment
|-- WORD  /x ( 'def )
|-- WORD  /x ( '#/ y )

** special character

|-- SPECIAL  /x '#(
|-- SPECIAL  /x '#)

|-- SPECIAL  /x '#[
|-- SPECIAL  /x '#]

|-- SPECIAL  /x '#{
|-- SPECIAL  /x '#}

|-- SPECIAL  /x '#/
|-- SPECIAL  /x '#:

** unicode

|-- UNICODE  /項目名 '項目内容


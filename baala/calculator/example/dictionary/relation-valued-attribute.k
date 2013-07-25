** -*- koshu -*-

|-- A  /r [| /a /b
           | 'A1 : (int 10)
           | 'A2 : (int 20)
           | 'A3 : (int 30)
           |]

|-- A  /r [| /a /b
           | 'A4 : (int 10)
           | 'A5 : (int 20)
           |]

a : source A /r

affirm A
  a | add /size ( length /r )
    | cut /r


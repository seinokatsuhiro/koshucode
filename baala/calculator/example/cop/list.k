** -*- koshu -*-
**
**  DESCRIPTION
**    Examples for Koshucode.Baala.Vanilla.Cop.List
**
**  USAGE
**    koshu list.k
**

|-- INT   /a [ 1 : 2 : 3 ]
|-- TEXT  /a [ 'a : 'b : 'c ]

int  : source INT /a
text : source TEXT /a

affirm INT  | int
affirm TEXT | text

affirm A
  | int
  | add /length  ( length /a )
        /min     ( min /a )
        /max     ( max /a )
        /total   ( total /a )

affirm B
  | text
  | add /length  ( length /a )
        /min     ( min /a )
        /max     ( max /a )

affirm C
  | reldee
  | add /list    ( list ('a) ('b) ('c) )
        /length  ( length ('abc) )

affirm D
  | reldee
  | add /list-a  [ 1 : 2 ]
        /list-b  [ 2 : 3 : 4 ]
        /set-a   { 1 : 2 }
        /set-b   { 2 : 3 : 4 }
  | add /list-union      ( /list-a ++ /list-b )
        /set-union       ( /set-a  ++ /set-b )
        /list-intersect  ( /list-a intersect /list-b )
        /set-intersect   ( /set-a  intersect /set-b )
        /list-minus      ( /list-a  minus /list-b )
        /set-minus       ( /set-a   minus /set-b )
  | cut /list-a /list-b
        /set-a  /set-b

affirm F
  | reldee
  | add /a 'aa
        /b 'bb
        /c 'cc
  | add /d ( /a ++ /b ++ /c )


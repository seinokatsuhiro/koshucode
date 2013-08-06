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

affirm INT int
affirm TEXT text

affirm A int
  | add /length  ( length /a )
        /min     ( min /a )
        /max     ( max /a )
        /total   ( total /a )

affirm B text
  | add /length  ( length /a )
        /min     ( min /a )
        /max     ( max /a )

affirm C reldee
  | add /list    ( list ('a) ('b) ('c) )
        /length  ( length ('abc) )

affirm D reldee
  | add /list    ( [ 1 : 2 ] ++ [ 2 : 3 : 4 ] )
        /set     ( { 1 : 2 } ++ { 2 : 3 : 4 } )

affirm E reldee
  | add /a ('aa)
        /b ('bb)
        /c ('cc)
  | add /d ( /a ++ /b ++ /c )


** -*- koshu -*-
**
**  DESCRIPTION
**    Examples for Koshucode.Baala.Vanilla.Cop.List
**
**  USAGE
**    koshu list.k
**

|-- INT   /a [ (int 1) (int 2) (int 3) ]
|-- TEXT  /a [ 'a' 'b' 'c' ]

int  : source INT /a
text : source TEXT /a

affirm INT int
affirm TEXT text

affirm A int
  | add /size  ( size /a )
        /min   ( min /a )
        /max   ( max /a )
        /total ( total /a )

affirm B text
  | add /size  ( size /a )
        /min   ( min /a )
        /max   ( max /a )

affirm C reldee
  | add /list  ( list a b c )
        /size  ( size 'abc' )


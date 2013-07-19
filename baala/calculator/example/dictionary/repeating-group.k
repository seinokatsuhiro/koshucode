** -*- koshu -*-

p    : source P  /pno /pname
sp   : source SP /sno /pno /qty

pno  : sp
     | pick /pno
     | hang -term /r -relmap sp
     | add /qty-list  ( /r/qty )
           /qty-total ( total /r/qty )
     | cut /r
     | meet p

affirm P pno


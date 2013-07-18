** -*- koshu -*-
**
**  DESCRIPTION
**    Matrix multiplication
**
**  USAGE
**    koshu MATRIX.k mult.k
**

**** **********************
     a  =  [/a /b /x]
           2  3  4
           1  0  0

     b  =  [/b /c /y]
           0 1000
           1  100
           0   10

     c  =  a * b
        =  [/a /c /z]
           3 2340
           0 1000

a  : source MATRIX /name /row /col /val
   | hold /name = a
   | cut /name
   | rename /a /row
            /b /col
            /x /val

b  : source MATRIX /name /row /col /val
   | hold /name = b
   | cut /name
   | rename /b /row
            /c /col
            /y /val

c1 : a
   | meet b
   | add /z ( /x * /y )
   | cut /x /y

c2 : c1
   | pick /a /c

c3 : c2
   | hang -term /r -relmap c1
   | add /z ( total /r/z )
   | cut /r

affirm C3 c3


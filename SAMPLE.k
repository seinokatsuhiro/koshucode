** -*- koshu -*-
**
**  USAGE
**    koshu SAMPLE.k
**

**  Input

|-- P  /a 1  /b 40
|-- P  /a 1  /b 50
|-- P  /a 2  /b 60

**  Output

|== Q : p | a 100
|== R : p | a 200 | keep /a = 1

**  Relmap

p : source P /a /b
a : add /c ( /a + /b + @'1 )

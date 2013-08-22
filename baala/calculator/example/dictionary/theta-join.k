** -*- koshu -*-

s    : source S  /city | rename /sc /city
p    : source P  /city | rename /pc /city

sp   : s | meet p
         | hold /sc > /pc

affirm S  | s
affirm P  | p
affirm SP | sp


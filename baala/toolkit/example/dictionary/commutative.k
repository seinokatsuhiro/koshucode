** -*- koshu -*-

s     : source S  /sno /sname /status /city
sp    : source SP /sno /pno /qty

s-sp  : s  | meet sp
sp-s  : sp | meet s

affirm S-SP s-sp
affirm SP-S sp-s


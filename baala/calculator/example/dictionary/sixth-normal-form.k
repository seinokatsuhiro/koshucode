** -*- koshu -*-

s        : source S  /sno /sname /status /city
s-sno    : s | pick /sno
s-sname  : s | pick /sno /sname
s-status : s | pick /sno /status
s-city   : s | pick /sno /city

affirm S-SNO    | s-sno
affirm S-SNAME  | s-sname
affirm S-STATUS | s-status
affirm S-CITY   | s-city


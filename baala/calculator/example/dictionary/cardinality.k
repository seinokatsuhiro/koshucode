** -*- koshu -*-

s    : source S  /sno /sname /status /city
p    : source P  /pno /pname /color /weight /city
sp   : source SP /sno /pno /qty

cs   : s  | size /card
cp   : p  | size /card
csp  : sp | size /card

affirm CS  | cs
affirm CP  | cp
affirm CSP | csp


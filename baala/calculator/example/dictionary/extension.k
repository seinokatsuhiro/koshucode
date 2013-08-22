** -*- koshu -*-

p    : source P  /pno /pname /color /weight /city
p2   : p | add /gmwt ( /weight * 454 )

affirm P | p2


** -*- koshu -*-

p    : source P  /pno /pname /color /weight /city
pw   : p | rank -add /w -order /weight
pq   : pw | hold /w <= int 3 | cut /w

affirm P pq


** -*- koshu -*-

s     : source S  /sno /sname
p     : source P  /pno /pname
sp    : source SP /sno /pno /qty

left  : (sp | meet s) | meet p
right : sp | meet (s | meet p)

affirm Left  left
affirm RIGHT right


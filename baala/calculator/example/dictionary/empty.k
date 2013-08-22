** -*- koshu -*-

s    : source S  /sno /sname /status /city
all  : s | size /size
zero : s | empty | size /size

affirm EMPTY-SIZE | all
affirm EMPTY-ZERO | zero


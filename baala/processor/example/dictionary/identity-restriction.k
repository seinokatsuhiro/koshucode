** -*- koshu -*-

s    : source S  /sno /sname /status /city
s2   : s | hold /status <> 25.0
s3   : s | hold /status = /status

affirm S2  s2
affirm S3  s3


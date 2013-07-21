** -*- koshu -*-

iso-639 : source ISO-639-2 /alpha3 /alpha3t /alpha2 /english /french

alp3    : iso-639 | pick /alpha3 /english /french
alp2    : iso-639 | pick /alpha3 /alpha2  | hold /alpha2  <> ()
term    : iso-639 | pick /alpha3 /alpha3t | hold /alpha3t <> ()

affirm ISO-639-2 alp3
affirm ISO-639-2-TERM term
affirm ISO-639-2-SHORT alp2


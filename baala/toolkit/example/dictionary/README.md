This directory contains examples of Koshucode
translated from C. J. Date's relational database
dictionary.


FILES
----------------------

* File `SP.d` contains schema in Tutorial D
  and data in tabluar form.
  
* File `DATA.k` contains a dataset in Koshucode.
  
* Files `[a-z]*.k` contains a calculations in Koshucode,
  translated from Date's dictionary.
  Filenames are correspond to heading words in the dictionary.

  
USAGE
----------------------

Run `koshu` command to calculate `natural-join.k`
for dataset `DATA.k`.

``` sh
$ koshu natural-join.k DATA.k
```

You can change the order of files.

``` sh
$ koshu DATA.k natural-join.k
```


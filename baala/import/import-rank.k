#!/usr/bin/env koshu
** -*- koshu -*-
**
**  DESCRIPTION
**    Dependent rank of importing modules.
**
**  USAGE
**    ./import-rank.k ../base/IMPORT.k
**    ./import-rank.k ../*/IMPORT.k
**

|== IMPORT-RANK
  : source IMPORT /module /import
  | keep /import =* "Koshucode"
  | dependent-rank /module /import -rank /rank
  | interp {| /module has dependent rank /rank . |}
  --order --forward /rank

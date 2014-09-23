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

|== IMPORT-RANK -order -fore /rank
       : source IMPORT /module /import
       | keep part "Koshucode" /import
       | dependent-rank /module /import -rank /rank
       | interp <<< /module has dependent rank /rank . >>>

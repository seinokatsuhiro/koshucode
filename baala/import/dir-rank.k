#!/usr/bin/env koshu
** -*- koshu -*-
**
**  DESCRIPTION
**    Dependent rank on module directories.
**
**  USAGE
**    ./dir-rank.k ../base/IMPORT.k
**    ./dir-rank.k ../*/IMPORT.k
**

dep    : source IMPORT /module /import
       | keep part "Koshucode" /import
       | dependent-rank /module /import -rank /rank
       | interp <<< /module has dependent rank /rank . >>>

dir    : dep
       | add /dir  ( dir-part  <dot> /module )
             /base ( base-part <dot> /module )
       | pick /dir /rank /base
       | hang /base-rank -on /dir
       | add /dir-rank ( max /base-rank/rank )
       | interp <<< Module directory /dir has dependent rank /dir-rank ,
                    /base-rank exists under the /dir . >>>

|== DIR-RANK -order -table -fore /dir-rank /dir
       : dir

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

|== DIR-RANK : dir
  --order --table --forward /dir-rank /dir

dir : dep
    | add /dir  ( dir-part  <dot> /module )
          /base ( base-part <dot> /module )
    | hier /dir -to /base-rank
    | for /base-rank ( pick /rank /base /import-dir )
    | add /dir-rank ( max /base-rank/rank )
    | interp {| Module directory /dir has dependent rank /dir-rank ,
                /base-rank exists under the /dir . |}

dep : imp
    | partial-order-height /import /module -to /module /rank
    | group ( imp | meet imp-dir ) -to /=import
    | add /import-dir ( sort /=import/dirname )
    | wipe
    | interp {| /module has dependent rank /rank .
                The module imports directory modules /import-dir . |}

imp-dir : imp
    | pick /module
    | add /import   ( dir-part  <dot> /module )
    | add /dirname  ( base-part <dot> /import )
    | pick /import /dirname
    | interp {| Module directory /import can be imported from other module.
                The base part of /import is /dirname . |}

imp : source IMPORT /module /import
    | keep /import =* "Koshucode"

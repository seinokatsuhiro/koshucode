# I/O List

- [./dir-rank.k](#dir-rankk)
- ./dir-rank.k [../base/data/IMPORT.k](#basedataimportk)
- ./dir-rank.k [../data/data/IMPORT.k](#datadataimportk)
- ./dir-rank.k [../core/data/IMPORT.k](#coredataimportk)
- ./dir-rank.k [../writer/data/IMPORT.k](#writerdataimportk)
- ./dir-rank.k [../rop-flat/data/IMPORT.k](#rop-flatdataimportk)
- ./dir-rank.k [../rop-nested/data/IMPORT.k](#rop-nesteddataimportk)
- ./dir-rank.k [../rop-cox/data/IMPORT.k](#rop-coxdataimportk)
- ./dir-rank.k [../calculator/data/IMPORT.k](#calculatordataimportk)
- ./dir-rank.k [../toolkit/data/IMPORT.k](#toolkitdataimportk)



## [./dir-rank.k](./dir-rank.k)

```
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
```



## ../base/data/IMPORT.k

```
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    110 judges
**

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.Abort.Reason"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Message"        /import "Koshucode.Baala.Base.Abort"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Reason"         /import "Koshucode.Baala.Base.Text"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Abort.Reason"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Text"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Abortable"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Reason"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Report"

|-- IMPORT  /module "Koshucode.Baala.Base.Message"              /import "Koshucode.Baala.Base.Abort.Message"
|-- IMPORT  /module "Koshucode.Baala.Base.Message"              /import "Koshucode.Baala.Base.Syntax.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Assoc"        /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Assoc"        /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Assoc"        /import "Koshucode.Baala.Base.Prelude.Class"


|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Tuple"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "GHC.IO.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "System.Exit"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Text.PrettyPrint"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.List"         /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.List"         /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.List"         /import "Koshucode.Baala.Base.Prelude.Class"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Order"        /import "Koshucode.Baala.Base.Prelude.Class"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Order"        /import "Koshucode.Baala.Base.Prelude.Import"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Order"        /import "Koshucode.Baala.Base.Prelude.Snip"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Order"        /import "Koshucode.Baala.Base.Prelude.Pair"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Pair"         /import "Koshucode.Baala.Base.Prelude.Class"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Snip"         /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Snip"         /import "Koshucode.Baala.Base.Prelude.Import"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Snip"         /import "Koshucode.Baala.Base.Prelude.Class"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Assoc"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Class"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Import"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Order"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Pair"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Snip"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Clause"        /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Clause"        /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Clause"        /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Clause"        /import "Koshucode.Baala.Base.Syntax.Line"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Koshucode.Baala.Base.Syntax.Tree"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Koshucode.Baala.Base.Syntax.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Message"       /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Message"       /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Message"       /import "Koshucode.Baala.Base.Text"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Syntax.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Clause"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Infix"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Line"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Tree"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.CodePt"          /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.CodePt"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.CodePt"          /import "Koshucode.Baala.Base.Text.IOPoint"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Comment"         /import "System.IO"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Data.ByteString.Char8"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Data.ByteString.Lazy.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Control.Exception"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Network.HTTP.Conduit"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Network.HTTP.Types.Status"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Network.URI"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Koshucode.Baala.Base.Text.Utility"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.IOPoint"         /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.IOPoint"         /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Unicode"         /import "Data.Char"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Text.Blaze"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Text.Blaze.Html.Renderer.Pretty"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Text.Blaze.Html.Renderer.String"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Text.Blaze.XHtml5"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Text.Blaze.XHtml5.Attributes"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Text.PrettyPrint"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.IOPoint"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.CodePt"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Comment"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Http"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.TextTable"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Unicode"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Utility"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Write"

|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Text"

```

Command `./dir-rank.k ../base/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../base/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Assoc" | [ ] ] [ 1 | "List" | [ ] ] [ 1 | "Pair" | [ ] ] [ 1 | "Snip" | [ ] ] [ 2 | "Order" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 4 | "IOPoint" | [ "Prelude" ] ] [ 4 | "TextTable" | [ "Prelude" ] ] [ 4 | "Utility" | [ "Prelude" ] ] [ 4 | "Write" | [ "Prelude" ] ] [ 5 | "CodePt" | [ "Prelude" ] ] [ 5 | "Http" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 7 | "Reason" | [ "Text" ] ] [ 8 | "Abortable" | [ "Prelude" | "Text" ] ] [ 8 | "Report" | [ "Prelude" | "Text" ] ] [ 10 | "Message" | [ "Abort" ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Base.Syntax"  /base-rank {= /rank /base /import-dir [ 10 | "Message" | [ "Abort" | "Prelude" | "Text" ] ] [ 11 | "Line" | [ "Abort" | "Prelude" | "Text" ] ] [ 11 | "Tree" | [ "Abort" | "Prelude" | "Text" ] ] [ 12 | "Clause" | [ "Prelude" | "Text" ] ] [ 12 | "Infix" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 13  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 3 | "Prelude" | [ ] ] [ 6 | "Text" | [ ] ] [ 9 | "Abort" | [ ] ] [ 11 | "Message" | [ ] ] [ 13 | "Syntax" | [ ] ] =}

|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 14 | "Base" | [ "Abort" | "Prelude" | "Syntax" | "Text" ] ] =}

*** 6 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                             /base-rank
  --------- -------------------------------- -----------------------------------------------------------------
  2         "Koshucode.Baala.Base.Prelude"   /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             0     "Class"       [ ]
                                             0     "Import"      [ ]
                                             1     "Assoc"       [ ]
                                             1     "List"        [ ]
                                             1     "Pair"        [ ]
                                             1     "Snip"        [ ]
                                             2     "Order"       [ ]
                                             
  5         "Koshucode.Baala.Base.Text"      /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             0     "Comment"     [ ]
                                             0     "Unicode"     [ ]
                                             4     "IOPoint"     [ "Prelude" ]
                                             4     "TextTable"   [ "Prelude" ]
                                             4     "Utility"     [ "Prelude" ]
                                             4     "Write"       [ "Prelude" ]
                                             5     "CodePt"      [ "Prelude" ]
                                             5     "Http"        [ "Prelude" ]
                                             
  10        "Koshucode.Baala.Base.Abort"     /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             7     "Reason"      [ "Text" ]
                                             8     "Abortable"   [ "Prelude" | "Text" ]
                                             8     "Report"      [ "Prelude" | "Text" ]
                                             10    "Message"     [ "Abort" ]
                                             
  12        "Koshucode.Baala.Base.Syntax"    /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             10    "Message"     [ "Abort" | "Prelude" | "Text" ]
                                             11    "Line"        [ "Abort" | "Prelude" | "Text" ]
                                             11    "Tree"        [ "Abort" | "Prelude" | "Text" ]
                                             12    "Clause"      [ "Prelude" | "Text" ]
                                             12    "Infix"       [ "Prelude" ]
                                             
  13        "Koshucode.Baala.Base"           /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             3     "Prelude"     [ ]
                                             6     "Text"        [ ]
                                             9     "Abort"       [ ]
                                             11    "Message"     [ ]
                                             13    "Syntax"      [ ]
                                             
  14        "Koshucode.Baala"                /rank /base         /import-dir
                                             ----- ------------- ---------------------------------------------
                                             14    "Base"        [ "Abort" | "Prelude" | "Syntax" | "Text" ]
                                             

=== rel

**
**  SUMMARY
**       6 judges on DIR-RANK
**       6 judges in total
**
```



## ../data/data/IMPORT.k

```
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    167 judges
**

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Content"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Data.Church.Cox"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Data.Church.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Data.Content.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Content"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Church.Build"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Content.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Church.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church"               /import "Koshucode.Baala.Data.Church.Build"
|-- IMPORT  /module "Koshucode.Baala.Data.Church"               /import "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT  /module "Koshucode.Baala.Data.Church"               /import "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT  /module "Koshucode.Baala.Data.Church"               /import "Koshucode.Baala.Data.Church.Run"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Content.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Class"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Class"        /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Class"        /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Class"        /import "Koshucode.Baala.Data.Content.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Tree"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Message"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Message"      /import "Koshucode.Baala.Data.Token"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Data.Content.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.BaalaC"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Decode"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Tree"

|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Church.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Content.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Token.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Token.AngleText"      /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.AngleText"      /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Data.Token.Message"        /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Data.Token.Next"           /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.Next"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.Next"           /import "Koshucode.Baala.Data.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Token.Short"          /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.Short"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.Short"          /import "Koshucode.Baala.Data.Token.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.Short"          /import "Koshucode.Baala.Data.Token.Next"


|-- IMPORT  /module "Koshucode.Baala.Data.Token.Token"          /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.Token"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.Token"          /import "Koshucode.Baala.Data.Token.Term"

|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenClause"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenClause"    /import "Koshucode.Baala.Data.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenClause"    /import "Koshucode.Baala.Data.Token.TokenLine"

|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenLine"      /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenLine"      /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenLine"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenLine"      /import "Koshucode.Baala.Data.Token.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenLine"      /import "Koshucode.Baala.Data.Token.Next"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenLine"      /import "Koshucode.Baala.Data.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenLine"      /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenLine"      /import "Koshucode.Baala.Data.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenTree"      /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenTree"      /import "Text.PrettyPrint"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenTree"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenTree"      /import "Koshucode.Baala.Data.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Token.TokenTree"      /import "Koshucode.Baala.Data.Token.TokenLine"

|-- IMPORT  /module "Koshucode.Baala.Data.Token"                /import "Koshucode.Baala.Data.Token.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Data.Token"                /import "Koshucode.Baala.Data.Token.Next"
|-- IMPORT  /module "Koshucode.Baala.Data.Token"                /import "Koshucode.Baala.Data.Token.Short"
|-- IMPORT  /module "Koshucode.Baala.Data.Token"                /import "Koshucode.Baala.Data.Token.Term"
|-- IMPORT  /module "Koshucode.Baala.Data.Token"                /import "Koshucode.Baala.Data.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Token"                /import "Koshucode.Baala.Data.Token.TokenClause"
|-- IMPORT  /module "Koshucode.Baala.Data.Token"                /import "Koshucode.Baala.Data.Token.TokenLine"
|-- IMPORT  /module "Koshucode.Baala.Data.Token"                /import "Koshucode.Baala.Data.Token.TokenTree"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.About"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.About"           /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.About"           /import "Koshucode.Baala.Data.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Arithmetic"      /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Arithmetic"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Arithmetic"      /import "Koshucode.Baala.Data.Type.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Arithmetic"      /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Clock"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Clock"           /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Date"            /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Date"            /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Date"            /import "Data.Time.Calendar.OrdinalDate"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Date"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Date"            /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Message"


|-- IMPORT  /module "Koshucode.Baala.Data.Type.Head"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Head"            /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Head"            /import "Koshucode.Baala.Data.Type.Type"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Interp"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Interp"          /import "Koshucode.Baala.Data.Token"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge"           /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge"           /import "Koshucode.Baala.Data.Token"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Message"         /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Mono"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Mono"            /import "Koshucode.Baala.Data.Type.Rel"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Para"            /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Para"            /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Para"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Para"            /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Text.Blaze.XHtml5"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Text.Blaze.XHtml5"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Text.Blaze.XHtml5.Attributes"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Head"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Clock"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Date"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type"            /import "Koshucode.Baala.Data.Token"

|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.About"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Arithmetic"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Clock"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Date"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Head"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Interp"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Judge"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Mono"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Para"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Rel"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Time"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Type"

|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Church"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Content"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Token"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Type"

```

Command `./dir-rank.k ../data/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../data/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Data.Token"  /base-rank {= /rank /base /import-dir [ 0 | "Term" | [ ] ] [ 1 | "AngleText" | [ ] ] [ 1 | "Message" | [ ] ] [ 1 | "Token" | [ ] ] [ 2 | "Next" | [ ] ] [ 3 | "Short" | [ ] ] [ 3 | "TokenLine" | [ ] ] [ 4 | "TokenClause" | [ ] ] [ 4 | "TokenTree" | [ ] ] =}
|-- DIR-RANK  /dir-rank 9  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 1 | "Clock" | [ ] ] [ 1 | "Message" | [ ] ] [ 1 | "Para" | [ ] ] [ 2 | "Date" | [ ] ] [ 2 | "Decimal" | [ ] ] [ 3 | "Arithmetic" | [ ] ] [ 3 | "Time" | [ ] ] [ 6 | "Interp" | [ "Token" ] ] [ 6 | "Judge" | [ "Token" ] ] [ 6 | "Type" | [ "Token" ] ] [ 7 | "About" | [ "Token" ] ] [ 7 | "Head" | [ "Token" ] ] [ 8 | "Rel" | [ "Token" ] ] [ 9 | "Mono" | [ ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 6 | "Message" | [ "Token" ] ] [ 11 | "Class" | [ "Token" | "Type" ] ] [ 11 | "Tree" | [ "Token" | "Type" ] ] [ 12 | "BaalaC" | [ "Token" | "Type" ] ] [ 12 | "Decode" | [ "Token" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 15  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 11 | "Message" | [ "Token" | "Type" ] ] [ 12 | "Cox" | [ "Token" ] ] [ 13 | "Cop" | [ "Token" ] ] [ 14 | "Build" | [ "Content" | "Token" ] ] [ 15 | "Run" | [ "Content" | "Token" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 5 | "Token" | [ ] ] [ 10 | "Type" | [ ] ] [ 12 | "Message" | [ ] ] [ 13 | "Content" | [ ] ] [ 16 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 17 | "Data" | [ "Church" | "Content" | "Token" | "Type" ] ] =}

*** 7 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                             /base-rank
  --------- -------------------------------- -------------------------------------------------------------------
  0         "Koshucode.Baala.Base"           /rank /base           /import-dir
                                             ----- --------------- ---------------------------------------------
                                             0     "Message"       [ ]
                                             
  4         "Koshucode.Baala.Data.Token"     /rank /base           /import-dir
                                             ----- --------------- ---------------------------------------------
                                             0     "Term"          [ ]
                                             1     "AngleText"     [ ]
                                             1     "Message"       [ ]
                                             1     "Token"         [ ]
                                             2     "Next"          [ ]
                                             3     "Short"         [ ]
                                             3     "TokenLine"     [ ]
                                             4     "TokenClause"   [ ]
                                             4     "TokenTree"     [ ]
                                             
  9         "Koshucode.Baala.Data.Type"      /rank /base           /import-dir
                                             ----- --------------- ---------------------------------------------
                                             1     "Clock"         [ ]
                                             1     "Message"       [ ]
                                             1     "Para"          [ ]
                                             2     "Date"          [ ]
                                             2     "Decimal"       [ ]
                                             3     "Arithmetic"    [ ]
                                             3     "Time"          [ ]
                                             6     "Interp"        [ "Token" ]
                                             6     "Judge"         [ "Token" ]
                                             6     "Type"          [ "Token" ]
                                             7     "About"         [ "Token" ]
                                             7     "Head"          [ "Token" ]
                                             8     "Rel"           [ "Token" ]
                                             9     "Mono"          [ ]
                                             
  12        "Koshucode.Baala.Data.Content"   /rank /base           /import-dir
                                             ----- --------------- ---------------------------------------------
                                             6     "Message"       [ "Token" ]
                                             11    "Class"         [ "Token" | "Type" ]
                                             11    "Tree"          [ "Token" | "Type" ]
                                             12    "BaalaC"        [ "Token" | "Type" ]
                                             12    "Decode"        [ "Token" | "Type" ]
                                             
  15        "Koshucode.Baala.Data.Church"    /rank /base           /import-dir
                                             ----- --------------- ---------------------------------------------
                                             11    "Message"       [ "Token" | "Type" ]
                                             12    "Cox"           [ "Token" ]
                                             13    "Cop"           [ "Token" ]
                                             14    "Build"         [ "Content" | "Token" ]
                                             15    "Run"           [ "Content" | "Token" | "Type" ]
                                             
  16        "Koshucode.Baala.Data"           /rank /base           /import-dir
                                             ----- --------------- ---------------------------------------------
                                             5     "Token"         [ ]
                                             10    "Type"          [ ]
                                             12    "Message"       [ ]
                                             13    "Content"       [ ]
                                             16    "Church"        [ ]
                                             
  17        "Koshucode.Baala"                /rank /base           /import-dir
                                             ----- --------------- ---------------------------------------------
                                             0     "Base"          [ ]
                                             17    "Data"          [ "Church" | "Content" | "Token" | "Type" ]
                                             

=== rel

**
**  SUMMARY
**       7 judges on DIR-RANK
**       7 judges in total
**
```



## ../core/data/IMPORT.k

```
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    197 judges
**

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Core.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Message"       /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.RelTable"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.Dataset"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.RelTable"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.Run"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attr"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attr"          /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attr"          /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attr"          /import "Koshucode.Baala.Core.Lexmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Core.Lexmap.Slot"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Core.Lexmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrPos"       /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrPos"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrPos"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrPos"       /import "Koshucode.Baala.Core.Lexmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.AttrEd"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Attr"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.LexmapTrees"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Slot"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Core.Lexmap.Attr"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Core.Lexmap.AttrEd"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Core.Lexmap.Attr"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Data.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Message"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Message"       /import "Koshucode.Baala.Data.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Core.Lexmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Attr"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.AttrEd"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.LexmapTrees"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Slot"

|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Assert.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Lexmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Relkit.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Relmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Core.Relkit.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Message"       /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Core.Lexmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Lexmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Run"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Option"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Result"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Data.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Core.Relmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "GHC.IO.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Relmap.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Relmap.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Lexmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Relmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Global"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Option"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Result"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Specialize"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Message"     /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "System.Directory"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "System.FilePath"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Control.Monad.State"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Include"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Assert.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Include"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Read"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Run"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Resource"

|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Resource"

```

Command `./dir-rank.k ../core/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../core/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [ ] ] [ 2 | "AttrPos" | [ ] ] [ 3 | "Attr" | [ ] ] [ 3 | "Slot" | [ ] ] [ 4 | "AttrEd" | [ ] ] [ 4 | "Lexmap" | [ ] ] [ 5 | "LexmapTrees" | [ ] ] [ 6 | "Construct" | [ ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [ ] ] [ 8 | "Relkit" | [ "Lexmap" ] ] [ 9 | "Construct" | [ ] ] [ 10 | "Run" | [ ] ] =}
|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [ ] ] [ 1 | "Result" | [ ] ] [ 2 | "Option" | [ ] ] [ 12 | "Relmap" | [ "Lexmap" | "Relkit" ] ] [ 13 | "Rop" | [ "Lexmap" ] ] [ 13 | "Specialize" | [ "Lexmap" | "Relkit" ] ] [ 14 | "Construct" | [ "Lexmap" | "Relkit" ] ] [ 14 | "Global" | [ ] ] =}

|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [ ] ] [ 1 | "RelTable" | [ ] ] [ 12 | "Dataset" | [ "Relkit" ] ] [ 16 | "Assert" | [ "Lexmap" | "Relmap" ] ] [ 17 | "Run" | [ "Lexmap" | "Relkit" | "Relmap" ] ] =}
|-- DIR-RANK  /dir-rank 21  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [ ] ] [ 8 | "Clause" | [ "Lexmap" ] ] [ 19 | "Resource" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" ] ] [ 20 | "Include" | [ "Assert" | "Lexmap" | "Relmap" ] ] [ 20 | "Run" | [ "Assert" | "Lexmap" | "Relmap" ] ] [ 21 | "Read" | [ "Relmap" ] ] =}
|-- DIR-RANK  /dir-rank 22  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 2 | "Message" | [ ] ] [ 7 | "Lexmap" | [ ] ] [ 11 | "Relkit" | [ ] ] [ 15 | "Relmap" | [ ] ] [ 18 | "Assert" | [ ] ] [ 22 | "Resource" | [ ] ] =}
|-- DIR-RANK  /dir-rank 23  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 | "Data" | [ ] ] [ 23 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] =}

*** 9 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                              /base-rank
  --------- --------------------------------- ----------------------------------------------------------------------------------
  0         "Koshucode.Baala.Base"            /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              0     "Message"       [ ]
                                              
  0         "Koshucode.Baala.Data"            /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              0     "Message"       [ ]
                                              
  6         "Koshucode.Baala.Core.Lexmap"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              2     "AttrPos"       [ ]
                                              3     "Attr"          [ ]
                                              3     "Slot"          [ ]
                                              4     "AttrEd"        [ ]
                                              4     "Lexmap"        [ ]
                                              5     "LexmapTrees"   [ ]
                                              6     "Construct"     [ ]
                                              
  10        "Koshucode.Baala.Core.Relkit"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              8     "Relkit"        [ "Lexmap" ]
                                              9     "Construct"     [ ]
                                              10    "Run"           [ ]
                                              
  14        "Koshucode.Baala.Core.Relmap"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              1     "Result"        [ ]
                                              2     "Option"        [ ]
                                              12    "Relmap"        [ "Lexmap" | "Relkit" ]
                                              13    "Rop"           [ "Lexmap" ]
                                              13    "Specialize"    [ "Lexmap" | "Relkit" ]
                                              14    "Construct"     [ "Lexmap" | "Relkit" ]
                                              14    "Global"        [ ]
                                              
  17        "Koshucode.Baala.Core.Assert"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              1     "RelTable"      [ ]
                                              12    "Dataset"       [ "Relkit" ]
                                              16    "Assert"        [ "Lexmap" | "Relmap" ]
                                              17    "Run"           [ "Lexmap" | "Relkit" | "Relmap" ]
                                              
  21        "Koshucode.Baala.Core.Resource"   /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              8     "Clause"        [ "Lexmap" ]
                                              19    "Resource"      [ "Assert" | "Lexmap" | "Relkit" | "Relmap" ]
                                              20    "Include"       [ "Assert" | "Lexmap" | "Relmap" ]
                                              20    "Run"           [ "Assert" | "Lexmap" | "Relmap" ]
                                              21    "Read"          [ "Relmap" ]
                                              
  22        "Koshucode.Baala.Core"            /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              2     "Message"       [ ]
                                              7     "Lexmap"        [ ]
                                              11    "Relkit"        [ ]
                                              15    "Relmap"        [ ]
                                              18    "Assert"        [ ]
                                              22    "Resource"      [ ]
                                              
  23        "Koshucode.Baala"                 /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              0     "Base"          [ ]
                                              0     "Data"          [ ]
                                              23    "Core"          [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                              

=== rel

**
**  SUMMARY
**       9 judges on DIR-RANK
**       9 judges in total
**
```



## ../writer/data/IMPORT.k

```
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    37 judges
**

|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Text.CSV"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.XHtml5"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.XHtml5"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.XHtml5.Attributes"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.Aeson"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.Aeson"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Writer.Judge"

|-- IMPORT  /module "Koshucode.Baala.Writer"                    /import "Koshucode.Baala.Writer.Csv"
|-- IMPORT  /module "Koshucode.Baala.Writer"                    /import "Koshucode.Baala.Writer.Html"
|-- IMPORT  /module "Koshucode.Baala.Writer"                    /import "Koshucode.Baala.Writer.Json"
|-- IMPORT  /module "Koshucode.Baala.Writer"                    /import "Koshucode.Baala.Writer.Judge"
|-- IMPORT  /module "Koshucode.Baala.Writer"                    /import "Koshucode.Baala.Writer.Koshu"

```

Command `./dir-rank.k ../writer/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../writer/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 1 | "Csv" | [ ] ] [ 1 | "Html" | [ ] ] [ 1 | "Json" | [ ] ] [ 1 | "Judge" | [ ] ] [ 2 | "Koshu" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 | "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 3 | "Writer" | [ ] ] =}

*** 2 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                       /base-rank
  --------- -------------------------- ----------------------------
  2         "Koshucode.Baala.Writer"   /rank /base      /import-dir
                                       ----- ---------- -----------
                                       1     "Csv"      [ ]
                                       1     "Html"     [ ]
                                       1     "Json"     [ ]
                                       1     "Judge"    [ ]
                                       2     "Koshu"    [ ]
                                       
  3         "Koshucode.Baala"          /rank /base      /import-dir
                                       ----- ---------- -----------
                                       0     "Base"     [ ]
                                       0     "Core"     [ ]
                                       0     "Data"     [ ]
                                       3     "Writer"   [ ]
                                       

=== rel

**
**  SUMMARY
**       2 judges on DIR-RANK
**       2 judges in total
**
```



## ../rop-flat/data/IMPORT.k

```
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    128 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Define"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Define"           /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get"              /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get"              /import "Koshucode.Baala.Rop.Base.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get"              /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Message"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Message"          /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Rop"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Rop"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Rop"              /import "Koshucode.Baala.Rop.Base.Define"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Rop"              /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Term"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Term"             /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Term"             /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base"                  /import "Koshucode.Baala.Rop.Base.Define"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base"                  /import "Koshucode.Baala.Rop.Base.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base"                  /import "Koshucode.Baala.Rop.Base.Rop"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base"                  /import "Koshucode.Baala.Rop.Base.Term"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Check"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Control"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Meta"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Order"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Peripheral"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Resource"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Source"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.TermGadget"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Flat.PoScale"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Flat.Term"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Rop.Flat.Lattice.Restrict"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice"          /import "Koshucode.Baala.Rop.Flat.Lattice.Restrict"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice"          /import "Koshucode.Baala.Rop.Flat.Lattice.Rop"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice"          /import "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.PoScale"          /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.PoScale"          /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.PoScale"          /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Koshucode.Baala.Rop.Flat.Term"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Check"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Control"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Meta"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Order"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.PoScale"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Peripheral"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Resource"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Source"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.TermGadget"

```

Command `./dir-rank.k ../rop-flat/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../rop-flat/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 | "Core" | [ ] ] [ 0 | "Data" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 1 | "Define" | [ ] ] [ 1 | "Message" | [ ] ] [ 2 | "Rop" | [ ] ] [ 2 | "Term" | [ ] ] [ 3 | "Get" | [ ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 5 | "Tropashko" | [ "Base" ] ] [ 6 | "Restrict" | [ "Base" ] ] [ 7 | "Rop" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 1 | "PoScale" | [ ] ] [ 2 | "Message" | [ ] ] [ 5 | "Gadget" | [ "Base" ] ] [ 5 | "Meta" | [ "Base" ] ] [ 5 | "Order" | [ "Base" ] ] [ 5 | "Resource" | [ "Base" ] ] [ 5 | "Source" | [ "Base" ] ] [ 5 | "Term" | [ "Base" ] ] [ 6 | "Peripheral" | [ "Base" ] ] [ 6 | "TermGadget" | [ "Base" ] ] [ 8 | "Lattice" | [ ] ] [ 9 | "Check" | [ "Base" | "Lattice" ] ] [ 9 | "Control" | [ "Base" | "Lattice" ] ] [ 10 | "Bundle" | [ "Base" | "Lattice" ] ] =}

|-- DIR-RANK  /dir-rank 11  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 4 | "Base" | [ ] ] [ 11 | "Flat" | [ "Lattice" ] ] =}

*** 6 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                 /base-rank
  --------- ------------------------------------ ---------------------------------------------
  0         "Koshucode.Baala"                    /rank /base          /import-dir
                                                 ----- -------------- ------------------------
                                                 0     "Base"         [ ]
                                                 0     "Core"         [ ]
                                                 0     "Data"         [ ]
                                                 
  0         "Koshucode.Baala.Core"               /rank /base          /import-dir
                                                 ----- -------------- ------------------------
                                                 0     "Message"      [ ]
                                                 
  3         "Koshucode.Baala.Rop.Base"           /rank /base          /import-dir
                                                 ----- -------------- ------------------------
                                                 1     "Define"       [ ]
                                                 1     "Message"      [ ]
                                                 2     "Rop"          [ ]
                                                 2     "Term"         [ ]
                                                 3     "Get"          [ ]
                                                 
  7         "Koshucode.Baala.Rop.Flat.Lattice"   /rank /base          /import-dir
                                                 ----- -------------- ------------------------
                                                 5     "Tropashko"    [ "Base" ]
                                                 6     "Restrict"     [ "Base" ]
                                                 7     "Rop"          [ "Base" ]
                                                 
  10        "Koshucode.Baala.Rop.Flat"           /rank /base          /import-dir
                                                 ----- -------------- ------------------------
                                                 1     "PoScale"      [ ]
                                                 2     "Message"      [ ]
                                                 5     "Gadget"       [ "Base" ]
                                                 5     "Meta"         [ "Base" ]
                                                 5     "Order"        [ "Base" ]
                                                 5     "Resource"     [ "Base" ]
                                                 5     "Source"       [ "Base" ]
                                                 5     "Term"         [ "Base" ]
                                                 6     "Peripheral"   [ "Base" ]
                                                 6     "TermGadget"   [ "Base" ]
                                                 8     "Lattice"      [ ]
                                                 9     "Check"        [ "Base" | "Lattice" ]
                                                 9     "Control"      [ "Base" | "Lattice" ]
                                                 10    "Bundle"       [ "Base" | "Lattice" ]
                                                 
  11        "Koshucode.Baala.Rop"                /rank /base          /import-dir
                                                 ----- -------------- ------------------------
                                                 4     "Base"         [ ]
                                                 11    "Flat"         [ "Lattice" ]
                                                 

=== rel

**
**  SUMMARY
**       6 judges on DIR-RANK
**       6 judges in total
**
```



## ../rop-nested/data/IMPORT.k

```
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    29 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Nest.Flow"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Rop.Flat"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Rop.Nest.Confl"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Rop.Nest.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Message"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Message"          /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Message"          /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rop"              /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rop"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rop"              /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rop"              /import "Koshucode.Baala.Rop.Nest.Confl"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rop"              /import "Koshucode.Baala.Rop.Nest.Deriv"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rop"              /import "Koshucode.Baala.Rop.Nest.Flow"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest"                  /import "Koshucode.Baala.Rop.Nest.Confl"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest"                  /import "Koshucode.Baala.Rop.Nest.Deriv"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest"                  /import "Koshucode.Baala.Rop.Nest.Flow"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest"                  /import "Koshucode.Baala.Rop.Nest.Rop"

```

Command `./dir-rank.k ../rop-nested/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../rop-nested/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 | "Core" | [ ] ] [ 0 | "Data" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [ ] ] [ 2 | "Flow" | [ ] ] [ 3 | "Confl" | [ ] ] [ 4 | "Deriv" | [ ] ] [ 5 | "Rop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 | "Flat" | [ ] ] [ 6 | "Nest" | [ ] ] =}

*** 4 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                         /base-rank
  --------- ---------------------------- -----------------------------
  0         "Koshucode.Baala"            /rank /base       /import-dir
                                         ----- ----------- -----------
                                         0     "Base"      [ ]
                                         0     "Core"      [ ]
                                         0     "Data"      [ ]
                                         
  0         "Koshucode.Baala.Rop.Flat"   /rank /base       /import-dir
                                         ----- ----------- -----------
                                         0     "Message"   [ ]
                                         
  5         "Koshucode.Baala.Rop.Nest"   /rank /base       /import-dir
                                         ----- ----------- -----------
                                         1     "Message"   [ ]
                                         2     "Flow"      [ ]
                                         3     "Confl"     [ ]
                                         4     "Deriv"     [ ]
                                         5     "Rop"       [ ]
                                         
  6         "Koshucode.Baala.Rop"        /rank /base       /import-dir
                                         ----- ----------- -----------
                                         0     "Base"      [ ]
                                         0     "Flat"      [ ]
                                         6     "Nest"      [ ]
                                         

=== rel

**
**  SUMMARY
**       4 judges on DIR-RANK
**       4 judges in total
**
```



## ../rop-cox/data/IMPORT.k

```
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    62 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Accessor"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Accessor"          /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Accessor"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Accessor"          /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Accessor"          /import "Koshucode.Baala.Rop.Cox.Get"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Bundle"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Bundle"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Bundle"            /import "Koshucode.Baala.Rop.Cox.Accessor"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Bundle"            /import "Koshucode.Baala.Rop.Cox.Calc"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Bundle"            /import "Koshucode.Baala.Rop.Cox.Empty"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Bundle"            /import "Koshucode.Baala.Rop.Cox.Filter"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Bundle"            /import "Koshucode.Baala.Rop.Cox.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Bundle"            /import "Koshucode.Baala.Rop.Cox.Range"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Rop.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Rop.Cox.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Rop.Flat"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Rop.Cox.Get"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Rop.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Rop.Cox.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Cox.GeoDatumJp"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Cox.Message"


|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Rop.Cox.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Message"           /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.Rop.Cox.Get"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Accessor"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Calc"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Empty"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Filter"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Range"

```

Command `./dir-rank.k ../rop-cox/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../rop-cox/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 | "Core" | [ ] ] [ 0 | "Data" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [ ] ] [ 1 | "Message" | [ ] ] [ 2 | "Get" | [ ] ] [ 3 | "Accessor" | [ ] ] [ 3 | "Calc" | [ ] ] [ 3 | "Empty" | [ ] ] [ 3 | "Filter" | [ ] ] [ 3 | "Gadget" | [ ] ] [ 3 | "Range" | [ ] ] [ 4 | "Bundle" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 | "Flat" | [ ] ] [ 5 | "Cox" | [ ] ] =}

*** 4 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                         /base-rank
  --------- ---------------------------- --------------------------------
  0         "Koshucode.Baala"            /rank /base          /import-dir
                                         ----- -------------- -----------
                                         0     "Base"         [ ]
                                         0     "Core"         [ ]
                                         0     "Data"         [ ]
                                         
  0         "Koshucode.Baala.Rop.Flat"   /rank /base          /import-dir
                                         ----- -------------- -----------
                                         0     "Message"      [ ]
                                         
  4         "Koshucode.Baala.Rop.Cox"    /rank /base          /import-dir
                                         ----- -------------- -----------
                                         0     "GeoDatumJp"   [ ]
                                         1     "Message"      [ ]
                                         2     "Get"          [ ]
                                         3     "Accessor"     [ ]
                                         3     "Calc"         [ ]
                                         3     "Empty"        [ ]
                                         3     "Filter"       [ ]
                                         3     "Gadget"       [ ]
                                         3     "Range"        [ ]
                                         4     "Bundle"       [ ]
                                         
  5         "Koshucode.Baala.Rop"        /rank /base          /import-dir
                                         ----- -------------- -----------
                                         0     "Base"         [ ]
                                         0     "Flat"         [ ]
                                         5     "Cox"          [ ]
                                         

=== rel

**
**  SUMMARY
**       4 judges on DIR-RANK
**       4 judges in total
**
```



## ../calculator/data/IMPORT.k

```
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    43 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "GHC.IO.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "System.Environment"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Global"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Global"    /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Global"    /import "Koshucode.Baala.Rop.Flat"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Global"    /import "Koshucode.Baala.Rop.Nest"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Global"    /import "Koshucode.Baala.Rop.Cox"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Global"    /import "Koshucode.Baala.Cop"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "System.Environment"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "Koshucode.Baala.Writer"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.SimpleOption"  /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.SimpleOption"  /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.SimpleOption"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.SimpleOption"  /import "System.Environment"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Version"   /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Version"   /import "Paths_koshucode_baala_calculator"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Data.Time"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Toolkit.Library.Run"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Toolkit.Library.Exit"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Data.Time"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Writer"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Element"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Run"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.SimpleOption"

```

Command `./dir-rank.k ../calculator/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../calculator/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 | "Cop" | [ ] ] [ 0 | "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Writer" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Cox" | [ ] ] [ 0 | "Flat" | [ ] ] [ 0 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" | [ ] ] [ 1 | "Element" | [ ] ] [ 1 | "Exit" | [ ] ] [ 1 | "Global" | [ ] ] [ 1 | "Run" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 2 | "KoshuFilter" | [ ] ] [ 2 | "KoshuMain" | [ ] ] =}

*** 4 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                /base-rank
  --------- ----------------------------------- ----------------------------------
  0         "Koshucode.Baala"                   /rank /base            /import-dir
                                                ----- ---------------- -----------
                                                0     "Base"           [ ]
                                                0     "Cop"            [ ]
                                                0     "Core"           [ ]
                                                0     "Data"           [ ]
                                                0     "Writer"         [ ]
                                                
  0         "Koshucode.Baala.Rop"               /rank /base            /import-dir
                                                ----- ---------------- -----------
                                                0     "Cox"            [ ]
                                                0     "Flat"           [ ]
                                                0     "Nest"           [ ]
                                                
  1         "Koshucode.Baala.Toolkit.Library"   /rank /base            /import-dir
                                                ----- ---------------- -----------
                                                0     "SimpleOption"   [ ]
                                                1     "Element"        [ ]
                                                1     "Exit"           [ ]
                                                1     "Global"         [ ]
                                                1     "Run"            [ ]
                                                
  2         "Koshucode.Baala.Toolkit.Main"      /rank /base            /import-dir
                                                ----- ---------------- -----------
                                                2     "KoshuFilter"    [ ]
                                                2     "KoshuMain"      [ ]
                                                

=== rel

**
**  SUMMARY
**       4 judges on DIR-RANK
**       4 judges in total
**
```



## ../toolkit/data/IMPORT.k

```
** -*- koshu -*-
**
**  DESCRIPTION
**    List of module imports.
**
**  IMPORT
**    <<< Module named /module imports module /import. >>>
**
**  SUMMARY
**    33 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Writer"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Toolkit.Library.Input"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Data.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Input"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Change"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Version"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.Version"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Toolkit.Library.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Toolkit.Library.Version"

```

Command `./dir-rank.k ../toolkit/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../toolkit/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 | "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Writer" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 | "Exit" | [ ] ] [ 0 | "Version" | [ ] ] [ 1 | "Input" | [ ] ] [ 1 | "RDF" | [ ] ] [ 2 | "Change" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 1 | "KoshuSyntax" | [ ] ] [ 2 | "KoshuRdf" | [ ] ] [ 3 | "KoshuChange" | [ ] ] =}

*** 3 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                /base-rank
  --------- ----------------------------------- ---------------------------------
  0         "Koshucode.Baala"                   /rank /base           /import-dir
                                                ----- --------------- -----------
                                                0     "Base"          [ ]
                                                0     "Core"          [ ]
                                                0     "Data"          [ ]
                                                0     "Writer"        [ ]
                                                
  2         "Koshucode.Baala.Toolkit.Library"   /rank /base           /import-dir
                                                ----- --------------- -----------
                                                0     "Exit"          [ ]
                                                0     "Version"       [ ]
                                                1     "Input"         [ ]
                                                1     "RDF"           [ ]
                                                2     "Change"        [ ]
                                                
  3         "Koshucode.Baala.Toolkit.Main"      /rank /base           /import-dir
                                                ----- --------------- -----------
                                                1     "KoshuSyntax"   [ ]
                                                2     "KoshuRdf"      [ ]
                                                3     "KoshuChange"   [ ]
                                                

=== rel

**
**  SUMMARY
**       3 judges on DIR-RANK
**       3 judges in total
**
```



## command

This document is produced by the command:

```
koshu-inout.sh -o DIR-RANK.md -f PATH ./dir-rank.k
```

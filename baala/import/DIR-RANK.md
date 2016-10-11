# I/O List

- [./dir-rank.k](#dir-rankk)
- ./dir-rank.k [../base/data/IMPORT.k](#basedataimportk)
- ./dir-rank.k [../syntax/data/IMPORT.k](#syntaxdataimportk)
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
**    163 judges
**

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.Abort.Reason"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Message"        /import "Koshucode.Baala.Base.Abort"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Reason"         /import "Koshucode.Baala.Base.IO"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Abort.Reason"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Abortable"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Reason"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Report"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.BzFile"            /import "Control.Exception"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.BzFile"            /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.BzFile"            /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.CodePt"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.CodePt"            /import "Koshucode.Baala.Base.IO.IOPoint"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.Exit"              /import "GHC.IO.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Exit"              /import "System.Environment"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Exit"              /import "System.Exit"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Exit"              /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Exit"              /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Data.ByteString.Char8"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Control.Exception"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Network.HTTP.Conduit"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Network.HTTP.Types.Status"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Network.URI"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Koshucode.Baala.Base.Text.Utility"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.IOPoint"           /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.IOPoint"           /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.IOPoint"           /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.SimpleOption"      /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.SimpleOption"      /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.SimpleOption"      /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.SimpleOption"      /import "System.Environment"

|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.BzFile"
|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.CodePt"
|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.Exit"
|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.Http"
|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.IOPoint"
|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.SimpleOption"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Assoc"           /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Assoc"           /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Assoc"           /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Dots"            /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Dots"            /import "Koshucode.Baala.Base.List.Split"

|-- IMPORT  /module "Koshucode.Baala.Base.List.List"            /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Order"           /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Order"           /import "Koshucode.Baala.Base.List.Snip"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Set"             /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Set"             /import "Data.Set"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Snip"            /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Snip"            /import "Koshucode.Baala.Base.Prelude"


|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Assoc"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Dots"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.List"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Order"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Set"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Snip"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Split"

|-- IMPORT  /module "Koshucode.Baala.Base.Message"              /import "Koshucode.Baala.Base.Abort.Message"
|-- IMPORT  /module "Koshucode.Baala.Base.Message"              /import "Koshucode.Baala.Base.Syntax.Message"



|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.ByteString"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.ByteString.Lazy.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Default"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Tuple"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "System.Exit"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Text.PrettyPrint"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Pair"         /import "Koshucode.Baala.Base.Prelude.Class"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Queue"        /import "Koshucode.Baala.Base.Prelude.Import"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Case"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Class"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Import"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Pair"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Queue"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Clause"        /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Clause"        /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Clause"        /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Clause"        /import "Koshucode.Baala.Base.Syntax.Line"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Koshucode.Baala.Base.Syntax.Tree"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Data.ByteString.Lazy.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Message"       /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Message"       /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Message"       /import "Koshucode.Baala.Base.IO"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Scan"          /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Scan"          /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Scan"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Scan"          /import "Koshucode.Baala.Base.Syntax.Line"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Syntax.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Clause"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Infix"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Line"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Scan"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Tree"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Comment"         /import "System.IO"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.LineBreak"       /import "Data.Default"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixClass"        /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixClass"        /import "Data.ByteString"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixClass"        /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixClass"        /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixClass"        /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixClass"        /import "Koshucode.Baala.Base.Text.MixText"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixEncode"       /import "Koshucode.Baala.Base.Text.MixText"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Control.Exception"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Numeric"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.ByteString"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.ByteString.Builder"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.ByteString.Lazy.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.Default"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.Text.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Data.Text.Lazy.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Koshucode.Baala.Base.Text.Utility"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.MixText"         /import "Koshucode.Baala.Base.Text.LineBreak"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.PPrint"          /import "Text.PrettyPrint"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.PPrint"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.PPrint"          /import "Koshucode.Baala.Base.List"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Unicode"         /import "Data.Char"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Comment"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.LineBreak"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.MixClass"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.MixEncode"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.MixText"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.PPrint"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.TextTable"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Unicode"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Utility"

|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.List"
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

|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Case" | [ ]
    ] [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Pair" | [ ] ] [ 1 | "Queue" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Set" | [ ] ] [
    0 | "Split" | [ ] ] [ 1 | "Dots" | [ ] ] [ 3 | "Assoc" | [ "Prelude" ] ] [ 3 | "List" | [ "Prelude" ] ] [ 3 | "Snip"
    | [ "Prelude" ] ] [ 4 | "Order" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ]
    ] [ 0 | "LineBreak" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 3 | "Utility" | [ "Prelude" ] ] [ 4 | "MixText" | [ ] ] [ 5 |
    "MixClass" | [ ] ] [ 5 | "MixEncode" | [ ] ] [ 6 | "PPrint" | [ "List" | "Prelude" ] ] [ 6 | "TextTable" | [ "List"
    | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" |
    [ ] ] [ 3 | "BzFile" | [ "Prelude" ] ] [ 3 | "Exit" | [ "Prelude" ] ] [ 4 | "Http" | [ "Prelude" ] ] [ 6 | "IOPoint"
    | [ "List" | "Prelude" ] ] [ 7 | "CodePt" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 9 | "Reason" | [
    "IO" ] ] [ 10 | "Abortable" | [ "IO" | "Prelude" ] ] [ 10 | "Report" | [ "IO" | "List" | "Prelude" | "Text" ] ] [ 12
    | "Message" | [ "Abort" ] ] =}

|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Base.Syntax"  /base-rank {= /rank /base /import-dir [ 9 | "Line" | [
    "IO" | "Prelude" ] ] [ 10 | "Clause" | [ "IO" | "List" | "Prelude" ] ] [ 10 | "Scan" | [ "IO" | "Prelude" ] ] [ 12 |
    "Message" | [ "Abort" | "IO" | "Prelude" ] ] [ 13 | "Tree" | [ "Abort" | "IO" | "List" | "Prelude" ] ] [ 14 |
    "Infix" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 15  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 2 | "Prelude" | [ ] ] [
    5 | "List" | [ ] ] [ 7 | "Text" | [ ] ] [ 8 | "IO" | [ ] ] [ 11 | "Abort" | [ ] ] [ 13 | "Message" | [ ] ] [ 15 |
    "Syntax" | [ ] ] =}
|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 16 | "Base" | [ "Abort" |
    "IO" | "List" | "Prelude" | "Syntax" | "Text" ] ] =}

*** 8 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                             /base-rank
  --------- -------------------------------- ------------------------------------------------------------------------------------
  1         "Koshucode.Baala.Base.Prelude"   /rank /base            /import-dir
                                             ----- ---------------- -------------------------------------------------------------
                                             0     "Case"           [ ]
                                             0     "Class"          [ ]
                                             0     "Import"         [ ]
                                             1     "Pair"           [ ]
                                             1     "Queue"          [ ]
                                             
  4         "Koshucode.Baala.Base.List"      /rank /base            /import-dir
                                             ----- ---------------- -------------------------------------------------------------
                                             0     "Set"            [ ]
                                             0     "Split"          [ ]
                                             1     "Dots"           [ ]
                                             3     "Assoc"          [ "Prelude" ]
                                             3     "List"           [ "Prelude" ]
                                             3     "Snip"           [ "Prelude" ]
                                             4     "Order"          [ "Prelude" ]
                                             
  6         "Koshucode.Baala.Base.Text"      /rank /base            /import-dir
                                             ----- ---------------- -------------------------------------------------------------
                                             0     "Comment"        [ ]
                                             0     "LineBreak"      [ ]
                                             0     "Unicode"        [ ]
                                             3     "Utility"        [ "Prelude" ]
                                             4     "MixText"        [ ]
                                             5     "MixClass"       [ ]
                                             5     "MixEncode"      [ ]
                                             6     "PPrint"         [ "List" | "Prelude" ]
                                             6     "TextTable"      [ "List" | "Prelude" ]
                                             
  7         "Koshucode.Baala.Base.IO"        /rank /base            /import-dir
                                             ----- ---------------- -------------------------------------------------------------
                                             0     "SimpleOption"   [ ]
                                             3     "BzFile"         [ "Prelude" ]
                                             3     "Exit"           [ "Prelude" ]
                                             4     "Http"           [ "Prelude" ]
                                             6     "IOPoint"        [ "List" | "Prelude" ]
                                             7     "CodePt"         [ "Prelude" ]
                                             
  12        "Koshucode.Baala.Base.Abort"     /rank /base            /import-dir
                                             ----- ---------------- -------------------------------------------------------------
                                             9     "Reason"         [ "IO" ]
                                             10    "Abortable"      [ "IO" | "Prelude" ]
                                             10    "Report"         [ "IO" | "List" | "Prelude" | "Text" ]
                                             12    "Message"        [ "Abort" ]
                                             
  14        "Koshucode.Baala.Base.Syntax"    /rank /base            /import-dir
                                             ----- ---------------- -------------------------------------------------------------
                                             9     "Line"           [ "IO" | "Prelude" ]
                                             10    "Clause"         [ "IO" | "List" | "Prelude" ]
                                             10    "Scan"           [ "IO" | "Prelude" ]
                                             12    "Message"        [ "Abort" | "IO" | "Prelude" ]
                                             13    "Tree"           [ "Abort" | "IO" | "List" | "Prelude" ]
                                             14    "Infix"          [ "Prelude" ]
                                             
  15        "Koshucode.Baala.Base"           /rank /base            /import-dir
                                             ----- ---------------- -------------------------------------------------------------
                                             2     "Prelude"        [ ]
                                             5     "List"           [ ]
                                             7     "Text"           [ ]
                                             8     "IO"             [ ]
                                             11    "Abort"          [ ]
                                             13    "Message"        [ ]
                                             15    "Syntax"         [ ]
                                             
  16        "Koshucode.Baala"                /rank /base            /import-dir
                                             ----- ---------------- -------------------------------------------------------------
                                             16    "Base"           [ "Abort" | "IO" | "List" | "Prelude" | "Syntax" | "Text" ]
                                             

=== rel

**
**  SUMMARY
**       8 judges on DIR-RANK
**       8 judges in total
**
```



## ../syntax/data/IMPORT.k

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
**    121 judges
**

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.TTree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Attr.AttrName"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Attr.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.TTree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Attr.AttrName"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Attr.Slot"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Attr.Message"


|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Syntax.TTree"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Parse"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Parse"         /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Parse"         /import "Koshucode.Baala.Syntax.Attr.Attr"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Parse"         /import "Koshucode.Baala.Syntax.Attr.AttrName"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.TTree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.Attr.AttrName"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.Attr.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr"               /import "Koshucode.Baala.Syntax.Attr.Attr"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr"               /import "Koshucode.Baala.Syntax.Attr.AttrEd"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr"               /import "Koshucode.Baala.Syntax.Attr.AttrName"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr"               /import "Koshucode.Baala.Syntax.Attr.Parse"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr"               /import "Koshucode.Baala.Syntax.Attr.Slot"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Message"            /import "Koshucode.Baala.Syntax.Attr.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Message"            /import "Koshucode.Baala.Syntax.Symbol.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Message"            /import "Koshucode.Baala.Syntax.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Get"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Get"           /import "Koshucode.Baala.Syntax.Para.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Get"           /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Para"          /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Para"          /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.ParaSpec"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.ParaSpec"      /import "Koshucode.Baala.Syntax.Para.Para"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.Get"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.ParaSpec"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.AngleText"   /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.AngleText"   /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Message"     /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Syntax.Symbol.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Syntax.Symbol.Next"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Term"        /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Next"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Short"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Term"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Nipper"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Rel"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Section"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Utility"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Message"      /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Symbol.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Pattern"      /import "Koshucode.Baala.Syntax.Token.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Token.Nipper"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Token.Section"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Nipper"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Utility"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Token"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Token"        /import "Koshucode.Baala.Syntax.Symbol"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Utility"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Utility"      /import "Koshucode.Baala.Syntax.Token.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Clause"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Utility"

|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Bracket"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Bracket"      /import "Koshucode.Baala.Syntax.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Parse"        /import "Text.PrettyPrint"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Parse"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Parse"        /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Parse"        /import "Koshucode.Baala.Syntax.TTree.Bracket"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Parse"        /import "Koshucode.Baala.Syntax.TTree.TokenTree"

|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Pattern"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Pattern"      /import "Koshucode.Baala.Syntax.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.TokenTree"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.TokenTree"    /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.TokenTree"    /import "Koshucode.Baala.Syntax.TTree.Bracket"

|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree"              /import "Koshucode.Baala.Syntax.TTree.Bracket"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree"              /import "Koshucode.Baala.Syntax.TTree.Parse"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree"              /import "Koshucode.Baala.Syntax.TTree.TokenTree"

|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Attr"
|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.TTree"
|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Token"

```

Command `./dir-rank.k ../syntax/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../syntax/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 1 | "Para" | [ ]
    ] [ 2 | "Get" | [ ] ] [ 2 | "ParaSpec" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 1 | "AngleText"
    | [ ] ] [ 1 | "Message" | [ ] ] [ 1 | "Term" | [ ] ] [ 2 | "Next" | [ ] ] [ 3 | "Short" | [ ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 1 | "Message" |
    [ ] ] [ 5 | "Token" | [ "Symbol" ] ] [ 6 | "Pattern" | [ ] ] [ 6 | "Utility" | [ ] ] [ 7 | "Nipper" | [ "Symbol" ] ]
    [ 8 | "Section" | [ "Symbol" ] ] [ 9 | "Rel" | [ "Symbol" ] ] [ 10 | "Clause" | [ "Symbol" ] ] =}
|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 12 | "Bracket"
    | [ "Token" ] ] [ 12 | "Pattern" | [ "Token" ] ] [ 13 | "TokenTree" | [ "Token" ] ] [ 14 | "Parse" | [ "Token" ] ]
    =}

|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 16 | "Message" | [ "Para" | "Symbol" | "TTree" ] ] [ 17 | "Attr" | [ "Para" | "TTree" ] ] [ 17 | "Slot" | [
    "TTree" | "Token" ] ] [ 18 | "AttrEd" | [ "TTree" | "Token" ] ] [ 18 | "Parse" | [ "Para" ] ] =}
|-- DIR-RANK  /dir-rank 19  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 3 | "Para" | [ ] ] [
    4 | "Symbol" | [ ] ] [ 11 | "Token" | [ ] ] [ 15 | "TTree" | [ ] ] [ 17 | "Message" | [ ] ] [ 19 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 20  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 20 |
    "Syntax" | [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ] ] =}

*** 8 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                              /base-rank
  --------- --------------------------------- ------------------------------------------------------------------------
  0         "Koshucode.Baala.Base"            /rank /base         /import-dir
                                              ----- ------------- ----------------------------------------------------
                                              0     "Message"     [ ]
                                              
  2         "Koshucode.Baala.Syntax.Para"     /rank /base         /import-dir
                                              ----- ------------- ----------------------------------------------------
                                              1     "Para"        [ ]
                                              2     "Get"         [ ]
                                              2     "ParaSpec"    [ ]
                                              
  3         "Koshucode.Baala.Syntax.Symbol"   /rank /base         /import-dir
                                              ----- ------------- ----------------------------------------------------
                                              1     "AngleText"   [ ]
                                              1     "Message"     [ ]
                                              1     "Term"        [ ]
                                              2     "Next"        [ ]
                                              3     "Short"       [ ]
                                              
  10        "Koshucode.Baala.Syntax.Token"    /rank /base         /import-dir
                                              ----- ------------- ----------------------------------------------------
                                              1     "Message"     [ ]
                                              5     "Token"       [ "Symbol" ]
                                              6     "Pattern"     [ ]
                                              6     "Utility"     [ ]
                                              7     "Nipper"      [ "Symbol" ]
                                              8     "Section"     [ "Symbol" ]
                                              9     "Rel"         [ "Symbol" ]
                                              10    "Clause"      [ "Symbol" ]
                                              
  14        "Koshucode.Baala.Syntax.TTree"    /rank /base         /import-dir
                                              ----- ------------- ----------------------------------------------------
                                              12    "Bracket"     [ "Token" ]
                                              12    "Pattern"     [ "Token" ]
                                              13    "TokenTree"   [ "Token" ]
                                              14    "Parse"       [ "Token" ]
                                              
  18        "Koshucode.Baala.Syntax.Attr"     /rank /base         /import-dir
                                              ----- ------------- ----------------------------------------------------
                                              0     "AttrName"    [ ]
                                              16    "Message"     [ "Para" | "Symbol" | "TTree" ]
                                              17    "Attr"        [ "Para" | "TTree" ]
                                              17    "Slot"        [ "TTree" | "Token" ]
                                              18    "AttrEd"      [ "TTree" | "Token" ]
                                              18    "Parse"       [ "Para" ]
                                              
  19        "Koshucode.Baala.Syntax"          /rank /base         /import-dir
                                              ----- ------------- ----------------------------------------------------
                                              3     "Para"        [ ]
                                              4     "Symbol"      [ ]
                                              11    "Token"       [ ]
                                              15    "TTree"       [ ]
                                              17    "Message"     [ ]
                                              19    "Attr"        [ ]
                                              
  20        "Koshucode.Baala"                 /rank /base         /import-dir
                                              ----- ------------- ----------------------------------------------------
                                              0     "Base"        [ ]
                                              20    "Syntax"      [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                              

=== rel

**
**  SUMMARY
**       8 judges on DIR-RANK
**       8 judges in total
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
**    163 judges
**

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Content"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Data.Church.Cox"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Data.Church.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Syntax.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Syntax"
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
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Content.Complex"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Content.Simple"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Content.Singleton"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Content.Utility"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Content.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Complex"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Complex"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Complex"      /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Complex"      /import "Koshucode.Baala.Data.Content.Singleton"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Singleton"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Complex"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Simple"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Tree"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Utility"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Syntax.TTree.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Message"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Message"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Message"      /import "Koshucode.Baala.Syntax.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Simple"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Simple"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Simple"       /import "Koshucode.Baala.Data.Content.Singleton"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Singleton"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Singleton"    /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Singleton"    /import "Koshucode.Baala.Data.Content.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Data.Content.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Data.Content.Singleton"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Data.Content.Complex"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Data.Content.Simple"

|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.BaalaC"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Complex"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Decode"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Simple"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Singleton"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Tree"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Utility"

|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Syntax.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Church.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Content.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Data.Type.Decimal.Coder"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Data.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Data.Type.Decimal.Fraction"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Data.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Data.Type.Decimal.Fraction"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Data.Type.Decimal.Rational"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Decimal"  /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Decimal"  /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Fraction"  /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Fraction"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Fraction"  /import "Koshucode.Baala.Data.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Fraction"  /import "Koshucode.Baala.Data.Type.Decimal.Rational"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Rational"  /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Rational"  /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.BinaryAb"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.Coder"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.Fraction"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.Rational"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Interp"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Interp"          /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Interp"          /import "Koshucode.Baala.Data.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge"           /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Message"         /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Data.Type.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Data.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Mono"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Mono"        /import "Koshucode.Baala.Data.Type.Rel.Rel"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Rel"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Rel"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Rel"         /import "Koshucode.Baala.Data.Type.Rel.Head"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Rel"         /import "Koshucode.Baala.Data.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.ShareSide"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.ShareSide"   /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.ShareSide"   /import "Koshucode.Baala.Data.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Rel.Head"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Rel.Mono"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Rel.Rel"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Rel.ShareSide"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Clock"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Clock"      /import "Koshucode.Baala.Data.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Clock"      /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Data.Time.Calendar.OrdinalDate"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Data.Type.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Data.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Time.Time"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type"            /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type"            /import "Koshucode.Baala.Data.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Interp"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Judge"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Rel"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Time"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Type"

|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Church"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Content"
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
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ]
    =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 0 | "Pattern" |
    [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Data.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 1 |
    "Decimal" | [ ] ] [ 1 | "Rational" | [ ] ] [ 2 | "Fraction" | [ ] ] [ 3 | "Coder" | [ ] ] [ 4 | "BinaryAb" | [ ] ]
    =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Data.Type.Time"  /base-rank {= /rank /base /import-dir [ 2 | "Date" | [
    ] ] [ 3 | "Clock" | [ ] ] [ 4 | "Time" | [ ] ] =}

|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 2 | "ShareSide"
    | [ ] ] [ 3 | "Head" | [ ] ] [ 4 | "Rel" | [ ] ] [ 5 | "Mono" | [ ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 1 | "Judge" | [ ] ]
    [ 1 | "Message" | [ ] ] [ 2 | "Interp" | [ ] ] [ 2 | "Type" | [ ] ] [ 5 | "Decimal" | [ ] ] [ 5 | "Time" | [ ] ] [ 6
    | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 11  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 1 | "Message" |
    [ ] ] [ 8 | "Singleton" | [ "Type" ] ] [ 8 | "Tree" | [ "Type" ] ] [ 9 | "Complex" | [ "Type" ] ] [ 9 | "Simple" | [
    "Type" ] ] [ 10 | "Utility" | [ "Type" ] ] [ 11 | "BaalaC" | [ "Type" ] ] [ 11 | "Decode" | [ "Type" ] ] =}
|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 8 | "Message" |
    [ "Type" ] ] [ 9 | "Cox" | [ ] ] [ 10 | "Cop" | [ ] ] [ 13 | "Build" | [ "Content" ] ] [ 14 | "Run" | [ "Content" |
    "Type" ] ] =}
|-- DIR-RANK  /dir-rank 15  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 7 | "Type" | [
    "Decimal" | "Rel" | "Time" ] ] [ 9 | "Message" | [ ] ] [ 12 | "Content" | [ ] ] [ 15 | "Church" | [ ] ] =}

|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Syntax" | [ ] ] [ 16 | "Data" | [ "Church" | "Content" | "Type" ] ] =}

*** 11 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                  /base-rank
  --------- ------------------------------------- -------------------------------------------------------
  0         "Koshucode.Baala.Base"                /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  0     "Message"     [ ]
                                                  
  0         "Koshucode.Baala.Syntax"              /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  0     "Message"     [ ]
                                                  
  0         "Koshucode.Baala.Syntax.TTree"        /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  0     "Pattern"     [ ]
                                                  
  4         "Koshucode.Baala.Data.Type.Decimal"   /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  1     "Decimal"     [ ]
                                                  1     "Rational"    [ ]
                                                  2     "Fraction"    [ ]
                                                  3     "Coder"       [ ]
                                                  4     "BinaryAb"    [ ]
                                                  
  4         "Koshucode.Baala.Data.Type.Time"      /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  2     "Date"        [ ]
                                                  3     "Clock"       [ ]
                                                  4     "Time"        [ ]
                                                  
  5         "Koshucode.Baala.Data.Type.Rel"       /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  2     "ShareSide"   [ ]
                                                  3     "Head"        [ ]
                                                  4     "Rel"         [ ]
                                                  5     "Mono"        [ ]
                                                  
  6         "Koshucode.Baala.Data.Type"           /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  1     "Judge"       [ ]
                                                  1     "Message"     [ ]
                                                  2     "Interp"      [ ]
                                                  2     "Type"        [ ]
                                                  5     "Decimal"     [ ]
                                                  5     "Time"        [ ]
                                                  6     "Rel"         [ ]
                                                  
  11        "Koshucode.Baala.Data.Content"        /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  1     "Message"     [ ]
                                                  8     "Singleton"   [ "Type" ]
                                                  8     "Tree"        [ "Type" ]
                                                  9     "Complex"     [ "Type" ]
                                                  9     "Simple"      [ "Type" ]
                                                  10    "Utility"     [ "Type" ]
                                                  11    "BaalaC"      [ "Type" ]
                                                  11    "Decode"      [ "Type" ]
                                                  
  14        "Koshucode.Baala.Data.Church"         /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  8     "Message"     [ "Type" ]
                                                  9     "Cox"         [ ]
                                                  10    "Cop"         [ ]
                                                  13    "Build"       [ "Content" ]
                                                  14    "Run"         [ "Content" | "Type" ]
                                                  
  15        "Koshucode.Baala.Data"                /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  7     "Type"        [ "Decimal" | "Rel" | "Time" ]
                                                  9     "Message"     [ ]
                                                  12    "Content"     [ ]
                                                  15    "Church"      [ ]
                                                  
  16        "Koshucode.Baala"                     /rank /base         /import-dir
                                                  ----- ------------- -----------------------------------
                                                  0     "Base"        [ ]
                                                  0     "Syntax"      [ ]
                                                  16    "Data"        [ "Church" | "Content" | "Type" ]
                                                  

=== rel

**
**  SUMMARY
**      11 judges on DIR-RANK
**      11 judges in total
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
**    191 judges
**

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Core.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Message"       /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Syntax"
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

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.LexmapTrees"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Data.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Message"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Message"       /import "Koshucode.Baala.Data.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.LexmapTrees"

|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Assert.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Lexmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Relkit.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Relmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Core.Relkit.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Message"       /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Core.Lexmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Lexmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Run"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Syntax"
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
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Data.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Core.Relmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Relmap.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Syntax"
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

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Core.Resource.Resource"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Relmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Message"     /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Control.Monad.State"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "System.Directory"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "System.FilePath"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Include"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Assert.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Concrete"
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
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 1 | "Lexmap" | [
    ] ] [ 1 | "LexmapTrees" | [ ] ] [ 1 | "Message" | [ ] ] [ 2 | "Construct" | [ ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [
    ] ] [ 4 | "Relkit" | [ "Lexmap" ] ] [ 5 | "Construct" | [ ] ] [ 6 | "Run" | [ ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 1 | "Message" |
    [ ] ] [ 1 | "Result" | [ ] ] [ 2 | "Option" | [ ] ] [ 8 | "Relmap" | [ "Lexmap" | "Relkit" ] ] [ 9 | "Rop" | [
    "Lexmap" ] ] [ 9 | "Specialize" | [ "Lexmap" | "Relkit" ] ] [ 10 | "Construct" | [ "Lexmap" | "Relkit" ] ] [ 10 |
    "Global" | [ ] ] =}

|-- DIR-RANK  /dir-rank 13  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 1 | "Message" |
    [ ] ] [ 1 | "RelTable" | [ ] ] [ 8 | "Dataset" | [ "Relkit" ] ] [ 12 | "Assert" | [ "Lexmap" | "Relmap" ] ] [ 13 |
    "Run" | [ "Lexmap" | "Relkit" | "Relmap" ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 1 | "Message"
    | [ ] ] [ 4 | "Clause" | [ "Lexmap" ] ] [ 15 | "Resource" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" ] ] [ 16 |
    "Concrete" | [ "Relmap" ] ] [ 16 | "Include" | [ "Assert" | "Lexmap" | "Relmap" ] ] [ 16 | "Run" | [ "Assert" |
    "Lexmap" | "Relmap" ] ] [ 17 | "Read" | [ "Relmap" ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 2 | "Message" | [ ] ] [
    3 | "Lexmap" | [ ] ] [ 7 | "Relkit" | [ ] ] [ 11 | "Relmap" | [ ] ] [ 14 | "Assert" | [ ] ] [ 18 | "Resource" | [ ]
    ] =}
|-- DIR-RANK  /dir-rank 19  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Data" | [ ] ] [ 0 | "Syntax" | [ ] ] [ 19 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ]
    =}

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
                                              
  2         "Koshucode.Baala.Core.Lexmap"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Lexmap"        [ ]
                                              1     "LexmapTrees"   [ ]
                                              1     "Message"       [ ]
                                              2     "Construct"     [ ]
                                              
  6         "Koshucode.Baala.Core.Relkit"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              4     "Relkit"        [ "Lexmap" ]
                                              5     "Construct"     [ ]
                                              6     "Run"           [ ]
                                              
  10        "Koshucode.Baala.Core.Relmap"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              1     "Result"        [ ]
                                              2     "Option"        [ ]
                                              8     "Relmap"        [ "Lexmap" | "Relkit" ]
                                              9     "Rop"           [ "Lexmap" ]
                                              9     "Specialize"    [ "Lexmap" | "Relkit" ]
                                              10    "Construct"     [ "Lexmap" | "Relkit" ]
                                              10    "Global"        [ ]
                                              
  13        "Koshucode.Baala.Core.Assert"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              1     "RelTable"      [ ]
                                              8     "Dataset"       [ "Relkit" ]
                                              12    "Assert"        [ "Lexmap" | "Relmap" ]
                                              13    "Run"           [ "Lexmap" | "Relkit" | "Relmap" ]
                                              
  17        "Koshucode.Baala.Core.Resource"   /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              4     "Clause"        [ "Lexmap" ]
                                              15    "Resource"      [ "Assert" | "Lexmap" | "Relkit" | "Relmap" ]
                                              16    "Concrete"      [ "Relmap" ]
                                              16    "Include"       [ "Assert" | "Lexmap" | "Relmap" ]
                                              16    "Run"           [ "Assert" | "Lexmap" | "Relmap" ]
                                              17    "Read"          [ "Relmap" ]
                                              
  18        "Koshucode.Baala.Core"            /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              2     "Message"       [ ]
                                              3     "Lexmap"        [ ]
                                              7     "Relkit"        [ ]
                                              11    "Relmap"        [ ]
                                              14    "Assert"        [ ]
                                              18    "Resource"      [ ]
                                              
  19        "Koshucode.Baala"                 /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              0     "Base"          [ ]
                                              0     "Data"          [ ]
                                              0     "Syntax"        [ ]
                                              19    "Core"          [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                              

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
**    40 judges
**

|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.XHtml5"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.XHtml5"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.XHtml5.Attributes"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.Html.Renderer.Pretty"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.Html.Renderer.String"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.Aeson"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.Aeson"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Syntax"
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

|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 1 | "Csv" | [ ] ] [ 1
    | "Html" | [ ] ] [ 1 | "Json" | [ ] ] [ 1 | "Judge" | [ ] ] [ 2 | "Koshu" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Syntax" | [ ] ] [ 3 | "Writer" | [ ] ] =}

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
                                       0     "Syntax"   [ ]
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
**    159 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Define"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Define"           /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get"              /import "Koshucode.Baala.Syntax"
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Term"             /import "Koshucode.Baala.Syntax"
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Bundle"           /import "Koshucode.Baala.Rop.Flat.Elem"
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Syntax"
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

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Flat.PoScale"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Flat.Subtext"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Rop.Flat.Lattice.Restrict"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice"          /import "Koshucode.Baala.Rop.Flat.Lattice.Restrict"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice"          /import "Koshucode.Baala.Rop.Flat.Lattice.Rop"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice"          /import "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Peripheral"       /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.PoScale"          /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.PoScale"          /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.PoScale"          /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Subtext"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Subtext"          /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Subtext"          /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Subtext"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Subtext"          /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Subtext"          /import "Koshucode.Baala.Subtext"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Subtext"          /import "Koshucode.Baala.Rop.Flat.Message"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Subtext"          /import "Koshucode.Baala.Syntax.TTree.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Syntax"
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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Subtext" | [ ] ] [ 0 | "Syntax" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 0 | "Pattern" |
    [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 1 | "Define" | [ ] ]
    [ 1 | "Message" | [ ] ] [ 2 | "Rop" | [ ] ] [ 2 | "Term" | [ ] ] [ 3 | "Get" | [ ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 5 |
    "Tropashko" | [ "Base" ] ] [ 6 | "Restrict" | [ "Base" ] ] [ 7 | "Rop" | [ "Base" ] ] =}

|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 1 | "PoScale" | [ ]
    ] [ 2 | "Message" | [ ] ] [ 5 | "Elem" | [ "Base" ] ] [ 5 | "Meta" | [ "Base" ] ] [ 5 | "Order" | [ "Base" ] ] [ 5 |
    "Resource" | [ "Base" ] ] [ 5 | "Source" | [ "Base" ] ] [ 5 | "Subtext" | [ "Base" ] ] [ 5 | "Term" | [ "Base" ] ] [
    6 | "Gadget" | [ "Base" ] ] [ 6 | "Peripheral" | [ "Base" ] ] [ 6 | "TermGadget" | [ "Base" ] ] [ 8 | "Lattice" | [
    ] ] [ 9 | "Check" | [ "Base" | "Lattice" ] ] [ 9 | "Control" | [ "Base" | "Lattice" ] ] [ 10 | "Bundle" | [ "Base" |
    "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 11  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 4 | "Base" | [ ] ] [ 11
    | "Flat" | [ "Lattice" ] ] =}

*** 7 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                 /base-rank
  --------- ------------------------------------ ---------------------------------------------
  0         "Koshucode.Baala"                    /rank /base          /import-dir
                                                 ----- -------------- ------------------------
                                                 0     "Base"         [ ]
                                                 0     "Core"         [ ]
                                                 0     "Data"         [ ]
                                                 0     "Subtext"      [ ]
                                                 0     "Syntax"       [ ]
                                                 
  0         "Koshucode.Baala.Core"               /rank /base          /import-dir
                                                 ----- -------------- ------------------------
                                                 0     "Message"      [ ]
                                                 
  0         "Koshucode.Baala.Syntax.TTree"       /rank /base          /import-dir
                                                 ----- -------------- ------------------------
                                                 0     "Pattern"      [ ]
                                                 
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
                                                 5     "Elem"         [ "Base" ]
                                                 5     "Meta"         [ "Base" ]
                                                 5     "Order"        [ "Base" ]
                                                 5     "Resource"     [ "Base" ]
                                                 5     "Source"       [ "Base" ]
                                                 5     "Subtext"      [ "Base" ]
                                                 5     "Term"         [ "Base" ]
                                                 6     "Gadget"       [ "Base" ]
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
**       7 judges on DIR-RANK
**       7 judges in total
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
**    35 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Flat"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Nest.Flow"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Rop.Flat"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Rop.Nest.Confl"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Rop.Nest.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Message"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Message"          /import "Koshucode.Baala.Syntax"
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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Syntax" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ]
    ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [ ]
    ] [ 2 | "Flow" | [ ] ] [ 3 | "Confl" | [ ] ] [ 4 | "Deriv" | [ ] ] [ 5 | "Rop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Flat" | [ ] ] [ 6 | "Nest" | [ ] ] =}

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
                                         0     "Syntax"    [ ]
                                         
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
**    67 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Accessor"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Accessor"          /import "Koshucode.Baala.Syntax"
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Syntax"
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Rop.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Rop.Cox.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Cox.GeoDatumJp"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Cox.Message"


|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Get"               /import "Koshucode.Baala.Rop.Cox.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Message"           /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.Syntax"
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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Syntax" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ]
    ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 1 | "Message" | [ ] ] [ 2 | "Get" | [ ] ] [ 3 | "Accessor" | [ ] ] [ 3 | "Calc" | [ ] ] [ 3 | "Empty" | [ ] ]
    [ 3 | "Filter" | [ ] ] [ 3 | "Gadget" | [ ] ] [ 3 | "Range" | [ ] ] [ 4 | "Bundle" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Flat" | [ ] ] [ 5 | "Cox" | [ ] ] =}

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
                                         0     "Syntax"       [ ]
                                         
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
**    33 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Global"    /import "Koshucode.Baala.Base"
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

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Version"   /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Version"   /import "Paths_koshucode_baala_calculator"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Data.Time"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Toolkit.Library.Run"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Data.Time"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Writer"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Base.IO.SimpleOption"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Element"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Run"

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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Cop" | [ ] ] [ 0 | "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Syntax" | [ ] ] [ 0 | "Writer" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" |
    [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Cox" | [ ] ] [ 0 |
    "Flat" | [ ] ] [ 0 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 1 | "Element"
    | [ ] ] [ 1 | "Global" | [ ] ] [ 1 | "Run" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 2 |
    "KoshuFilter" | [ ] ] [ 2 | "KoshuMain" | [ ] ] =}

*** 5 judges

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
                                                0     "Syntax"         [ ]
                                                0     "Writer"         [ ]
                                                
  0         "Koshucode.Baala.Base.IO"           /rank /base            /import-dir
                                                ----- ---------------- -----------
                                                0     "SimpleOption"   [ ]
                                                
  0         "Koshucode.Baala.Rop"               /rank /base            /import-dir
                                                ----- ---------------- -----------
                                                0     "Cox"            [ ]
                                                0     "Flat"           [ ]
                                                0     "Nest"           [ ]
                                                
  1         "Koshucode.Baala.Toolkit.Library"   /rank /base            /import-dir
                                                ----- ---------------- -----------
                                                1     "Element"        [ ]
                                                1     "Global"         [ ]
                                                1     "Run"            [ ]
                                                
  2         "Koshucode.Baala.Toolkit.Main"      /rank /base            /import-dir
                                                ----- ---------------- -----------
                                                2     "KoshuFilter"    [ ]
                                                2     "KoshuMain"      [ ]
                                                

=== rel

**
**  SUMMARY
**       5 judges on DIR-RANK
**       5 judges in total
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

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Data.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Input"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Change"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Version"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.Version"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Core"
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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Syntax" | [ ] ] [ 0 | "Writer" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 | "Version"
    | [ ] ] [ 1 | "Input" | [ ] ] [ 1 | "RDF" | [ ] ] [ 2 | "Change" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 1 |
    "KoshuSyntax" | [ ] ] [ 2 | "KoshuRdf" | [ ] ] [ 3 | "KoshuChange" | [ ] ] =}

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
                                                0     "Syntax"        [ ]
                                                0     "Writer"        [ ]
                                                
  2         "Koshucode.Baala.Toolkit.Library"   /rank /base           /import-dir
                                                ----- --------------- -----------
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

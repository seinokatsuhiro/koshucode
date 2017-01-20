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
**    182 judges
**

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.CodePos"        /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.CodePos"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.CodePos"        /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Message"        /import "Koshucode.Baala.Base.Abort"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Reason"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Reason"         /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Reason"         /import "Koshucode.Baala.Base.Abort.CodePos"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Abort.CodePos"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Abort.Reason"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.CodePos"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Reason"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Report"

|-- IMPORT  /module "Koshucode.Baala.Base.Code.Bracket"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Bracket"         /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Bracket"         /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Bracket"         /import "Koshucode.Baala.Base.Code.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Code.Clause"          /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Clause"          /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Clause"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Clause"          /import "Koshucode.Baala.Base.Code.Line"

|-- IMPORT  /module "Koshucode.Baala.Base.Code.Infix"           /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Infix"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Infix"           /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Infix"           /import "Koshucode.Baala.Base.Code.Tree"

|-- IMPORT  /module "Koshucode.Baala.Base.Code.Line"            /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Line"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Line"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Line"            /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Line"            /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Code.Message"         /import "Koshucode.Baala.Base.Abort"

|-- IMPORT  /module "Koshucode.Baala.Base.Code.Scan"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Scan"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Scan"            /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Scan"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Scan"            /import "Koshucode.Baala.Base.Code.Line"

|-- IMPORT  /module "Koshucode.Baala.Base.Code.Tree"            /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Tree"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Tree"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Tree"            /import "Koshucode.Baala.Base.Code.Bracket"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Tree"            /import "Koshucode.Baala.Base.Code.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Code"                 /import "Koshucode.Baala.Base.Code.Bracket"
|-- IMPORT  /module "Koshucode.Baala.Base.Code"                 /import "Koshucode.Baala.Base.Code.Clause"
|-- IMPORT  /module "Koshucode.Baala.Base.Code"                 /import "Koshucode.Baala.Base.Code.Infix"
|-- IMPORT  /module "Koshucode.Baala.Base.Code"                 /import "Koshucode.Baala.Base.Code.Line"
|-- IMPORT  /module "Koshucode.Baala.Base.Code"                 /import "Koshucode.Baala.Base.Code.Scan"
|-- IMPORT  /module "Koshucode.Baala.Base.Code"                 /import "Koshucode.Baala.Base.Code.Tree"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.BzFile"            /import "Control.Exception"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.BzFile"            /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.BzFile"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.BzFile"            /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.Encoding"          /import "GHC.IO.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Encoding"          /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Encoding"          /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Encoding"          /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Control.Exception"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Network.HTTP.Conduit"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Network.HTTP.Types.Status"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Network.URI"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.IO.IOPoint"           /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.IOPoint"           /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.IOPoint"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.IOPoint"           /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.IOPoint"           /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.BzFile"
|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.Http"
|-- IMPORT  /module "Koshucode.Baala.Base.IO"                   /import "Koshucode.Baala.Base.IO.IOPoint"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Assoc"           /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Assoc"           /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Assoc"           /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.List.List"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.List.List"            /import "Koshucode.Baala.Base.List.Split"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Order"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Order"           /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Order"           /import "Koshucode.Baala.Base.List.Select"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Picker"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Picker"          /import "Koshucode.Baala.Base.List.Select"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Picker"          /import "Koshucode.Baala.Base.List.Set"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Select"          /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Select"          /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Select"          /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Set"             /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Set"             /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Set"             /import "Koshucode.Baala.Overture"


|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Assoc"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.List"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Order"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Picker"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Select"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Set"
|-- IMPORT  /module "Koshucode.Baala.Base.List"                 /import "Koshucode.Baala.Base.List.Split"

|-- IMPORT  /module "Koshucode.Baala.Base.Message"              /import "Koshucode.Baala.Base.Abort.Message"
|-- IMPORT  /module "Koshucode.Baala.Base.Message"              /import "Koshucode.Baala.Base.Code.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.MixText.Deriv"        /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.Deriv"        /import "Koshucode.Baala.Base.MixText.MixText"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.Deriv"        /import "Koshucode.Baala.Base.MixText.MixClass"

|-- IMPORT  /module "Koshucode.Baala.Base.MixText.LineBreak"    /import "Data.Default"

|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Data.ByteString"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Koshucode.Baala.Base.MixText.MixText"

|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixEncode"    /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixEncode"    /import "Koshucode.Baala.Base.MixText.MixText"

|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Control.Exception"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Numeric"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.ByteString"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.ByteString.Builder"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.ByteString.Lazy.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.Default"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.Text.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Data.Text.Lazy.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixText"      /import "Koshucode.Baala.Base.MixText.LineBreak"

|-- IMPORT  /module "Koshucode.Baala.Base.MixText"              /import "Koshucode.Baala.Base.MixText.LineBreak"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText"              /import "Koshucode.Baala.Base.MixText.Deriv"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText"              /import "Koshucode.Baala.Base.MixText.MixClass"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText"              /import "Koshucode.Baala.Base.MixText.MixText"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText"              /import "Koshucode.Baala.Base.MixText.MixEncode"



|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Control.Exception"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.ByteString"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.ByteString.Lazy.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.ByteString.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Default"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.String"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Tuple"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "System.Exit"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Text.PrettyPrint"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Pair"         /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Queue"        /import "Koshucode.Baala.Base.Prelude.Import"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Case"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Class"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Import"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Pair"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude"              /import "Koshucode.Baala.Base.Prelude.Queue"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Comment"         /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Comment"         /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Dots"            /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Dots"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Dots"            /import "Koshucode.Baala.Base.List"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.PPrint"          /import "Text.PrettyPrint"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.PPrint"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.PPrint"          /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Suffix"          /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Suffix"          /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Comment"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Dots"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.PPrint"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.TextTable"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Suffix"

|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Code"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.MixText"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Prelude"
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
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Split" | [ ] ]
    [ 1 | "Assoc" | [ ] ] [ 1 | "List" | [ ] ] [ 1 | "Select" | [ ] ] [ 1 | "Set" | [ ] ] [ 2 | "Picker" | [ ] ] [ 3 |
    "Order" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Base.MixText"  /base-rank {= /rank /base /import-dir [ 0 | "LineBreak"
    | [ ] ] [ 1 | "MixText" | [ ] ] [ 2 | "MixClass" | [ ] ] [ 2 | "MixEncode" | [ ] ] [ 3 | "Deriv" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 1 | "Comment" | [ ]
    ] [ 1 | "Suffix" | [ ] ] [ 1 | "TextTable" | [ ] ] [ 3 | "PPrint" | [ "Prelude" ] ] [ 5 | "Dots" | [ "List" ] ] =}
|-- DIR-RANK  /dir-rank 9  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 3 | "CodePos" | [
    "Prelude" ] ] [ 5 | "Reason" | [ "List" ] ] [ 7 | "Report" | [ "List" | "Text" ] ] [ 9 | "Message" | [ "Abort" ] ]
    =}

|-- DIR-RANK  /dir-rank 9  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 3 | "Encoding" | [
    "Prelude" ] ] [ 3 | "Http" | [ "Prelude" ] ] [ 5 | "IOPoint" | [ "List" | "Prelude" ] ] [ 9 | "BzFile" | [ "Abort" |
    "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Base.Code"  /base-rank {= /rank /base /import-dir [ 9 | "Message" | [
    "Abort" ] ] [ 10 | "Bracket" | [ "Abort" | "List" ] ] [ 11 | "Line" | [ "Abort" | "IO" | "Prelude" ] ] [ 11 | "Tree"
    | [ "Abort" ] ] [ 12 | "Clause" | [ "Abort" | "List" | "Prelude" ] ] [ 12 | "Infix" | [ "Prelude" ] ] [ 12 | "Scan"
    | [ "Abort" | "IO" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 13  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 2 | "Prelude" | [ ] ] [
    4 | "List" | [ ] ] [ 4 | "MixText" | [ ] ] [ 6 | "Text" | [ ] ] [ 8 | "Abort" | [ ] ] [ 10 | "IO" | [ ] ] [ 10 |
    "Message" | [ ] ] [ 13 | "Code" | [ ] ] =}
|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Overture" | [ ] ] [ 0 |
    "System" | [ ] ] [ 14 | "Base" | [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ] ] =}

*** 9 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                             /base-rank
  --------- -------------------------------- -------------------------------------------------------------------------------------------
  1         "Koshucode.Baala.Base.Prelude"   /rank /base         /import-dir
                                             ----- ------------- -----------------------------------------------------------------------
                                             0     "Case"        [ ]
                                             0     "Class"       [ ]
                                             0     "Import"      [ ]
                                             1     "Pair"        [ ]
                                             1     "Queue"       [ ]
                                             
  3         "Koshucode.Baala.Base.List"      /rank /base         /import-dir
                                             ----- ------------- -----------------------------------------------------------------------
                                             0     "Split"       [ ]
                                             1     "Assoc"       [ ]
                                             1     "List"        [ ]
                                             1     "Select"      [ ]
                                             1     "Set"         [ ]
                                             2     "Picker"      [ ]
                                             3     "Order"       [ "Prelude" ]
                                             
  3         "Koshucode.Baala.Base.MixText"   /rank /base         /import-dir
                                             ----- ------------- -----------------------------------------------------------------------
                                             0     "LineBreak"   [ ]
                                             1     "MixText"     [ ]
                                             2     "MixClass"    [ ]
                                             2     "MixEncode"   [ ]
                                             3     "Deriv"       [ ]
                                             
  5         "Koshucode.Baala.Base.Text"      /rank /base         /import-dir
                                             ----- ------------- -----------------------------------------------------------------------
                                             1     "Comment"     [ ]
                                             1     "Suffix"      [ ]
                                             1     "TextTable"   [ ]
                                             3     "PPrint"      [ "Prelude" ]
                                             5     "Dots"        [ "List" ]
                                             
  9         "Koshucode.Baala.Base.Abort"     /rank /base         /import-dir
                                             ----- ------------- -----------------------------------------------------------------------
                                             3     "CodePos"     [ "Prelude" ]
                                             5     "Reason"      [ "List" ]
                                             7     "Report"      [ "List" | "Text" ]
                                             9     "Message"     [ "Abort" ]
                                             
  9         "Koshucode.Baala.Base.IO"        /rank /base         /import-dir
                                             ----- ------------- -----------------------------------------------------------------------
                                             3     "Encoding"    [ "Prelude" ]
                                             3     "Http"        [ "Prelude" ]
                                             5     "IOPoint"     [ "List" | "Prelude" ]
                                             9     "BzFile"      [ "Abort" | "Prelude" ]
                                             
  12        "Koshucode.Baala.Base.Code"      /rank /base         /import-dir
                                             ----- ------------- -----------------------------------------------------------------------
                                             9     "Message"     [ "Abort" ]
                                             10    "Bracket"     [ "Abort" | "List" ]
                                             11    "Line"        [ "Abort" | "IO" | "Prelude" ]
                                             11    "Tree"        [ "Abort" ]
                                             12    "Clause"      [ "Abort" | "List" | "Prelude" ]
                                             12    "Infix"       [ "Prelude" ]
                                             12    "Scan"        [ "Abort" | "IO" | "Prelude" ]
                                             
  13        "Koshucode.Baala.Base"           /rank /base         /import-dir
                                             ----- ------------- -----------------------------------------------------------------------
                                             2     "Prelude"     [ ]
                                             4     "List"        [ ]
                                             4     "MixText"     [ ]
                                             6     "Text"        [ ]
                                             8     "Abort"       [ ]
                                             10    "IO"          [ ]
                                             10    "Message"     [ ]
                                             13    "Code"        [ ]
                                             
  14        "Koshucode.Baala"                /rank /base         /import-dir
                                             ----- ------------- -----------------------------------------------------------------------
                                             0     "Overture"    [ ]
                                             0     "System"      [ ]
                                             14    "Base"        [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ]
                                             

=== rel

**
**  SUMMARY
**       9 judges on DIR-RANK
**       9 judges in total
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
**    178 judges
**

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Tree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Attr.AttrName"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Attr.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Tree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Attr.AttrName"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Attr.Slot"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrEd"        /import "Koshucode.Baala.Syntax.Attr.Message"


|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Syntax.Symbol"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.Tree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.Attr.AttrName"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.Attr.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr"               /import "Koshucode.Baala.Syntax.Attr.Attr"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr"               /import "Koshucode.Baala.Syntax.Attr.AttrEd"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr"               /import "Koshucode.Baala.Syntax.Attr.AttrName"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr"               /import "Koshucode.Baala.Syntax.Attr.Slot"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Message"            /import "Koshucode.Baala.Syntax.Attr.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Message"            /import "Koshucode.Baala.Syntax.Symbol.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Message"            /import "Koshucode.Baala.Syntax.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Get"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Get"           /import "Koshucode.Baala.Syntax.Para.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Get"           /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Para"          /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Para"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Para"          /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.ParaSpec"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.ParaSpec"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.ParaSpec"      /import "Koshucode.Baala.Syntax.Para.Para"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Parse"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Parse"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Parse"         /import "Koshucode.Baala.Syntax.Para.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Parse"         /import "Koshucode.Baala.Syntax.Para.ParaSpec"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.Get"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.ParaSpec"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.Parse"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Pattern"            /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Pattern"            /import "Koshucode.Baala.Syntax.Tree.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Subtext"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Syntax.Tree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Syntax.Subtree.Subtree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Syntax.Tree.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Decode"     /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.DirTree"    /import "System.Directory"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.DirTree"    /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.DirTree"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.DirTree"    /import "Koshucode.Baala.Syntax.Subtree.Subtree"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Subtree"    /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Subtree"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Subtree"    /import "Koshucode.Baala.Subtext"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Subtree"    /import "Koshucode.Baala.Syntax.Symbol"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Xml"        /import "Text.HTML.TagSoup"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Xml"        /import "Text.StringLike"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Xml"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Xml"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Xml"        /import "Koshucode.Baala.Subtext"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Xml"        /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree.Xml"        /import "Koshucode.Baala.Syntax.Subtree.Subtree"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree"            /import "Koshucode.Baala.Syntax.Subtree.Decode"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree"            /import "Koshucode.Baala.Syntax.Subtree.DirTree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree"            /import "Koshucode.Baala.Syntax.Subtree.Subtree"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Subtree"            /import "Koshucode.Baala.Syntax.Subtree.Xml"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.AngleText"   /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.AngleText"   /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Message"     /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Syntax.Symbol.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Syntax.Symbol.Symbol"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Symbol"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Symbol"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Symbol"      /import "Koshucode.Baala.Syntax.Symbol.Next"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Symbol"      /import "Koshucode.Baala.Syntax.Symbol.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Term"        /import "Data.String"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Term"        /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Term"        /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Term"        /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Next"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Short"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Term"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Clip"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Rel"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Section"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Utility"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clip"         /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clip"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clip"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clip"         /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clip"         /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clip"         /import "Koshucode.Baala.Syntax.Symbol.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clip"         /import "Koshucode.Baala.Syntax.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Message"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Message"      /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Pattern"      /import "Koshucode.Baala.Syntax.Token.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Token.Clip"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Token.Section"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Syntax.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Clip"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Utility"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Section"      /import "Koshucode.Baala.Syntax.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Token"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Token"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Token"        /import "Koshucode.Baala.Syntax.Symbol"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Utility"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Utility"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Utility"      /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Utility"      /import "Koshucode.Baala.Syntax.Token.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Clause"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Utility"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Bracket"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Bracket"       /import "Koshucode.Baala.Syntax.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Parse"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Parse"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Parse"         /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Parse"         /import "Koshucode.Baala.Syntax.Tree.Bracket"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Pattern"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Pattern"       /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Pattern"       /import "Koshucode.Baala.Syntax.Tree.Bracket"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Pattern"       /import "Koshucode.Baala.Syntax.Token.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Split"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Split"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Split"         /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Split"         /import "Koshucode.Baala.Syntax.Tree.Parse"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Split"         /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Split"         /import "Koshucode.Baala.Syntax.Tree.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree"               /import "Koshucode.Baala.Syntax.Tree.Bracket"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree"               /import "Koshucode.Baala.Syntax.Tree.Parse"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree"               /import "Koshucode.Baala.Syntax.Tree.Split"

|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Attr"
|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Subtree"
|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax"                    /import "Koshucode.Baala.Syntax.Tree"

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
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 1 | "Para" | [ ]
    ] [ 2 | "Get" | [ ] ] [ 2 | "ParaSpec" | [ ] ] [ 3 | "Parse" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 1 | "AngleText"
    | [ ] ] [ 1 | "Message" | [ ] ] [ 1 | "Term" | [ ] ] [ 2 | "Next" | [ ] ] [ 3 | "Symbol" | [ ] ] [ 4 | "Short" | [ ]
    ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 1 | "Message" |
    [ ] ] [ 6 | "Token" | [ "Symbol" ] ] [ 7 | "Clip" | [ "Symbol" ] ] [ 7 | "Pattern" | [ ] ] [ 7 | "Utility" | [
    "Symbol" ] ] [ 8 | "Section" | [ ] ] [ 9 | "Rel" | [ "Symbol" ] ] [ 10 | "Clause" | [ "Symbol" ] ] =}
|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Syntax.Tree"  /base-rank {= /rank /base /import-dir [ 12 | "Bracket" |
    [ "Token" ] ] [ 13 | "Parse" | [ "Token" ] ] [ 13 | "Pattern" | [ "Token" ] ] [ 14 | "Split" | [ "Token" ] ] =}

|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala.Syntax.Subtree"  /base-rank {= /rank /base /import-dir [ 6 | "Subtree"
    | [ "Symbol" ] ] [ 7 | "DirTree" | [ ] ] [ 7 | "Xml" | [ "Symbol" ] ] [ 16 | "Decode" | [ "Symbol" | "Token" |
    "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 6 | "Message" | [ "Para" | "Symbol" ] ] [ 16 | "Attr" | [ "Para" | "Tree" ] ] [ 16 | "Slot" | [ "Token" |
    "Tree" ] ] [ 17 | "AttrEd" | [ "Symbol" | "Token" | "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 4 | "Para" | [ ] ] [
    5 | "Symbol" | [ ] ] [ 7 | "Message" | [ ] ] [ 11 | "Token" | [ ] ] [ 14 | "Pattern" | [ ] ] [ 15 | "Tree" | [ ] ] [
    17 | "Subtree" | [ ] ] [ 18 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 19  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Overture" | [ ] ] [ 0 | "Subtext" | [ ] ] [ 19 | "Syntax" | [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" |
    "Tree" ] ] =}

*** 9 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                               /base-rank
  --------- ---------------------------------- -----------------------------------------------------------------------------------
  0         "Koshucode.Baala.Base"             /rank /base         /import-dir
                                               ----- ------------- ---------------------------------------------------------------
                                               0     "Message"     [ ]
                                               
  3         "Koshucode.Baala.Syntax.Para"      /rank /base         /import-dir
                                               ----- ------------- ---------------------------------------------------------------
                                               1     "Para"        [ ]
                                               2     "Get"         [ ]
                                               2     "ParaSpec"    [ ]
                                               3     "Parse"       [ ]
                                               
  4         "Koshucode.Baala.Syntax.Symbol"    /rank /base         /import-dir
                                               ----- ------------- ---------------------------------------------------------------
                                               1     "AngleText"   [ ]
                                               1     "Message"     [ ]
                                               1     "Term"        [ ]
                                               2     "Next"        [ ]
                                               3     "Symbol"      [ ]
                                               4     "Short"       [ ]
                                               
  10        "Koshucode.Baala.Syntax.Token"     /rank /base         /import-dir
                                               ----- ------------- ---------------------------------------------------------------
                                               1     "Message"     [ ]
                                               6     "Token"       [ "Symbol" ]
                                               7     "Clip"        [ "Symbol" ]
                                               7     "Pattern"     [ ]
                                               7     "Utility"     [ "Symbol" ]
                                               8     "Section"     [ ]
                                               9     "Rel"         [ "Symbol" ]
                                               10    "Clause"      [ "Symbol" ]
                                               
  14        "Koshucode.Baala.Syntax.Tree"      /rank /base         /import-dir
                                               ----- ------------- ---------------------------------------------------------------
                                               12    "Bracket"     [ "Token" ]
                                               13    "Parse"       [ "Token" ]
                                               13    "Pattern"     [ "Token" ]
                                               14    "Split"       [ "Token" ]
                                               
  16        "Koshucode.Baala.Syntax.Subtree"   /rank /base         /import-dir
                                               ----- ------------- ---------------------------------------------------------------
                                               6     "Subtree"     [ "Symbol" ]
                                               7     "DirTree"     [ ]
                                               7     "Xml"         [ "Symbol" ]
                                               16    "Decode"      [ "Symbol" | "Token" | "Tree" ]
                                               
  17        "Koshucode.Baala.Syntax.Attr"      /rank /base         /import-dir
                                               ----- ------------- ---------------------------------------------------------------
                                               0     "AttrName"    [ ]
                                               6     "Message"     [ "Para" | "Symbol" ]
                                               16    "Attr"        [ "Para" | "Tree" ]
                                               16    "Slot"        [ "Token" | "Tree" ]
                                               17    "AttrEd"      [ "Symbol" | "Token" | "Tree" ]
                                               
  18        "Koshucode.Baala.Syntax"           /rank /base         /import-dir
                                               ----- ------------- ---------------------------------------------------------------
                                               4     "Para"        [ ]
                                               5     "Symbol"      [ ]
                                               7     "Message"     [ ]
                                               11    "Token"       [ ]
                                               14    "Pattern"     [ ]
                                               15    "Tree"        [ ]
                                               17    "Subtree"     [ ]
                                               18    "Attr"        [ ]
                                               
  19        "Koshucode.Baala"                  /rank /base         /import-dir
                                               ----- ------------- ---------------------------------------------------------------
                                               0     "Base"        [ ]
                                               0     "Overture"    [ ]
                                               0     "Subtext"     [ ]
                                               19    "Syntax"      [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" | "Tree" ]
                                               

=== rel

**
**  SUMMARY
**       9 judges on DIR-RANK
**       9 judges in total
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
**    142 judges
**

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Decode"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cop"           /import "Koshucode.Baala.Data.Church.Cox"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Cox"           /import "Koshucode.Baala.Data.Church.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Type"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Decode"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Church.Build"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Class.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Church.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church"               /import "Koshucode.Baala.Data.Church.Build"
|-- IMPORT  /module "Koshucode.Baala.Data.Church"               /import "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT  /module "Koshucode.Baala.Data.Church"               /import "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT  /module "Koshucode.Baala.Data.Church"               /import "Koshucode.Baala.Data.Church.Run"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Data.Class.Edge"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Data.Class.Simple"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Data.Class.Complex"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Data.Class.Edge"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Data.Class.Simple"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Edge"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Edge"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Edge"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Edge"           /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Edge"           /import "Koshucode.Baala.Data.Class.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Encode"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Encode"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Encode"         /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Encode"         /import "Koshucode.Baala.Data.Class.Complex"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Encode"         /import "Koshucode.Baala.Data.Class.Content"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Encode"         /import "Koshucode.Baala.Data.Class.Edge"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Encode"         /import "Koshucode.Baala.Data.Class.Simple"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Map"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Map"            /import "Koshucode.Baala.Data.Class.Simple"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Map"            /import "Koshucode.Baala.Data.Class.Complex"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Message"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Message"        /import "Koshucode.Baala.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Data.Class.Edge"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Data.Class.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Complex"
|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Content"
|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Edge"
|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Encode"
|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Map"
|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Simple"

|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Decode"

|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Data.Decode.Numeric"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Data.Decode.Term"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Data.Decode.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Content"       /import "Koshucode.Baala.Data.Decode.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Dataset"       /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Dataset"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Dataset"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Dataset"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Dataset"       /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Dataset"       /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Dataset"       /import "Koshucode.Baala.Data.Decode.Content"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Dataset"       /import "Koshucode.Baala.Data.Decode.Term"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Dataset"       /import "Koshucode.Baala.Syntax.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Message"       /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Numeric"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Numeric"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Numeric"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Numeric"       /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Numeric"       /import "Koshucode.Baala.Data.Decode.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Numeric"       /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Numeric"       /import "Koshucode.Baala.Data.Decode.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Term"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Term"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Term"          /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Term"          /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Term"          /import "Koshucode.Baala.Data.Decode.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Type"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Type"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Type"          /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Type"          /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Type"          /import "Koshucode.Baala.Data.Decode.Term"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Type"          /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Type"          /import "Koshucode.Baala.Data.Decode.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Decode"               /import "Koshucode.Baala.Data.Decode.Content"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode"               /import "Koshucode.Baala.Data.Decode.Dataset"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode"               /import "Koshucode.Baala.Data.Decode.Numeric"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode"               /import "Koshucode.Baala.Data.Decode.Term"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode"               /import "Koshucode.Baala.Data.Decode.Type"

|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Syntax.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Type.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Church.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Class.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Decode.Message"

|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Church"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Content"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Decode"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Type"

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
    [ 0 | "Pattern" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Type"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Data.Class"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [
    ] ] [ 2 | "Edge" | [ ] ] [ 3 | "Simple" | [ ] ] [ 4 | "Complex" | [ ] ] [ 5 | "Content" | [ ] ] [ 5 | "Map" | [ ] ]
    [ 6 | "Encode" | [ ] ] =}
|-- DIR-RANK  /dir-rank 9  /dir "Koshucode.Baala.Data.Decode"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [
    ] ] [ 2 | "Term" | [ ] ] [ 3 | "Type" | [ ] ] [ 4 | "Numeric" | [ ] ] [ 8 | "Content" | [ "Class" ] ] [ 9 |
    "Dataset" | [ "Class" ] ] =}

|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 1 | "Message" |
    [ ] ] [ 2 | "Cox" | [ ] ] [ 3 | "Cop" | [ ] ] [ 11 | "Build" | [ "Class" | "Decode" ] ] [ 12 | "Run" | [ "Class" |
    "Decode" ] ] =}
|-- DIR-RANK  /dir-rank 13  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 2 | "Message" | [ ] ] [
    7 | "Class" | [ ] ] [ 10 | "Decode" | [ ] ] [ 11 | "Content" | [ "Class" | "Decode" ] ] [ 13 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Overture" | [ ] ] [ 0 | "Syntax" | [ ] ] [ 0 | "Type" | [ ] ] [ 14 | "Data" | [ "Church" | "Class" | "Decode" ] ]
    =}

*** 8 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                            /base-rank
  --------- ------------------------------- ------------------------------------------------------
  0         "Koshucode.Baala.Base"          /rank /base        /import-dir
                                            ----- ------------ -----------------------------------
                                            0     "Message"    [ ]
                                            
  0         "Koshucode.Baala.Syntax"        /rank /base        /import-dir
                                            ----- ------------ -----------------------------------
                                            0     "Message"    [ ]
                                            0     "Pattern"    [ ]
                                            
  0         "Koshucode.Baala.Type"          /rank /base        /import-dir
                                            ----- ------------ -----------------------------------
                                            0     "Message"    [ ]
                                            
  6         "Koshucode.Baala.Data.Class"    /rank /base        /import-dir
                                            ----- ------------ -----------------------------------
                                            1     "Message"    [ ]
                                            2     "Edge"       [ ]
                                            3     "Simple"     [ ]
                                            4     "Complex"    [ ]
                                            5     "Content"    [ ]
                                            5     "Map"        [ ]
                                            6     "Encode"     [ ]
                                            
  9         "Koshucode.Baala.Data.Decode"   /rank /base        /import-dir
                                            ----- ------------ -----------------------------------
                                            1     "Message"    [ ]
                                            2     "Term"       [ ]
                                            3     "Type"       [ ]
                                            4     "Numeric"    [ ]
                                            8     "Content"    [ "Class" ]
                                            9     "Dataset"    [ "Class" ]
                                            
  12        "Koshucode.Baala.Data.Church"   /rank /base        /import-dir
                                            ----- ------------ -----------------------------------
                                            1     "Message"    [ ]
                                            2     "Cox"        [ ]
                                            3     "Cop"        [ ]
                                            11    "Build"      [ "Class" | "Decode" ]
                                            12    "Run"        [ "Class" | "Decode" ]
                                            
  13        "Koshucode.Baala.Data"          /rank /base        /import-dir
                                            ----- ------------ -----------------------------------
                                            2     "Message"    [ ]
                                            7     "Class"      [ ]
                                            10    "Decode"     [ ]
                                            11    "Content"    [ "Class" | "Decode" ]
                                            13    "Church"     [ ]
                                            
  14        "Koshucode.Baala"               /rank /base        /import-dir
                                            ----- ------------ -----------------------------------
                                            0     "Base"       [ ]
                                            0     "Overture"   [ ]
                                            0     "Syntax"     [ ]
                                            0     "Type"       [ ]
                                            14    "Data"       [ "Church" | "Class" | "Decode" ]
                                            

=== rel

**
**  SUMMARY
**       8 judges on DIR-RANK
**       8 judges in total
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
**    213 judges
**

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Message"       /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.RelTable"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.RelTable"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.Run"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.LexmapTrees"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Data.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Message"       /import "Koshucode.Baala.Base"

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

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Core.Relkit.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Message"       /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Core.Lexmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Lexmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Run"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Option"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Result"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Type"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Core.Relmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Type"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Relmap.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Type"
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
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Concrete"    /import "Koshucode.Baala.Core.Resource.Resource"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Relmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Message"     /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Control.Monad.State"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "System.Directory"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "System.FilePath"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Include"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Type"
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
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 0 | "Pattern" | [ ] ]
    =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 1 | "Lexmap" | [
    ] ] [ 1 | "LexmapTrees" | [ ] ] [ 1 | "Message" | [ ] ] [ 2 | "Construct" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [
    ] ] [ 4 | "Relkit" | [ "Lexmap" ] ] [ 5 | "Construct" | [ ] ] [ 5 | "Run" | [ ] ] =}

|-- DIR-RANK  /dir-rank 9  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 1 | "Message" | [
    ] ] [ 1 | "Result" | [ ] ] [ 2 | "Option" | [ ] ] [ 7 | "Relmap" | [ "Lexmap" | "Relkit" ] ] [ 8 | "Rop" | [
    "Lexmap" ] ] [ 8 | "Specialize" | [ "Lexmap" | "Relkit" ] ] [ 9 | "Construct" | [ "Lexmap" | "Relkit" ] ] [ 9 |
    "Global" | [ ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 1 | "Message" |
    [ ] ] [ 1 | "RelTable" | [ ] ] [ 11 | "Assert" | [ "Lexmap" | "Relmap" ] ] [ 12 | "Run" | [ "Lexmap" | "Relkit" |
    "Relmap" ] ] =}
|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 1 | "Message"
    | [ ] ] [ 4 | "Clause" | [ "Lexmap" ] ] [ 14 | "Resource" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" ] ] [ 15 |
    "Concrete" | [ "Relmap" ] ] [ 15 | "Include" | [ "Assert" | "Lexmap" | "Relmap" ] ] [ 15 | "Run" | [ "Assert" |
    "Lexmap" | "Relmap" ] ] [ 16 | "Read" | [ "Relmap" ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 2 | "Message" | [ ] ] [
    3 | "Lexmap" | [ ] ] [ 6 | "Relkit" | [ ] ] [ 10 | "Relmap" | [ ] ] [ 13 | "Assert" | [ ] ] [ 17 | "Resource" | [ ]
    ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Data" | [ ] ] [ 0 | "Overture" | [ ] ] [ 0 | "Syntax" | [ ] ] [ 0 | "System" | [ ] ] [ 0 | "Type" | [ ] ] [ 18 |
    "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] =}

*** 10 judges

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
                                              
  0         "Koshucode.Baala.Syntax"          /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              0     "Pattern"       [ ]
                                              
  2         "Koshucode.Baala.Core.Lexmap"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Lexmap"        [ ]
                                              1     "LexmapTrees"   [ ]
                                              1     "Message"       [ ]
                                              2     "Construct"     [ ]
                                              
  5         "Koshucode.Baala.Core.Relkit"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              4     "Relkit"        [ "Lexmap" ]
                                              5     "Construct"     [ ]
                                              5     "Run"           [ ]
                                              
  9         "Koshucode.Baala.Core.Relmap"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              1     "Result"        [ ]
                                              2     "Option"        [ ]
                                              7     "Relmap"        [ "Lexmap" | "Relkit" ]
                                              8     "Rop"           [ "Lexmap" ]
                                              8     "Specialize"    [ "Lexmap" | "Relkit" ]
                                              9     "Construct"     [ "Lexmap" | "Relkit" ]
                                              9     "Global"        [ ]
                                              
  12        "Koshucode.Baala.Core.Assert"     /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              1     "RelTable"      [ ]
                                              11    "Assert"        [ "Lexmap" | "Relmap" ]
                                              12    "Run"           [ "Lexmap" | "Relkit" | "Relmap" ]
                                              
  16        "Koshucode.Baala.Core.Resource"   /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              1     "Message"       [ ]
                                              4     "Clause"        [ "Lexmap" ]
                                              14    "Resource"      [ "Assert" | "Lexmap" | "Relkit" | "Relmap" ]
                                              15    "Concrete"      [ "Relmap" ]
                                              15    "Include"       [ "Assert" | "Lexmap" | "Relmap" ]
                                              15    "Run"           [ "Assert" | "Lexmap" | "Relmap" ]
                                              16    "Read"          [ "Relmap" ]
                                              
  17        "Koshucode.Baala.Core"            /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              2     "Message"       [ ]
                                              3     "Lexmap"        [ ]
                                              6     "Relkit"        [ ]
                                              10    "Relmap"        [ ]
                                              13    "Assert"        [ ]
                                              17    "Resource"      [ ]
                                              
  18        "Koshucode.Baala"                 /rank /base           /import-dir
                                              ----- --------------- ------------------------------------------------------------
                                              0     "Base"          [ ]
                                              0     "Data"          [ ]
                                              0     "Overture"      [ ]
                                              0     "Syntax"        [ ]
                                              0     "System"        [ ]
                                              0     "Type"          [ ]
                                              18    "Core"          [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                              

=== rel

**
**  SUMMARY
**      10 judges on DIR-RANK
**      10 judges in total
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
**    49 judges
**

|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.XHtml5"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.XHtml5"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.XHtml5.Attributes"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.Html.Renderer.Pretty"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Text.Blaze.Html.Renderer.String"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.Aeson"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.Aeson"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Json"               /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Type"
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
    "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Overture" | [ ] ] [ 0 | "Syntax" | [ ] ] [ 0 | "System" | [ ] ] [ 0 |
    "Type" | [ ] ] [ 3 | "Writer" | [ ] ] =}

*** 2 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                       /base-rank
  --------- -------------------------- ------------------------------
  2         "Koshucode.Baala.Writer"   /rank /base        /import-dir
                                       ----- ------------ -----------
                                       1     "Csv"        [ ]
                                       1     "Html"       [ ]
                                       1     "Json"       [ ]
                                       1     "Judge"      [ ]
                                       2     "Koshu"      [ ]
                                       
  3         "Koshucode.Baala"          /rank /base        /import-dir
                                       ----- ------------ -----------
                                       0     "Base"       [ ]
                                       0     "Core"       [ ]
                                       0     "Data"       [ ]
                                       0     "Overture"   [ ]
                                       0     "Syntax"     [ ]
                                       0     "System"     [ ]
                                       0     "Type"       [ ]
                                       3     "Writer"     [ ]
                                       

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
**    114 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Gadget"   /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Gadget"   /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Gadget"   /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Gadget"   /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Gadget"   /import "Koshucode.Baala.Rop.Flat.Applied.PoScale"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Gadget"   /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Peripheral"  /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Peripheral"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Peripheral"  /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Peripheral"  /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Peripheral"  /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.PoScale"  /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.PoScale"  /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.PoScale"  /import "Koshucode.Baala.DataPlus"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Rops"     /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Rops"     /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Rops"     /import "Koshucode.Baala.Rop.Flat.Applied.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Rops"     /import "Koshucode.Baala.Rop.Flat.Applied.Peripheral"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Rops"     /import "Koshucode.Baala.Rop.Flat.Applied.Subtext"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Subtext"  /import "Koshucode.Baala.Subtext"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Subtext"  /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Subtext"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Subtext"  /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Subtext"  /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied.Subtext"  /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied"          /import "Koshucode.Baala.Rop.Flat.Applied.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied"          /import "Koshucode.Baala.Rop.Flat.Applied.Peripheral"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied"          /import "Koshucode.Baala.Rop.Flat.Applied.PoScale"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied"          /import "Koshucode.Baala.Rop.Flat.Applied.Rops"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Applied"          /import "Koshucode.Baala.Rop.Flat.Applied.Subtext"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Check"            /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Elem"             /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Rop.Flat.Lattice.Restrict"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"      /import "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"  /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice"          /import "Koshucode.Baala.Rop.Flat.Lattice.Restrict"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice"          /import "Koshucode.Baala.Rop.Flat.Lattice.Rop"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice"          /import "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Meta"             /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Order"            /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Applied"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Check"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Control"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Elem"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Meta"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Order"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Resource"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Source"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.Term"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Rops"             /import "Koshucode.Baala.Rop.Flat.TermGadget"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Term"             /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.TermGadget"       /import "Koshucode.Baala.Rop.Flat.Term"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Applied"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Check"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Control"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Meta"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Order"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Resource"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat"                  /import "Koshucode.Baala.Rop.Flat.Rops"
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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Core" | [ ] ] [ 0 |
    "DataPlus" | [ ] ] [ 0 | "Subtext" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ]
    ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 0 | "Pattern" | [ ] ]
    =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Rop.Flat.Applied"  /base-rank {= /rank /base /import-dir [ 1 |
    "PoScale" | [ ] ] [ 1 | "Subtext" | [ ] ] [ 2 | "Gadget" | [ ] ] [ 2 | "Peripheral" | [ ] ] [ 3 | "Rops" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 1 |
    "Tropashko" | [ ] ] [ 2 | "Restrict" | [ ] ] [ 3 | "Rop" | [ ] ] =}

|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 1 | "Elem" | [ ] ] [
    1 | "Meta" | [ ] ] [ 1 | "Order" | [ ] ] [ 1 | "Resource" | [ ] ] [ 1 | "Source" | [ ] ] [ 1 | "Term" | [ ] ] [ 2 |
    "TermGadget" | [ ] ] [ 4 | "Applied" | [ ] ] [ 4 | "Lattice" | [ ] ] [ 5 | "Check" | [ "Lattice" ] ] [ 5 | "Control"
    | [ "Lattice" ] ] [ 6 | "Rops" | [ "Applied" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 7 |
    "Flat" | [ "Applied" | "Lattice" ] ] =}

*** 7 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                 /base-rank
  --------- ------------------------------------ ------------------------------------------------
  0         "Koshucode.Baala"                    /rank /base          /import-dir
                                                 ----- -------------- ---------------------------
                                                 0     "Core"         [ ]
                                                 0     "DataPlus"     [ ]
                                                 0     "Subtext"      [ ]
                                                 
  0         "Koshucode.Baala.Rop.Base"           /rank /base          /import-dir
                                                 ----- -------------- ---------------------------
                                                 0     "Message"      [ ]
                                                 
  0         "Koshucode.Baala.Syntax"             /rank /base          /import-dir
                                                 ----- -------------- ---------------------------
                                                 0     "Pattern"      [ ]
                                                 
  3         "Koshucode.Baala.Rop.Flat.Applied"   /rank /base          /import-dir
                                                 ----- -------------- ---------------------------
                                                 1     "PoScale"      [ ]
                                                 1     "Subtext"      [ ]
                                                 2     "Gadget"       [ ]
                                                 2     "Peripheral"   [ ]
                                                 3     "Rops"         [ ]
                                                 
  3         "Koshucode.Baala.Rop.Flat.Lattice"   /rank /base          /import-dir
                                                 ----- -------------- ---------------------------
                                                 1     "Tropashko"    [ ]
                                                 2     "Restrict"     [ ]
                                                 3     "Rop"          [ ]
                                                 
  6         "Koshucode.Baala.Rop.Flat"           /rank /base          /import-dir
                                                 ----- -------------- ---------------------------
                                                 1     "Elem"         [ ]
                                                 1     "Meta"         [ ]
                                                 1     "Order"        [ ]
                                                 1     "Resource"     [ ]
                                                 1     "Source"       [ ]
                                                 1     "Term"         [ ]
                                                 2     "TermGadget"   [ ]
                                                 4     "Applied"      [ ]
                                                 4     "Lattice"      [ ]
                                                 5     "Check"        [ "Lattice" ]
                                                 5     "Control"      [ "Lattice" ]
                                                 6     "Rops"         [ "Applied" | "Lattice" ]
                                                 
  7         "Koshucode.Baala.Rop"                /rank /base          /import-dir
                                                 ----- -------------- ---------------------------
                                                 0     "Base"         [ ]
                                                 7     "Flat"         [ "Applied" | "Lattice" ]
                                                 

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
**    25 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Flat"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Nest.Flow"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Rop.Flat"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Rop.Nest.Confl"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Flow"             /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rops"             /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rops"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rops"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rops"             /import "Koshucode.Baala.Rop.Nest.Confl"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rops"             /import "Koshucode.Baala.Rop.Nest.Deriv"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Rops"             /import "Koshucode.Baala.Rop.Nest.Flow"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest"                  /import "Koshucode.Baala.Rop.Nest.Confl"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest"                  /import "Koshucode.Baala.Rop.Nest.Deriv"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest"                  /import "Koshucode.Baala.Rop.Nest.Flow"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest"                  /import "Koshucode.Baala.Rop.Nest.Rops"

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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Core" | [ ] ] [ 0 |
    "DataPlus" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ]
    ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 1 | "Flow" | [ ] ] [
    2 | "Confl" | [ ] ] [ 3 | "Deriv" | [ ] ] [ 4 | "Rops" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Flat" | [ ] ] [ 5 | "Nest" | [ ] ] =}

*** 4 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                         /base-rank
  --------- ---------------------------- ------------------------------
  0         "Koshucode.Baala"            /rank /base        /import-dir
                                         ----- ------------ -----------
                                         0     "Core"       [ ]
                                         0     "DataPlus"   [ ]
                                         
  0         "Koshucode.Baala.Rop.Base"   /rank /base        /import-dir
                                         ----- ------------ -----------
                                         0     "Message"    [ ]
                                         
  4         "Koshucode.Baala.Rop.Nest"   /rank /base        /import-dir
                                         ----- ------------ -----------
                                         1     "Flow"       [ ]
                                         2     "Confl"      [ ]
                                         3     "Deriv"      [ ]
                                         4     "Rops"       [ ]
                                         
  5         "Koshucode.Baala.Rop"        /rank /base        /import-dir
                                         ----- ------------ -----------
                                         0     "Base"       [ ]
                                         0     "Flat"       [ ]
                                         5     "Nest"       [ ]
                                         

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
**    47 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Calc"              /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Rop.Flat"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Filter"            /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Cox.GeoDatumJp"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Rop.Base.Message"


|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Range"             /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Rop.Cox.Calc"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Rop.Cox.Empty"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Rop.Cox.Filter"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Rop.Cox.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Rop.Cox.Range"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Rop.Cox.Type.Clock"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Rop.Cox.Type.Dec"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Clock"        /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Clock"        /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Clock"        /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Calc"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Empty"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Filter"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Range"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Rops"

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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 0 | "Core" | [ ] ] [ 0 |
    "DataPlus" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 0 | "Message" | [ ]
    ] =}
|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Rop.Cox.Type"  /base-rank {= /rank /base /import-dir [ 1 | "Clock" | [
    ] ] [ 1 | "Dec" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 1 | "Calc" | [ ] ] [ 1 | "Empty" | [ ] ] [ 1 | "Filter" | [ ] ] [ 1 | "Gadget" | [ ] ] [ 1 | "Range" | [ ] ] [
    2 | "Rops" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Base" | [ ] ] [ 0 |
    "Flat" | [ ] ] [ 3 | "Cox" | [ ] ] =}

*** 5 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                             /base-rank
  --------- -------------------------------- --------------------------------
  0         "Koshucode.Baala"                /rank /base          /import-dir
                                             ----- -------------- -----------
                                             0     "Core"         [ ]
                                             0     "DataPlus"     [ ]
                                             
  0         "Koshucode.Baala.Rop.Base"       /rank /base          /import-dir
                                             ----- -------------- -----------
                                             0     "Message"      [ ]
                                             
  1         "Koshucode.Baala.Rop.Cox.Type"   /rank /base          /import-dir
                                             ----- -------------- -----------
                                             1     "Clock"        [ ]
                                             1     "Dec"          [ ]
                                             
  2         "Koshucode.Baala.Rop.Cox"        /rank /base          /import-dir
                                             ----- -------------- -----------
                                             0     "GeoDatumJp"   [ ]
                                             1     "Calc"         [ ]
                                             1     "Empty"        [ ]
                                             1     "Filter"       [ ]
                                             1     "Gadget"       [ ]
                                             1     "Range"        [ ]
                                             2     "Rops"         [ ]
                                             
  3         "Koshucode.Baala.Rop"            /rank /base          /import-dir
                                             ----- -------------- -----------
                                             0     "Base"         [ ]
                                             0     "Flat"         [ ]
                                             3     "Cox"          [ ]
                                             

=== rel

**
**  SUMMARY
**       5 judges on DIR-RANK
**       5 judges in total
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
**    38 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Type"
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
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Toolkit.Library.Run"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Writer"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.System.CliParser"
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
    "Cop" | [ ] ] [ 0 | "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Overture" | [ ] ] [ 0 | "Syntax" | [ ] ] [ 0 |
    "System" | [ ] ] [ 0 | "Type" | [ ] ] [ 0 | "Writer" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 0 | "Cox" | [ ] ] [ 0 |
    "Flat" | [ ] ] [ 0 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.System"  /base-rank {= /rank /base /import-dir [ 0 | "CliParser" | [ ]
    ] =}
|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 1 | "Element"
    | [ ] ] [ 1 | "Global" | [ ] ] [ 1 | "Run" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 2 |
    "KoshuFilter" | [ ] ] [ 2 | "KoshuMain" | [ ] ] =}

*** 5 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                /base-rank
  --------- ----------------------------------- ---------------------------------
  0         "Koshucode.Baala"                   /rank /base           /import-dir
                                                ----- --------------- -----------
                                                0     "Base"          [ ]
                                                0     "Cop"           [ ]
                                                0     "Core"          [ ]
                                                0     "Data"          [ ]
                                                0     "Overture"      [ ]
                                                0     "Syntax"        [ ]
                                                0     "System"        [ ]
                                                0     "Type"          [ ]
                                                0     "Writer"        [ ]
                                                
  0         "Koshucode.Baala.Rop"               /rank /base           /import-dir
                                                ----- --------------- -----------
                                                0     "Cox"           [ ]
                                                0     "Flat"          [ ]
                                                0     "Nest"          [ ]
                                                
  0         "Koshucode.Baala.System"            /rank /base           /import-dir
                                                ----- --------------- -----------
                                                0     "CliParser"     [ ]
                                                
  1         "Koshucode.Baala.Toolkit.Library"   /rank /base           /import-dir
                                                ----- --------------- -----------
                                                1     "Element"       [ ]
                                                1     "Global"        [ ]
                                                1     "Run"           [ ]
                                                
  2         "Koshucode.Baala.Toolkit.Main"      /rank /base           /import-dir
                                                ----- --------------- -----------
                                                2     "KoshuFilter"   [ ]
                                                2     "KoshuMain"     [ ]
                                                

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
**    41 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Writer"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Toolkit.Library.Input"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Data.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Input"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Change"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "Koshucode.Baala.Toolkit.Library.Version"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.Version"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.System"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Type"
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
    "Core" | [ ] ] [ 0 | "Data" | [ ] ] [ 0 | "Overture" | [ ] ] [ 0 | "Syntax" | [ ] ] [ 0 | "System" | [ ] ] [ 0 |
    "Type" | [ ] ] [ 0 | "Writer" | [ ] ] =}
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
                                                0     "Overture"      [ ]
                                                0     "Syntax"        [ ]
                                                0     "System"        [ ]
                                                0     "Type"          [ ]
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

# I/O List

- [./import-rank.k](#import-rankk)
- [../base/data/IMPORT.k](#basedataimportk)
- [../calculator/data/IMPORT.k](#calculatordataimportk)
- [../cop/data/IMPORT.k](#copdataimportk)
- [../core/data/IMPORT.k](#coredataimportk)
- [../data-plus/data/IMPORT.k](#data-plusdataimportk)
- [../data/data/IMPORT.k](#datadataimportk)
- [../overture/data/IMPORT.k](#overturedataimportk)
- [../rop-cox/data/IMPORT.k](#rop-coxdataimportk)
- [../rop-flat/data/IMPORT.k](#rop-flatdataimportk)
- [../rop-nested/data/IMPORT.k](#rop-nesteddataimportk)
- [../rop/data/IMPORT.k](#ropdataimportk)
- [../subtext/data/IMPORT.k](#subtextdataimportk)
- [../syntax/data/IMPORT.k](#syntaxdataimportk)
- [../toolkit/data/IMPORT.k](#toolkitdataimportk)
- [../type/data/IMPORT.k](#typedataimportk)
- [../writer/data/IMPORT.k](#writerdataimportk)
- [output](#output)



## [./import-rank.k](./import-rank.k)

```
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
  | partial-order-height /import /module -to /module /rank
  | interp {| /module has dependent rank /rank . |}
  --order --forward /rank
```



## [../base/data/IMPORT.k](../base/data/IMPORT.k)

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
**    184 judges
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

|-- IMPORT  /module "Koshucode.Baala.Base.Code.Line"            /import "Data.ByteString"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Line"            /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Line"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Code.Line"            /import "Koshucode.Baala.Base.Abort"
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
|-- IMPORT  /module "Koshucode.Baala.Base.IO.BzFile"            /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Base.IO.IOPoint"           /import "Koshucode.Baala.Base.IO.BzFile"

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

|-- IMPORT  /module "Koshucode.Baala.Base.MixText.Deriv"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.Deriv"        /import "Koshucode.Baala.Base.MixText.MixText"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.Deriv"        /import "Koshucode.Baala.Base.MixText.MixClass"

|-- IMPORT  /module "Koshucode.Baala.Base.MixText.LineBreak"    /import "Data.Default"

|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Data.ByteString"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixClass"     /import "Koshucode.Baala.Base.MixText.MixText"

|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixEncode"    /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.MixText.MixEncode"    /import "Koshucode.Baala.Base.MixText.MixText"

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
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Koshucode.Baala.Overture"

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



## [../calculator/data/IMPORT.k](../calculator/data/IMPORT.k)

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



## [../cop/data/IMPORT.k](../cop/data/IMPORT.k)

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
**    66 judges
**

|-- IMPORT  /module "Koshucode.Baala.Cop.Arith"                 /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Arith"                 /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.Arith"                 /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Cop.Arith"                 /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Arith"                 /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Cop.Arith"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Cop.List"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Cop.Logic"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Cop.Misc"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Cop.Order"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Cop.Text"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Cop.Time"
|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Cop.Type"

|-- IMPORT  /module "Koshucode.Baala.Cop.Coxhand"               /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Coxhand"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.Coxhand"               /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Cop.Coxhand"               /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Cop.Replace"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Logic"                 /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Logic"                 /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Logic"                 /import "Koshucode.Baala.Cop.Coxhand"

|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Order"                 /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Order"                 /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Order"                 /import "Koshucode.Baala.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Cop.Order"                 /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Replace"               /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Subtext"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Time"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Time"                  /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Cop.Time"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Time"                  /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Type"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Type"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Type"                  /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop"                       /import "Koshucode.Baala.Cop.Arith"
|-- IMPORT  /module "Koshucode.Baala.Cop"                       /import "Koshucode.Baala.Cop.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Cop"                       /import "Koshucode.Baala.Cop.List"
|-- IMPORT  /module "Koshucode.Baala.Cop"                       /import "Koshucode.Baala.Cop.Logic"
|-- IMPORT  /module "Koshucode.Baala.Cop"                       /import "Koshucode.Baala.Cop.Misc"
|-- IMPORT  /module "Koshucode.Baala.Cop"                       /import "Koshucode.Baala.Cop.Order"
|-- IMPORT  /module "Koshucode.Baala.Cop"                       /import "Koshucode.Baala.Cop.Time"
|-- IMPORT  /module "Koshucode.Baala.Cop"                       /import "Koshucode.Baala.Cop.Type"

```



## [../core/data/IMPORT.k](../core/data/IMPORT.k)

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
**    216 judges
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
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Syntax"
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
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Message"     /import "Koshucode.Baala.Overture"
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



## [../data-plus/data/IMPORT.k](../data-plus/data/IMPORT.k)

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
**    5 judges
**

|-- IMPORT  /module "Koshucode.Baala.DataPlus"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.DataPlus"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.DataPlus"                  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.DataPlus"                  /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.DataPlus"                  /import "Koshucode.Baala.Data"

```



## [../data/data/IMPORT.k](../data/data/IMPORT.k)

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
**    147 judges
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

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Data.Class.Edge"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Data.Class.Simple"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Syntax"
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
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Map"            /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Map"            /import "Koshucode.Baala.Data.Class.Simple"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Map"            /import "Koshucode.Baala.Data.Class.Complex"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Message"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Message"        /import "Koshucode.Baala.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Prelude"
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

|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Message"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Decode.Message"       /import "Koshucode.Baala.Base"

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



## [../overture/data/IMPORT.k](../overture/data/IMPORT.k)

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
**    76 judges
**

|-- IMPORT  /module "Koshucode.Baala.Overture.Cache"            /import "Data.Map.Strict"

|-- IMPORT  /module "Koshucode.Baala.Overture.Fn"               /import "Koshucode.Baala.Overture.Name.String"


|-- IMPORT  /module "Koshucode.Baala.Overture.Infix"            /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Overture.Infix"            /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Overture.Infix"            /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Overture.Infix"            /import "Koshucode.Baala.Overture.Type"

|-- IMPORT  /module "Koshucode.Baala.Overture.List"             /import "Data.ListLike"
|-- IMPORT  /module "Koshucode.Baala.Overture.List"             /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Overture.List"             /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Overture.List"             /import "Koshucode.Baala.Overture.Infix"
|-- IMPORT  /module "Koshucode.Baala.Overture.List"             /import "Koshucode.Baala.Overture.Shorthand"

|-- IMPORT  /module "Koshucode.Baala.Overture.Misc"             /import "Koshucode.Baala.Overture.Type"



|-- IMPORT  /module "Koshucode.Baala.Overture.Name.Text"        /import "Data.Text"

|-- IMPORT  /module "Koshucode.Baala.Overture.Shorthand"        /import "Data.ByteString"
|-- IMPORT  /module "Koshucode.Baala.Overture.Shorthand"        /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Overture.Shorthand"        /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Overture.Shorthand"        /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Overture.Shorthand"        /import "Koshucode.Baala.Overture.Type"


|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Category"    /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Category"    /import "Data.Set"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Integer"     /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Integer"     /import "Data.IntMap.Strict"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Integer"     /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Integer"     /import "Numeric"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Integer"     /import "Koshucode.Baala.Overture.List"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Integer"     /import "Koshucode.Baala.Overture.Text.Textual"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Span"        /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Span"        /import "Data.Text.Internal"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Span"        /import "Data.Text.Unsafe"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.String"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.Text.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.Text.Encoding.Error"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.Text.Lazy.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.ByteString.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Data.ByteString.Lazy.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Koshucode.Baala.Overture.Infix"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Koshucode.Baala.Overture.List"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Textual"     /import "Koshucode.Baala.Overture.Shorthand"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Unicode"     /import "Data.Char"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Utility"     /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Utility"     /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Utility"     /import "Koshucode.Baala.Overture.Infix"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Utility"     /import "Koshucode.Baala.Overture.List"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Utility"     /import "Koshucode.Baala.Overture.Type"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Utility"     /import "Koshucode.Baala.Overture.Text.Textual"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text"             /import "Koshucode.Baala.Overture.Text.Category"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text"             /import "Koshucode.Baala.Overture.Text.Integer"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text"             /import "Koshucode.Baala.Overture.Text.Textual"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text"             /import "Koshucode.Baala.Overture.Text.Unicode"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text"             /import "Koshucode.Baala.Overture.Text.Utility"


|-- IMPORT  /module "Koshucode.Baala.Overture.Value"            /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Overture.Value"            /import "Data.Text.Lazy"

|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Cache"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Index"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Infix"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.List"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Misc"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Shorthand"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Some"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Text"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Type"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Value"

|-- IMPORT  /module "Koshucode.Baala.System.CliParser"          /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.System.CliParser"          /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.System.CliParser"          /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.System.CliParser"          /import "System.Environment"

|-- IMPORT  /module "Koshucode.Baala.System.Exit"               /import "System.Environment"
|-- IMPORT  /module "Koshucode.Baala.System.Exit"               /import "System.Exit"
|-- IMPORT  /module "Koshucode.Baala.System.Exit"               /import "System.IO"

|-- IMPORT  /module "Koshucode.Baala.System"                    /import "Koshucode.Baala.System.CliParser"
|-- IMPORT  /module "Koshucode.Baala.System"                    /import "Koshucode.Baala.System.Exit"

```



## [../rop-cox/data/IMPORT.k](../rop-cox/data/IMPORT.k)

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
**    56 judges
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Rop.Cox.Type.Time"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Rops"              /import "Koshucode.Baala.Rop.Cox.Type.Text"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Clock"        /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Clock"        /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Clock"        /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Dec"          /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Text"         /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Text"         /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Text"         /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Text"         /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Time"         /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Time"         /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Type.Time"         /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Calc"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Empty"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Filter"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Range"
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox"                   /import "Koshucode.Baala.Rop.Cox.Rops"

```



## [../rop-flat/data/IMPORT.k](../rop-flat/data/IMPORT.k)

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



## [../rop-nested/data/IMPORT.k](../rop-nested/data/IMPORT.k)

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



## [../rop/data/IMPORT.k](../rop/data/IMPORT.k)

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
**    30 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Define"           /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Define"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Define"           /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Cox"          /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Cox"          /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Cox"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Cox"          /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Cox"          /import "Koshucode.Baala.Rop.Base.Get.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Cox"          /import "Koshucode.Baala.Rop.Base.Get.Rel"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Cox"          /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Get"          /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Get"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Get"          /import "Koshucode.Baala.Syntax.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Get"          /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Rel"          /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Rel"          /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Rel"          /import "Koshucode.Baala.Rop.Base.Get.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get.Rel"          /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Message"          /import "Koshucode.Baala.Core.Message"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Message"          /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Message"          /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Rops"             /import "Koshucode.Baala.DataPlus"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Rops"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Rops"             /import "Koshucode.Baala.Rop.Base.Define"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Rops"             /import "Koshucode.Baala.Rop.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base"                  /import "Koshucode.Baala.Rop.Base.Define"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base"                  /import "Koshucode.Baala.Rop.Base.Get.Cox"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base"                  /import "Koshucode.Baala.Rop.Base.Get.Get"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base"                  /import "Koshucode.Baala.Rop.Base.Get.Rel"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base"                  /import "Koshucode.Baala.Rop.Base.Rops"

```



## [../subtext/data/IMPORT.k](../subtext/data/IMPORT.k)

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
**    72 judges
**

|-- IMPORT  /module "Koshucode.Baala.Subtext.Bundle"            /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Bundle"            /import "Koshucode.Baala.Overture.Fn"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Bundle"            /import "Koshucode.Baala.Subtext.Expr"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Bundle"            /import "Koshucode.Baala.Subtext.MinMax"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Bundle"            /import "Koshucode.Baala.Subtext.Operator"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Expr"              /import "Koshucode.Baala.Overture.Fn"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Expr"              /import "Koshucode.Baala.Subtext.MinMax"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Overture.Fn"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.Expr"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.MinMax"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.Operator"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.Para"


|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Basic"    /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Basic"    /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Basic"    /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Basic"    /import "Koshucode.Baala.Overture.Fn"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Basic"    /import "Koshucode.Baala.Subtext.Expr"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Char"     /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Char"     /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Char"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Char"     /import "Koshucode.Baala.Overture.Fn"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Char"     /import "Koshucode.Baala.Subtext.Expr"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Char"     /import "Koshucode.Baala.Subtext.Operator.Basic"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Char"     /import "Koshucode.Baala.Subtext.Operator.Repeat"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Char"     /import "Koshucode.Baala.Subtext.Operator.Combine"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Combine"  /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Combine"  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Combine"  /import "Koshucode.Baala.Overture.Fn"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Combine"  /import "Koshucode.Baala.Subtext.Expr"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Combine"  /import "Koshucode.Baala.Subtext.MinMax"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Combine"  /import "Koshucode.Baala.Subtext.Operator.Basic"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Repeat"   /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Repeat"   /import "Koshucode.Baala.Subtext.Expr"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Repeat"   /import "Koshucode.Baala.Subtext.MinMax"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Repeat"   /import "Koshucode.Baala.Subtext.Operator.Basic"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Repeat"   /import "Koshucode.Baala.Subtext.Operator.Combine"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator"          /import "Koshucode.Baala.Subtext.Operator.Basic"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator"          /import "Koshucode.Baala.Subtext.Operator.Char"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator"          /import "Koshucode.Baala.Subtext.Operator.Combine"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator"          /import "Koshucode.Baala.Subtext.Operator.Repeat"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Overture.Fn"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Subtext.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Subtext.Expr"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Subtext.MinMax"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Subtext.Operator"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Sivmap"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Sivmap"      /import "Koshucode.Baala.Subtext.Operator"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Sivmap"      /import "Koshucode.Baala.Subtext.Sieve.Tree"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Token"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Token"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Token"       /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Tree"        /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Tree"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Tree"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Tree"        /import "Koshucode.Baala.Subtext.Match"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Tree"        /import "Koshucode.Baala.Subtext.Operator"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Tree"        /import "Koshucode.Baala.Subtext.Sieve.Token"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve.Tree"        /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve"             /import "Koshucode.Baala.Subtext.Sieve.Sivmap"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve"             /import "Koshucode.Baala.Subtext.Sieve.Token"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Sieve"             /import "Koshucode.Baala.Subtext.Sieve.Tree"

|-- IMPORT  /module "Koshucode.Baala.Subtext"                   /import "Koshucode.Baala.Subtext.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Subtext"                   /import "Koshucode.Baala.Subtext.Match"
|-- IMPORT  /module "Koshucode.Baala.Subtext"                   /import "Koshucode.Baala.Subtext.Operator"
|-- IMPORT  /module "Koshucode.Baala.Subtext"                   /import "Koshucode.Baala.Subtext.Sieve"

```



## [../syntax/data/IMPORT.k](../syntax/data/IMPORT.k)

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
**    184 judges
**

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Syntax.Symbol"
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

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrName"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.AttrName"      /import "Koshucode.Baala.Syntax.Symbol"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Message"       /import "Koshucode.Baala.Syntax.Symbol"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Syntax.Symbol"
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

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Chars"       /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Message"     /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Syntax.Symbol.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Syntax.Symbol.Symbol"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Symbol"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Symbol"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Symbol"      /import "Koshucode.Baala.Syntax.Symbol.Next"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Symbol"      /import "Koshucode.Baala.Syntax.Symbol.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Term"        /import "Data.String"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Term"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Term"        /import "Koshucode.Baala.Syntax.Symbol.Chars"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Chars"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Next"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Short"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Term"

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

|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Bracket"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Bracket"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Bracket"       /import "Koshucode.Baala.Syntax.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Parse"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Parse"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Parse"         /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Parse"         /import "Koshucode.Baala.Syntax.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Parse"         /import "Koshucode.Baala.Syntax.Tree.Bracket"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Tree.Pattern"       /import "Koshucode.Baala.Overture"
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



## [../toolkit/data/IMPORT.k](../toolkit/data/IMPORT.k)

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



## [../type/data/IMPORT.k](../type/data/IMPORT.k)

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
**    129 judges
**

|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.BinaryAb"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.BinaryAb"     /import "Koshucode.Baala.Type.Decimal.Coder"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.BinaryAb"     /import "Koshucode.Baala.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.BinaryAb"     /import "Koshucode.Baala.Type.Decimal.Fraction"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.BinaryAb"     /import "Koshucode.Baala.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Coder"        /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Coder"        /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Coder"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Coder"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Coder"        /import "Koshucode.Baala.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Coder"        /import "Koshucode.Baala.Type.Decimal.Fraction"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Coder"        /import "Koshucode.Baala.Type.Decimal.Rational"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Coder"        /import "Koshucode.Baala.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Decimal"      /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Decimal"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Decimal"      /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Fraction"     /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Fraction"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Fraction"     /import "Koshucode.Baala.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Fraction"     /import "Koshucode.Baala.Type.Decimal.Rational"

|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Rational"     /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal.Rational"     /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Type.Decimal"              /import "Koshucode.Baala.Type.Decimal.BinaryAb"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal"              /import "Koshucode.Baala.Type.Decimal.Coder"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal"              /import "Koshucode.Baala.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal"              /import "Koshucode.Baala.Type.Decimal.Fraction"
|-- IMPORT  /module "Koshucode.Baala.Type.Decimal"              /import "Koshucode.Baala.Type.Decimal.Rational"

|-- IMPORT  /module "Koshucode.Baala.Type.Judge.Interp"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Judge.Interp"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Type.Judge.Interp"         /import "Koshucode.Baala.Type.Judge.Judge"
|-- IMPORT  /module "Koshucode.Baala.Type.Judge.Interp"         /import "Koshucode.Baala.Type.Judge.JudgeClass"

|-- IMPORT  /module "Koshucode.Baala.Type.Judge.Judge"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Judge.Judge"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Judge.Judge"          /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Type.Judge.Judge"          /import "Koshucode.Baala.Type.Judge.JudgeClass"

|-- IMPORT  /module "Koshucode.Baala.Type.Judge.JudgeClass"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Judge.JudgeClass"     /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Type.Judge"                /import "Koshucode.Baala.Type.Judge.Interp"
|-- IMPORT  /module "Koshucode.Baala.Type.Judge"                /import "Koshucode.Baala.Type.Judge.Judge"
|-- IMPORT  /module "Koshucode.Baala.Type.Judge"                /import "Koshucode.Baala.Type.Judge.JudgeClass"

|-- IMPORT  /module "Koshucode.Baala.Type.Message"              /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Message"              /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Head"             /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Head"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Head"             /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Head"             /import "Koshucode.Baala.Type.Type"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Head"             /import "Koshucode.Baala.Type.Judge"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Head"             /import "Koshucode.Baala.Type.Rel.TermPicker"

|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Mono"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Mono"             /import "Koshucode.Baala.Type.Rel.Rel"

|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Order"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Order"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Order"            /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Order"            /import "Koshucode.Baala.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Rel"              /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Rel"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Rel"              /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Rel"              /import "Koshucode.Baala.Type.Judge"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.Rel"              /import "Koshucode.Baala.Type.Rel.Head"

|-- IMPORT  /module "Koshucode.Baala.Type.Rel.TermPicker"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.TermPicker"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.TermPicker"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel.TermPicker"       /import "Koshucode.Baala.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Type.Rel"                  /import "Koshucode.Baala.Type.Rel.Head"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel"                  /import "Koshucode.Baala.Type.Rel.Mono"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel"                  /import "Koshucode.Baala.Type.Rel.Order"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel"                  /import "Koshucode.Baala.Type.Rel.Rel"
|-- IMPORT  /module "Koshucode.Baala.Type.Rel"                  /import "Koshucode.Baala.Type.Rel.TermPicker"

|-- IMPORT  /module "Koshucode.Baala.Type.Time.Clock"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Clock"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Clock"           /import "Koshucode.Baala.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Clock"           /import "Koshucode.Baala.Type.Time.Parts"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Clock"           /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Type.Time.ClockCalc"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.ClockCalc"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.ClockCalc"       /import "Koshucode.Baala.Type.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.ClockCalc"       /import "Koshucode.Baala.Type.Time.Parts"

|-- IMPORT  /module "Koshucode.Baala.Type.Time.Date"            /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Date"            /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Date"            /import "Data.Time.Calendar.OrdinalDate"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Date"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Date"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Date"            /import "Koshucode.Baala.Type.Time.Parts"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Date"            /import "Koshucode.Baala.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Type.Time.Parts"           /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Parts"           /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Parts"           /import "Data.Time.Calendar.OrdinalDate"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Parts"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Parts"           /import "Koshucode.Baala.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Data.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Data.Time.LocalTime"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Koshucode.Baala.Type.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Koshucode.Baala.Type.Time.ClockCalc"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Koshucode.Baala.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.Time"            /import "Koshucode.Baala.Type.Time.Parts"

|-- IMPORT  /module "Koshucode.Baala.Type.Time.TimeCalc"        /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.TimeCalc"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.TimeCalc"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.TimeCalc"        /import "Koshucode.Baala.Type.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.TimeCalc"        /import "Koshucode.Baala.Type.Time.ClockCalc"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.TimeCalc"        /import "Koshucode.Baala.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.TimeCalc"        /import "Koshucode.Baala.Type.Time.Parts"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.TimeCalc"        /import "Koshucode.Baala.Type.Time.Time"
|-- IMPORT  /module "Koshucode.Baala.Type.Time.TimeCalc"        /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Type.Time"                 /import "Koshucode.Baala.Type.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Type.Time"                 /import "Koshucode.Baala.Type.Time.ClockCalc"
|-- IMPORT  /module "Koshucode.Baala.Type.Time"                 /import "Koshucode.Baala.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Type.Time"                 /import "Koshucode.Baala.Type.Time.Parts"
|-- IMPORT  /module "Koshucode.Baala.Type.Time"                 /import "Koshucode.Baala.Type.Time.Time"
|-- IMPORT  /module "Koshucode.Baala.Type.Time"                 /import "Koshucode.Baala.Type.Time.TimeCalc"

|-- IMPORT  /module "Koshucode.Baala.Type.Type.Genus"           /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Type.Type.Genus"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Type.Genus"           /import "Koshucode.Baala.Data.Class"

|-- IMPORT  /module "Koshucode.Baala.Type.Type.Predefined where"  /import "Koshucode.Baala.Type.Type.Genus"
|-- IMPORT  /module "Koshucode.Baala.Type.Type.Predefined where"  /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Type.Type.Predefined where"  /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Type.Type"                 /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Type.Type"                 /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Type"                 /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Type.Type"                 /import "Koshucode.Baala.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Type"                      /import "Koshucode.Baala.Type.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Type"                      /import "Koshucode.Baala.Type.Judge"
|-- IMPORT  /module "Koshucode.Baala.Type"                      /import "Koshucode.Baala.Type.Rel"
|-- IMPORT  /module "Koshucode.Baala.Type"                      /import "Koshucode.Baala.Type.Time"
|-- IMPORT  /module "Koshucode.Baala.Type"                      /import "Koshucode.Baala.Type.Type"

```



## [../writer/data/IMPORT.k](../writer/data/IMPORT.k)

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
**    48 judges
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

|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Data.Map.Strict"
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



## output


Command `./import-rank.k ../base/data/IMPORT.k ../calculator/data/IMPORT.k ../cop/data/IMPORT.k ../core/data/IMPORT.k ../data-plus/data/IMPORT.k ../data/data/IMPORT.k ../overture/data/IMPORT.k ../rop-cox/data/IMPORT.k ../rop-flat/data/IMPORT.k ../rop-nested/data/IMPORT.k ../rop/data/IMPORT.k ../subtext/data/IMPORT.k ../syntax/data/IMPORT.k ../toolkit/data/IMPORT.k ../type/data/IMPORT.k ../writer/data/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./import-rank.k
**    ../base/data/IMPORT.k
**    ../calculator/data/IMPORT.k
**    ../cop/data/IMPORT.k
**    ../core/data/IMPORT.k
**    ../data-plus/data/IMPORT.k
**    ../data/data/IMPORT.k
**    ../overture/data/IMPORT.k
**    ../rop-cox/data/IMPORT.k
**    ../rop-flat/data/IMPORT.k
**    ../rop-nested/data/IMPORT.k
**    ../rop/data/IMPORT.k
**    ../subtext/data/IMPORT.k
**    ../syntax/data/IMPORT.k
**    ../toolkit/data/IMPORT.k
**    ../type/data/IMPORT.k
**    ../writer/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.List.Split"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.MixText.LineBreak"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.Prelude.Case"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.Prelude.Class"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Cache"

|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Index"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Name.String"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Some"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Text.Category"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Text.Unicode"

|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Type"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Value"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Rop.Cox.GeoDatumJp"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Subtext.MinMax"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.System.CliParser"

|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.System.Exit"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Toolkit.Library.Version"
|-- IMPORT-RANK  /rank 1  /module "Koshucode.Baala.Overture.Fn"
|-- IMPORT-RANK  /rank 1  /module "Koshucode.Baala.Overture.Infix"
|-- IMPORT-RANK  /rank 1  /module "Koshucode.Baala.Overture.Misc"

|-- IMPORT-RANK  /rank 1  /module "Koshucode.Baala.Overture.Shorthand"
|-- IMPORT-RANK  /rank 1  /module "Koshucode.Baala.System"
|-- IMPORT-RANK  /rank 2  /module "Koshucode.Baala.Overture.List"
|-- IMPORT-RANK  /rank 2  /module "Koshucode.Baala.Subtext.Expr"
|-- IMPORT-RANK  /rank 3  /module "Koshucode.Baala.Overture.Text.Textual"
*** 25

|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Overture.Text.Integer"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Overture.Text.Utility"
|-- IMPORT-RANK  /rank 5  /module "Koshucode.Baala.Overture.Text"
|-- IMPORT-RANK  /rank 6  /module "Koshucode.Baala.Overture"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.List.Assoc"

|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.List.List"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.List.Select"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.List.Set"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.MixText.MixText"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.Prelude.Import"

|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.Prelude.Pair"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.Text.Comment"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.Text.Suffix"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.Text.TextTable"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Cop.Replace"

|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Subtext.Operator.Basic"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Syntax.Symbol.Chars"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Type.Decimal.Rational"
|-- IMPORT-RANK  /rank 8  /module "Koshucode.Baala.Base.List.Picker"

|-- IMPORT-RANK  /rank 8  /module "Koshucode.Baala.Base.MixText.MixClass"
|-- IMPORT-RANK  /rank 8  /module "Koshucode.Baala.Base.MixText.MixEncode"
|-- IMPORT-RANK  /rank 8  /module "Koshucode.Baala.Base.Prelude.Queue"
|-- IMPORT-RANK  /rank 8  /module "Koshucode.Baala.Subtext.Operator.Combine"
|-- IMPORT-RANK  /rank 8  /module "Koshucode.Baala.Syntax.Symbol.Term"
*** 50

|-- IMPORT-RANK  /rank 9  /module "Koshucode.Baala.Base.MixText.Deriv"
|-- IMPORT-RANK  /rank 9  /module "Koshucode.Baala.Base.Prelude"
|-- IMPORT-RANK  /rank 9  /module "Koshucode.Baala.Subtext.Operator.Repeat"
|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Base.Abort.CodePos"
|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Base.IO.Encoding"

|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Base.IO.Http"
|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Base.List.Order"
|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Base.MixText"
|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Base.Text.PPrint"
|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Subtext.Operator.Char"

|-- IMPORT-RANK  /rank 11  /module "Koshucode.Baala.Base.List"
|-- IMPORT-RANK  /rank 11  /module "Koshucode.Baala.Subtext.Operator"
|-- IMPORT-RANK  /rank 12  /module "Koshucode.Baala.Base.Abort.Reason"
|-- IMPORT-RANK  /rank 12  /module "Koshucode.Baala.Base.Text.Dots"
|-- IMPORT-RANK  /rank 12  /module "Koshucode.Baala.Subtext.Bundle"

|-- IMPORT-RANK  /rank 13  /module "Koshucode.Baala.Base.Text"
|-- IMPORT-RANK  /rank 13  /module "Koshucode.Baala.Subtext.Para"
|-- IMPORT-RANK  /rank 14  /module "Koshucode.Baala.Base.Abort.Report"
|-- IMPORT-RANK  /rank 14  /module "Koshucode.Baala.Subtext.Match"
|-- IMPORT-RANK  /rank 15  /module "Koshucode.Baala.Base.Abort"

|-- IMPORT-RANK  /rank 16  /module "Koshucode.Baala.Base.Abort.Message"
|-- IMPORT-RANK  /rank 16  /module "Koshucode.Baala.Base.Code.Line"
|-- IMPORT-RANK  /rank 16  /module "Koshucode.Baala.Base.Code.Message"
|-- IMPORT-RANK  /rank 16  /module "Koshucode.Baala.Base.IO.BzFile"
|-- IMPORT-RANK  /rank 17  /module "Koshucode.Baala.Base.Code.Bracket"
*** 75

|-- IMPORT-RANK  /rank 17  /module "Koshucode.Baala.Base.Code.Clause"
|-- IMPORT-RANK  /rank 17  /module "Koshucode.Baala.Base.IO.IOPoint"
|-- IMPORT-RANK  /rank 17  /module "Koshucode.Baala.Base.Message"
|-- IMPORT-RANK  /rank 18  /module "Koshucode.Baala.Base.Code.Tree"
|-- IMPORT-RANK  /rank 18  /module "Koshucode.Baala.Base.IO"

|-- IMPORT-RANK  /rank 19  /module "Koshucode.Baala.Base.Code.Infix"
|-- IMPORT-RANK  /rank 19  /module "Koshucode.Baala.Base.Code.Scan"
|-- IMPORT-RANK  /rank 20  /module "Koshucode.Baala.Base.Code"
|-- IMPORT-RANK  /rank 21  /module "Koshucode.Baala.Base"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Core.Lexmap.Message"

|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Core.Resource.Message"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Data.Decode.Message"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Subtext.Sieve.Token"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Syntax.Para.Para"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Syntax.Symbol.Message"

|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Syntax.Token.Message"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Type.Decimal.Decimal"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Type.Message"
|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Data.Class.Message"
|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Subtext.Sieve.Tree"

|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Syntax.Para.Get"
|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Syntax.Para.ParaSpec"
|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Syntax.Symbol.Next"
|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Type.Decimal.Fraction"
|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Type.Time.Parts"
*** 100

|-- IMPORT-RANK  /rank 24  /module "Koshucode.Baala.Subtext.Sieve.Sivmap"
|-- IMPORT-RANK  /rank 24  /module "Koshucode.Baala.Syntax.Para.Parse"
|-- IMPORT-RANK  /rank 24  /module "Koshucode.Baala.Syntax.Symbol.Symbol"
|-- IMPORT-RANK  /rank 24  /module "Koshucode.Baala.Type.Decimal.Coder"
|-- IMPORT-RANK  /rank 24  /module "Koshucode.Baala.Type.Time.Date"

|-- IMPORT-RANK  /rank 25  /module "Koshucode.Baala.Subtext.Sieve"
|-- IMPORT-RANK  /rank 25  /module "Koshucode.Baala.Syntax.Para"
|-- IMPORT-RANK  /rank 25  /module "Koshucode.Baala.Syntax.Symbol.Short"
|-- IMPORT-RANK  /rank 25  /module "Koshucode.Baala.Type.Decimal.BinaryAb"
|-- IMPORT-RANK  /rank 25  /module "Koshucode.Baala.Type.Time.Clock"

|-- IMPORT-RANK  /rank 26  /module "Koshucode.Baala.Subtext"
|-- IMPORT-RANK  /rank 26  /module "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT-RANK  /rank 26  /module "Koshucode.Baala.Type.Decimal"
|-- IMPORT-RANK  /rank 26  /module "Koshucode.Baala.Type.Time.ClockCalc"
|-- IMPORT-RANK  /rank 27  /module "Koshucode.Baala.Syntax.Attr.AttrName"

|-- IMPORT-RANK  /rank 27  /module "Koshucode.Baala.Syntax.Attr.Message"
|-- IMPORT-RANK  /rank 27  /module "Koshucode.Baala.Syntax.Subtree.Subtree"
|-- IMPORT-RANK  /rank 27  /module "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT-RANK  /rank 27  /module "Koshucode.Baala.Type.Time.Time"
|-- IMPORT-RANK  /rank 28  /module "Koshucode.Baala.Syntax.Message"

|-- IMPORT-RANK  /rank 28  /module "Koshucode.Baala.Syntax.Subtree.DirTree"
|-- IMPORT-RANK  /rank 28  /module "Koshucode.Baala.Syntax.Subtree.Xml"
|-- IMPORT-RANK  /rank 28  /module "Koshucode.Baala.Syntax.Token.Clip"
|-- IMPORT-RANK  /rank 28  /module "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT-RANK  /rank 28  /module "Koshucode.Baala.Syntax.Token.Utility"
*** 125

|-- IMPORT-RANK  /rank 28  /module "Koshucode.Baala.Type.Time.TimeCalc"
|-- IMPORT-RANK  /rank 29  /module "Koshucode.Baala.Syntax.Token.Section"
|-- IMPORT-RANK  /rank 29  /module "Koshucode.Baala.Type.Time"
|-- IMPORT-RANK  /rank 30  /module "Koshucode.Baala.Syntax.Token.Rel"
|-- IMPORT-RANK  /rank 31  /module "Koshucode.Baala.Syntax.Token.Clause"

|-- IMPORT-RANK  /rank 32  /module "Koshucode.Baala.Syntax.Token"
|-- IMPORT-RANK  /rank 33  /module "Koshucode.Baala.Syntax.Tree.Bracket"
|-- IMPORT-RANK  /rank 34  /module "Koshucode.Baala.Syntax.Tree.Parse"
|-- IMPORT-RANK  /rank 34  /module "Koshucode.Baala.Syntax.Tree.Pattern"
|-- IMPORT-RANK  /rank 35  /module "Koshucode.Baala.Syntax.Pattern"

|-- IMPORT-RANK  /rank 35  /module "Koshucode.Baala.Syntax.Tree.Split"
|-- IMPORT-RANK  /rank 36  /module "Koshucode.Baala.Syntax.Tree"
|-- IMPORT-RANK  /rank 37  /module "Koshucode.Baala.Syntax.Attr.Attr"
|-- IMPORT-RANK  /rank 37  /module "Koshucode.Baala.Syntax.Attr.Slot"
|-- IMPORT-RANK  /rank 37  /module "Koshucode.Baala.Syntax.Subtree.Decode"

|-- IMPORT-RANK  /rank 38  /module "Koshucode.Baala.Syntax.Attr.AttrEd"
|-- IMPORT-RANK  /rank 38  /module "Koshucode.Baala.Syntax.Subtree"
|-- IMPORT-RANK  /rank 39  /module "Koshucode.Baala.Syntax.Attr"
|-- IMPORT-RANK  /rank 40  /module "Koshucode.Baala.Syntax"
|-- IMPORT-RANK  /rank 41  /module "Koshucode.Baala.Core.Assert.Message"

|-- IMPORT-RANK  /rank 41  /module "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT-RANK  /rank 41  /module "Koshucode.Baala.Core.Relkit.Message"
|-- IMPORT-RANK  /rank 41  /module "Koshucode.Baala.Data.Decode.Term"
|-- IMPORT-RANK  /rank 41  /module "Koshucode.Baala.Type.Judge.JudgeClass"
|-- IMPORT-RANK  /rank 42  /module "Koshucode.Baala.Type.Judge.Judge"
*** 150

|-- IMPORT-RANK  /rank 43  /module "Koshucode.Baala.Type.Judge.Interp"
|-- IMPORT-RANK  /rank 44  /module "Koshucode.Baala.Type.Judge"
|-- IMPORT-RANK  /rank 45  /module "Koshucode.Baala.Type.Rel.Order"
|-- IMPORT-RANK  /rank 45  /module "Koshucode.Baala.Type.Rel.TermPicker"
|-- IMPORT-RANK  /rank 45  /module "Koshucode.Baala.Type.Type"

|-- IMPORT-RANK  /rank 46  /module "Koshucode.Baala.Type.Rel.Head"
|-- IMPORT-RANK  /rank 47  /module "Koshucode.Baala.Type.Rel.Rel"
|-- IMPORT-RANK  /rank 48  /module "Koshucode.Baala.Type.Rel.Mono"
|-- IMPORT-RANK  /rank 49  /module "Koshucode.Baala.Type.Rel"
|-- IMPORT-RANK  /rank 50  /module "Koshucode.Baala.Type"

|-- IMPORT-RANK  /rank 51  /module "Koshucode.Baala.Core.Relmap.Message"
|-- IMPORT-RANK  /rank 51  /module "Koshucode.Baala.Core.Relmap.Result"
|-- IMPORT-RANK  /rank 51  /module "Koshucode.Baala.Data.Church.Message"
|-- IMPORT-RANK  /rank 51  /module "Koshucode.Baala.Data.Class.Edge"
|-- IMPORT-RANK  /rank 51  /module "Koshucode.Baala.Data.Decode.Type"

|-- IMPORT-RANK  /rank 52  /module "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT-RANK  /rank 52  /module "Koshucode.Baala.Data.Class.Simple"
|-- IMPORT-RANK  /rank 52  /module "Koshucode.Baala.Data.Decode.Numeric"
|-- IMPORT-RANK  /rank 52  /module "Koshucode.Baala.Data.Message"
|-- IMPORT-RANK  /rank 53  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"

|-- IMPORT-RANK  /rank 53  /module "Koshucode.Baala.Core.Message"
|-- IMPORT-RANK  /rank 53  /module "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT-RANK  /rank 53  /module "Koshucode.Baala.Data.Class.Complex"
|-- IMPORT-RANK  /rank 54  /module "Koshucode.Baala.Core.Lexmap.Construct"
|-- IMPORT-RANK  /rank 54  /module "Koshucode.Baala.Data.Class.Content"
*** 175

|-- IMPORT-RANK  /rank 54  /module "Koshucode.Baala.Data.Class.Map"
|-- IMPORT-RANK  /rank 55  /module "Koshucode.Baala.Core.Lexmap"
|-- IMPORT-RANK  /rank 55  /module "Koshucode.Baala.Data.Class.Encode"
|-- IMPORT-RANK  /rank 56  /module "Koshucode.Baala.Core.Relkit.Relkit"
|-- IMPORT-RANK  /rank 56  /module "Koshucode.Baala.Core.Resource.Clause"

|-- IMPORT-RANK  /rank 56  /module "Koshucode.Baala.Data.Class"
|-- IMPORT-RANK  /rank 57  /module "Koshucode.Baala.Core.Relkit.Construct"
|-- IMPORT-RANK  /rank 57  /module "Koshucode.Baala.Data.Decode.Content"
|-- IMPORT-RANK  /rank 57  /module "Koshucode.Baala.Type.Type.Genus"
|-- IMPORT-RANK  /rank 58  /module "Koshucode.Baala.Data.Decode.Dataset"

|-- IMPORT-RANK  /rank 59  /module "Koshucode.Baala.Data.Decode"
|-- IMPORT-RANK  /rank 60  /module "Koshucode.Baala.Data.Church.Build"
|-- IMPORT-RANK  /rank 60  /module "Koshucode.Baala.Data.Content"
|-- IMPORT-RANK  /rank 61  /module "Koshucode.Baala.Data.Church.Run"
|-- IMPORT-RANK  /rank 62  /module "Koshucode.Baala.Data.Church"

|-- IMPORT-RANK  /rank 63  /module "Koshucode.Baala.Data"
|-- IMPORT-RANK  /rank 64  /module "Koshucode.Baala.Cop.Coxhand"
|-- IMPORT-RANK  /rank 64  /module "Koshucode.Baala.Core.Assert.RelTable"
|-- IMPORT-RANK  /rank 64  /module "Koshucode.Baala.Core.Relkit.Run"
|-- IMPORT-RANK  /rank 64  /module "Koshucode.Baala.Core.Relmap.Option"

|-- IMPORT-RANK  /rank 64  /module "Koshucode.Baala.DataPlus"
|-- IMPORT-RANK  /rank 64  /module "Koshucode.Baala.Toolkit.Library.RDF"
|-- IMPORT-RANK  /rank 64  /module "Koshucode.Baala.Type.Type.Predefined where"
|-- IMPORT-RANK  /rank 65  /module "Koshucode.Baala.Cop.Logic"
|-- IMPORT-RANK  /rank 65  /module "Koshucode.Baala.Core.Relkit"
*** 200

|-- IMPORT-RANK  /rank 65  /module "Koshucode.Baala.Rop.Flat.Applied.PoScale"
|-- IMPORT-RANK  /rank 65  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"
|-- IMPORT-RANK  /rank 66  /module "Koshucode.Baala.Core.Relmap.Relmap"
|-- IMPORT-RANK  /rank 67  /module "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT-RANK  /rank 67  /module "Koshucode.Baala.Core.Relmap.Specialize"

|-- IMPORT-RANK  /rank 68  /module "Koshucode.Baala.Core.Relmap.Construct"
|-- IMPORT-RANK  /rank 68  /module "Koshucode.Baala.Core.Relmap.Global"
|-- IMPORT-RANK  /rank 69  /module "Koshucode.Baala.Core.Relmap"
|-- IMPORT-RANK  /rank 70  /module "Koshucode.Baala.Core.Assert.Assert"
|-- IMPORT-RANK  /rank 71  /module "Koshucode.Baala.Core.Assert.Run"

|-- IMPORT-RANK  /rank 72  /module "Koshucode.Baala.Core.Assert"
|-- IMPORT-RANK  /rank 73  /module "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT-RANK  /rank 74  /module "Koshucode.Baala.Core.Resource.Concrete"
|-- IMPORT-RANK  /rank 74  /module "Koshucode.Baala.Core.Resource.Include"
|-- IMPORT-RANK  /rank 74  /module "Koshucode.Baala.Core.Resource.Run"

|-- IMPORT-RANK  /rank 75  /module "Koshucode.Baala.Core.Resource.Read"
|-- IMPORT-RANK  /rank 76  /module "Koshucode.Baala.Core.Resource"
|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Core"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Rop.Base.Message"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Toolkit.Library.Element"

|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Toolkit.Library.Input"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Writer.Csv"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Writer.Html"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Writer.Json"
*** 225

|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Writer.Judge"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Arith"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.List"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Misc"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Order"

|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Text"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Time"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Type"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Rop.Base.Define"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Rop.Base.Get.Get"

|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Writer.Koshu"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Cop.Bundle"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Base.Get.Rel"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Base.Rops"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Writer"

|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Cop"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Base.Get.Cox"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Toolkit.Library.Change"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Toolkit.Library.Run"
|-- IMPORT-RANK  /rank 82  /module "Koshucode.Baala.Rop.Base"

|-- IMPORT-RANK  /rank 82  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"
|-- IMPORT-RANK  /rank 82  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"
|-- IMPORT-RANK  /rank 82  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Cox.Calc"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Cox.Filter"
*** 250

|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Cox.Gadget"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Cox.Range"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Cox.Type.Clock"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Cox.Type.Dec"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Cox.Type.Text"

|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Cox.Type.Time"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Applied.Gadget"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Applied.Subtext"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Elem"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"

|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Meta"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Order"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Resource"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Source"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Term"

|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Nest.Flow"
|-- IMPORT-RANK  /rank 84  /module "Koshucode.Baala.Rop.Flat.Applied.Peripheral"
|-- IMPORT-RANK  /rank 84  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"
|-- IMPORT-RANK  /rank 84  /module "Koshucode.Baala.Rop.Flat.TermGadget"
|-- IMPORT-RANK  /rank 85  /module "Koshucode.Baala.Rop.Flat.Applied.Rops"

|-- IMPORT-RANK  /rank 85  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"
|-- IMPORT-RANK  /rank 86  /module "Koshucode.Baala.Rop.Flat.Applied"
|-- IMPORT-RANK  /rank 86  /module "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT-RANK  /rank 87  /module "Koshucode.Baala.Rop.Flat.Check"
|-- IMPORT-RANK  /rank 87  /module "Koshucode.Baala.Rop.Flat.Control"
*** 275

|-- IMPORT-RANK  /rank 88  /module "Koshucode.Baala.Rop.Flat.Rops"
|-- IMPORT-RANK  /rank 89  /module "Koshucode.Baala.Rop.Flat"
|-- IMPORT-RANK  /rank 90  /module "Koshucode.Baala.Rop.Cox.Empty"
|-- IMPORT-RANK  /rank 90  /module "Koshucode.Baala.Rop.Nest.Confl"
|-- IMPORT-RANK  /rank 91  /module "Koshucode.Baala.Rop.Cox.Rops"

|-- IMPORT-RANK  /rank 91  /module "Koshucode.Baala.Rop.Nest.Deriv"
|-- IMPORT-RANK  /rank 92  /module "Koshucode.Baala.Rop.Cox"
|-- IMPORT-RANK  /rank 92  /module "Koshucode.Baala.Rop.Nest.Rops"
|-- IMPORT-RANK  /rank 93  /module "Koshucode.Baala.Rop.Nest"
|-- IMPORT-RANK  /rank 94  /module "Koshucode.Baala.Toolkit.Library.Global"

*** 285 judges

**
**  SUMMARY
**     285 judges on IMPORT-RANK
**     285 judges in total
**
```



## command

This document is produced by the command:

```
koshu-inout.sh -o IMPORT-RANK.md ./import-rank.k ../base/data/IMPORT.k ../calculator/data/IMPORT.k ../cop/data/IMPORT.k ../core/data/IMPORT.k ../data-plus/data/IMPORT.k ../data/data/IMPORT.k ../overture/data/IMPORT.k ../rop-cox/data/IMPORT.k ../rop-flat/data/IMPORT.k ../rop-nested/data/IMPORT.k ../rop/data/IMPORT.k ../subtext/data/IMPORT.k ../syntax/data/IMPORT.k ../toolkit/data/IMPORT.k ../type/data/IMPORT.k ../writer/data/IMPORT.k
```

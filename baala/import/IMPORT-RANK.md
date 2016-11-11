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
- [../subtext/data/IMPORT.k](#subtextdataimportk)
- [../syntax/data/IMPORT.k](#syntaxdataimportk)
- [../toolkit/data/IMPORT.k](#toolkitdataimportk)
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
**    174 judges
**

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.Abort.Reason"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Message"        /import "Koshucode.Baala.Base.Abort"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Reason"         /import "Koshucode.Baala.Base.IO"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Base.IO.CodePt"            /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.IO.Http"              /import "Koshucode.Baala.Base.Prelude"

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
|-- IMPORT  /module "Koshucode.Baala.Base.List.Assoc"           /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Dots"            /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Dots"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Dots"            /import "Koshucode.Baala.Base.List.Split"

|-- IMPORT  /module "Koshucode.Baala.Base.List.List"            /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Order"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Order"           /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Order"           /import "Koshucode.Baala.Base.List.Snip"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Set"             /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Set"             /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Set"             /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.List.Snip"            /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.List.Snip"            /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Pair"         /import "Koshucode.Baala.Overture"

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
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Koshucode.Baala.Base.Syntax.Tree"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Data.ByteString.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Data.ByteString.Lazy.UTF8"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Line"          /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Message"       /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Message"       /import "Koshucode.Baala.Base.IO"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Scan"          /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Scan"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Scan"          /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Scan"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Scan"          /import "Koshucode.Baala.Base.Syntax.Line"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Comment"         /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.PPrint"          /import "Text.PrettyPrint"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.PPrint"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.PPrint"          /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Suffix"          /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Suffix"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Suffix"          /import "Koshucode.Baala.Base.List"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Comment"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.PPrint"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.TextTable"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Suffix"

|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.IO"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.List"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.MixText"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Syntax"
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
**    33 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Toolkit.Library.Run"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Writer"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Base.IO.SimpleOption"
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
**    58 judges
**

|-- IMPORT  /module "Koshucode.Baala.Cop.Arith"                 /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Arith"                 /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.Arith"                 /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Arith"                 /import "Koshucode.Baala.Cop.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Bundle"                /import "Koshucode.Baala.Base"
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
|-- IMPORT  /module "Koshucode.Baala.Cop.Coxhand"               /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Cop.Coxhand"               /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Cop.Replace"
|-- IMPORT  /module "Koshucode.Baala.Cop.List"                  /import "Koshucode.Baala.Cop.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Logic"                 /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Logic"                 /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Logic"                 /import "Koshucode.Baala.Cop.Coxhand"

|-- IMPORT  /module "Koshucode.Baala.Cop.Message"               /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Cop.Misc"                  /import "Koshucode.Baala.Cop.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Order"                 /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Order"                 /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Order"                 /import "Koshucode.Baala.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Cop.Order"                 /import "Koshucode.Baala.Cop.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Replace"               /import "Data.List"

|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Text"                  /import "Koshucode.Baala.Cop.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Time"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Cop.Time"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Time"                  /import "Koshucode.Baala.Cop.Message"

|-- IMPORT  /module "Koshucode.Baala.Cop.Type"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Cop.Type"                  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Cop.Type"                  /import "Koshucode.Baala.Cop.Message"

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
**    212 judges
**

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Message"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Message"       /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.LexmapTrees"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Syntax.TTree.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Core.Relkit.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Message"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Message"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Message"       /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Core.Lexmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Syntax"
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
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Data.Message"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Option"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Result"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Message"       /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Result"        /import "Koshucode.Baala.Data"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Relmap.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Overture"
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
**    4 judges
**

|-- IMPORT  /module "Koshucode.Baala.DataPlus"                  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.DataPlus"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.DataPlus"                  /import "Koshucode.Baala.Syntax"
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
**    220 judges
**

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Content"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Data.Church.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Build"         /import "Koshucode.Baala.Syntax.TTree.Pattern"

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
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Message"       /import "Koshucode.Baala.Syntax.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Church.Run"           /import "Koshucode.Baala.Data.Content"
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
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Data.Class.Singleton"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Complex"        /import "Koshucode.Baala.Data.Class.Simple"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Data.Class.Singleton"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Data.Class.Simple"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Content"        /import "Koshucode.Baala.Data.Class.Complex"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Message"        /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Data.Text"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Data.Text.Lazy"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Simple"         /import "Koshucode.Baala.Data.Class.Singleton"

|-- IMPORT  /module "Koshucode.Baala.Data.Class.Singleton"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Singleton"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Singleton"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Singleton"      /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Class.Singleton"      /import "Koshucode.Baala.Data.Class.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Complex"
|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Content"
|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Simple"
|-- IMPORT  /module "Koshucode.Baala.Data.Class"                /import "Koshucode.Baala.Data.Class.Singleton"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Content.Utility"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.BaalaC"       /import "Koshucode.Baala.Data.Class.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Term"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Tree"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Data.Content.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Decode"       /import "Koshucode.Baala.Syntax.TTree.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Message"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Message"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Message"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Message"      /import "Koshucode.Baala.Syntax.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Term"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Term"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Term"         /import "Koshucode.Baala.Data.Content.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Term"         /import "Koshucode.Baala.Syntax.TTree.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Data.Content.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Tree"         /import "Koshucode.Baala.Syntax.TTree.Pattern"

|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Data.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Content.Utility"      /import "Koshucode.Baala.Data.Class"

|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.BaalaC"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Decode"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Term"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Tree"
|-- IMPORT  /module "Koshucode.Baala.Data.Content"              /import "Koshucode.Baala.Data.Content.Utility"

|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Syntax.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Church.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Class.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Content.Message"
|-- IMPORT  /module "Koshucode.Baala.Data.Message"              /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Data.Type.Decimal.Coder"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Data.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Data.Type.Decimal.Fraction"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"  /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Data.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Data.Type.Decimal.Fraction"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Data.Type.Decimal.Rational"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Coder"   /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Decimal"  /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Decimal"  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Decimal"  /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Fraction"  /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Fraction"  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Fraction"  /import "Koshucode.Baala.Data.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Fraction"  /import "Koshucode.Baala.Data.Type.Decimal.Rational"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Rational"  /import "Data.Ratio"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal.Rational"  /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.BinaryAb"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.Coder"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.Fraction"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Decimal"         /import "Koshucode.Baala.Data.Type.Decimal.Rational"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.Interp"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.Interp"    /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.Interp"    /import "Koshucode.Baala.Data.Type.Judge.Judge"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.Interp"    /import "Koshucode.Baala.Data.Type.Judge.JudgeClass"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.Judge"     /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.Judge"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.Judge"     /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.Judge"     /import "Koshucode.Baala.Data.Type.Judge.JudgeClass"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.JudgeClass"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge.JudgeClass"  /import "Koshucode.Baala.Syntax"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge"           /import "Koshucode.Baala.Data.Type.Judge.Interp"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge"           /import "Koshucode.Baala.Data.Type.Judge.Judge"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Judge"           /import "Koshucode.Baala.Data.Type.Judge.JudgeClass"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Message"         /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Data.Type.Type"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Data.Type.Judge"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Head"        /import "Koshucode.Baala.Data.Type.Rel.TermPicker"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Mono"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Mono"        /import "Koshucode.Baala.Data.Type.Rel.Rel"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Rel"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Rel"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Rel"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Rel"         /import "Koshucode.Baala.Data.Type.Judge"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.Rel"         /import "Koshucode.Baala.Data.Type.Rel.Head"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.TermPicker"  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.TermPicker"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.TermPicker"  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel.TermPicker"  /import "Koshucode.Baala.Data.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Rel.Head"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Rel.Mono"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Rel.Rel"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Rel"             /import "Koshucode.Baala.Data.Type.Rel.TermPicker"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Clock"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Clock"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Clock"      /import "Koshucode.Baala.Data.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Clock"      /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.ClockCalc"  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.ClockCalc"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.ClockCalc"  /import "Koshucode.Baala.Data.Type.Time.Clock"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Data.Time.Calendar.OrdinalDate"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Date"       /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Data.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Data.Time.LocalTime"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Data.Type.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Data.Type.Time.ClockCalc"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Data.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.Time"       /import "Koshucode.Baala.Data.Type.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.TimeCalc"   /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.TimeCalc"   /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.TimeCalc"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.TimeCalc"   /import "Koshucode.Baala.Data.Type.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.TimeCalc"   /import "Koshucode.Baala.Data.Type.Time.ClockCalc"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.TimeCalc"   /import "Koshucode.Baala.Data.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.TimeCalc"   /import "Koshucode.Baala.Data.Type.Time.Time"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time.TimeCalc"   /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Time.Clock"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Time.ClockCalc"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Time.Date"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Time.Time"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Time"            /import "Koshucode.Baala.Data.Type.Time.TimeCalc"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type.TypeCalc"   /import "Koshucode.Baala.Data.Type.Type"

|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type"            /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Data.Type.Type"            /import "Koshucode.Baala.Data.Type.Judge"

|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Judge"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Rel"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Time"
|-- IMPORT  /module "Koshucode.Baala.Data.Type"                 /import "Koshucode.Baala.Data.Type.Type"

|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Church"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Class"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Content"
|-- IMPORT  /module "Koshucode.Baala.Data"                      /import "Koshucode.Baala.Data.Type"

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
**    17 judges
**

|-- IMPORT  /module "Koshucode.Baala.Overture.Fn"               /import "Koshucode.Baala.Overture.Name.String"



|-- IMPORT  /module "Koshucode.Baala.Overture.Name.Text"        /import "Data.Text"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Category"    /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Category"    /import "Data.Set"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Integer"     /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Integer"     /import "Data.IntMap.Strict"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Integer"     /import "Numeric"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Unicode"     /import "Data.Char"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Utility"     /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Utility"     /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text.Utility"     /import "Koshucode.Baala.Overture.Type"

|-- IMPORT  /module "Koshucode.Baala.Overture.Text"             /import "Koshucode.Baala.Overture.Text.Category"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text"             /import "Koshucode.Baala.Overture.Text.Integer"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text"             /import "Koshucode.Baala.Overture.Text.Unicode"
|-- IMPORT  /module "Koshucode.Baala.Overture.Text"             /import "Koshucode.Baala.Overture.Text.Utility"


|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Text"
|-- IMPORT  /module "Koshucode.Baala.Overture"                  /import "Koshucode.Baala.Overture.Type"

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
**    69 judges
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

|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Empty"             /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Cox.Gadget"            /import "Koshucode.Baala.Overture"
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
**    165 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Define"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Define"           /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Rop.Base.Get"              /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Control"          /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Flat.PoScale"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Flat.Subtext"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Gadget"           /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"  /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Message"          /import "Koshucode.Baala.Data.Message"
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
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.PoScale"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.PoScale"          /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Resource"         /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Source"           /import "Koshucode.Baala.Rop.Base"

|-- IMPORT  /module "Koshucode.Baala.Rop.Flat.Subtext"          /import "Koshucode.Baala.Overture"
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
**    37 judges
**

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Base"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Flat"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Nest.Flow"
|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Confl"            /import "Koshucode.Baala.Rop.Flat.Message"

|-- IMPORT  /module "Koshucode.Baala.Rop.Nest.Deriv"            /import "Koshucode.Baala.Overture"
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
**    52 judges
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
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Overture.Fn"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.Expr"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.MinMax"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.Operator"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Match"             /import "Koshucode.Baala.Subtext.Para"


|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Basic"    /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Basic"    /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Operator.Basic"    /import "Data.List"
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
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Overture.Fn"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Subtext.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Subtext.Expr"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Subtext.MinMax"
|-- IMPORT  /module "Koshucode.Baala.Subtext.Para"              /import "Koshucode.Baala.Subtext.Operator"

|-- IMPORT  /module "Koshucode.Baala.Subtext"                   /import "Koshucode.Baala.Subtext.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Subtext"                   /import "Koshucode.Baala.Subtext.Match"
|-- IMPORT  /module "Koshucode.Baala.Subtext"                   /import "Koshucode.Baala.Subtext.Operator"

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
**    133 judges
**

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Attr"          /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Parse"         /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Parse"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Parse"         /import "Koshucode.Baala.Syntax.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Parse"         /import "Koshucode.Baala.Syntax.Attr.Attr"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Parse"         /import "Koshucode.Baala.Syntax.Attr.AttrName"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Attr.Slot"          /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Para"          /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.Para"          /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.ParaSpec"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.ParaSpec"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para.ParaSpec"      /import "Koshucode.Baala.Syntax.Para.Para"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.Get"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.Para"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Para"               /import "Koshucode.Baala.Syntax.Para.ParaSpec"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.AngleText"   /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Message"     /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Next"        /import "Koshucode.Baala.Syntax.Symbol.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Short"       /import "Koshucode.Baala.Syntax.Symbol.Next"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol.Term"        /import "Koshucode.Baala.Overture"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Next"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Short"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Symbol"             /import "Koshucode.Baala.Syntax.Symbol.Term"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Nipper"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Rel"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Section"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Syntax.Token.Utility"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Clause"       /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Message"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Message"      /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Symbol.Message"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Nipper"       /import "Koshucode.Baala.Syntax.Token.Message"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Pattern"      /import "Koshucode.Baala.Syntax.Token.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Rel"          /import "Koshucode.Baala.Overture"
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

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Utility"      /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Utility"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token.Utility"      /import "Koshucode.Baala.Syntax.Token.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Clause"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Syntax.Token"              /import "Koshucode.Baala.Syntax.Token.Utility"

|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Bracket"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Bracket"      /import "Koshucode.Baala.Syntax.Token"

|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Parse"        /import "Text.PrettyPrint"
|-- IMPORT  /module "Koshucode.Baala.Syntax.TTree.Parse"        /import "Koshucode.Baala.Overture"
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
**    33 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Writer"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Toolkit.Library.Input"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Data"
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
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.Version"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Toolkit.Library.Version"

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
**    44 judges
**

|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Writer.Csv"                /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Writer.Html"               /import "Koshucode.Baala.Overture"
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
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Overture"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Data"
|-- IMPORT  /module "Koshucode.Baala.Writer.Judge"              /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Writer.Koshu"              /import "Koshucode.Baala.Overture"
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



## output


Command `./import-rank.k ../base/data/IMPORT.k ../calculator/data/IMPORT.k ../cop/data/IMPORT.k ../core/data/IMPORT.k ../data-plus/data/IMPORT.k ../data/data/IMPORT.k ../overture/data/IMPORT.k ../rop-cox/data/IMPORT.k ../rop-flat/data/IMPORT.k ../rop-nested/data/IMPORT.k ../subtext/data/IMPORT.k ../syntax/data/IMPORT.k ../toolkit/data/IMPORT.k ../writer/data/IMPORT.k` produces:

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
**    ../subtext/data/IMPORT.k
**    ../syntax/data/IMPORT.k
**    ../toolkit/data/IMPORT.k
**    ../writer/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.IO.SimpleOption"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.List.Split"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.MixText.LineBreak"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.Prelude.Case"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.Prelude.Class"

|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Base.Prelude.Import"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Cop.Replace"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Name.String"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Text.Category"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Text.Integer"

|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Text.Unicode"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Overture.Type"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Rop.Cox.GeoDatumJp"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Subtext.MinMax"
|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Syntax.Attr.AttrName"

|-- IMPORT-RANK  /rank 0  /module "Koshucode.Baala.Toolkit.Library.Version"
|-- IMPORT-RANK  /rank 1  /module "Koshucode.Baala.Base.Prelude.Queue"
|-- IMPORT-RANK  /rank 1  /module "Koshucode.Baala.Overture.Fn"
|-- IMPORT-RANK  /rank 1  /module "Koshucode.Baala.Overture.Text.Utility"
|-- IMPORT-RANK  /rank 2  /module "Koshucode.Baala.Overture.Text"

|-- IMPORT-RANK  /rank 2  /module "Koshucode.Baala.Subtext.Expr"
|-- IMPORT-RANK  /rank 3  /module "Koshucode.Baala.Overture"
|-- IMPORT-RANK  /rank 3  /module "Koshucode.Baala.Subtext.Operator.Basic"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Base.List.Assoc"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Base.List.Dots"
*** 25

|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Base.List.List"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Base.List.Set"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Base.MixText.MixText"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Base.Prelude.Pair"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Base.Text.Comment"

|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Base.Text.TextTable"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Data.Type.Decimal.Rational"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Subtext.Operator.Combine"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Syntax.Symbol.AngleText"
|-- IMPORT-RANK  /rank 4  /module "Koshucode.Baala.Syntax.Symbol.Term"

|-- IMPORT-RANK  /rank 5  /module "Koshucode.Baala.Base.MixText.MixClass"
|-- IMPORT-RANK  /rank 5  /module "Koshucode.Baala.Base.MixText.MixEncode"
|-- IMPORT-RANK  /rank 5  /module "Koshucode.Baala.Base.Prelude"
|-- IMPORT-RANK  /rank 5  /module "Koshucode.Baala.Subtext.Operator.Repeat"
|-- IMPORT-RANK  /rank 6  /module "Koshucode.Baala.Base.IO.BzFile"

|-- IMPORT-RANK  /rank 6  /module "Koshucode.Baala.Base.IO.Exit"
|-- IMPORT-RANK  /rank 6  /module "Koshucode.Baala.Base.IO.Http"
|-- IMPORT-RANK  /rank 6  /module "Koshucode.Baala.Base.List.Snip"
|-- IMPORT-RANK  /rank 6  /module "Koshucode.Baala.Base.MixText.Deriv"
|-- IMPORT-RANK  /rank 6  /module "Koshucode.Baala.Base.Text.PPrint"

|-- IMPORT-RANK  /rank 6  /module "Koshucode.Baala.Subtext.Operator.Char"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.List.Order"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Base.MixText"
|-- IMPORT-RANK  /rank 7  /module "Koshucode.Baala.Subtext.Operator"
|-- IMPORT-RANK  /rank 8  /module "Koshucode.Baala.Base.List"
*** 50

|-- IMPORT-RANK  /rank 8  /module "Koshucode.Baala.Subtext.Bundle"
|-- IMPORT-RANK  /rank 9  /module "Koshucode.Baala.Base.IO.IOPoint"
|-- IMPORT-RANK  /rank 9  /module "Koshucode.Baala.Base.Text.Suffix"
|-- IMPORT-RANK  /rank 9  /module "Koshucode.Baala.Subtext.Para"
|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Base.IO.CodePt"

|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Base.Text"
|-- IMPORT-RANK  /rank 10  /module "Koshucode.Baala.Subtext.Match"
|-- IMPORT-RANK  /rank 11  /module "Koshucode.Baala.Base.IO"
|-- IMPORT-RANK  /rank 11  /module "Koshucode.Baala.Subtext"
|-- IMPORT-RANK  /rank 12  /module "Koshucode.Baala.Base.Abort.Reason"

|-- IMPORT-RANK  /rank 12  /module "Koshucode.Baala.Base.Syntax.Line"
|-- IMPORT-RANK  /rank 13  /module "Koshucode.Baala.Base.Abort.Abortable"
|-- IMPORT-RANK  /rank 13  /module "Koshucode.Baala.Base.Abort.Report"
|-- IMPORT-RANK  /rank 13  /module "Koshucode.Baala.Base.Syntax.Clause"
|-- IMPORT-RANK  /rank 13  /module "Koshucode.Baala.Base.Syntax.Scan"

|-- IMPORT-RANK  /rank 14  /module "Koshucode.Baala.Base.Abort"
|-- IMPORT-RANK  /rank 15  /module "Koshucode.Baala.Base.Abort.Message"
|-- IMPORT-RANK  /rank 15  /module "Koshucode.Baala.Base.Syntax.Message"
|-- IMPORT-RANK  /rank 16  /module "Koshucode.Baala.Base.Message"
|-- IMPORT-RANK  /rank 16  /module "Koshucode.Baala.Base.Syntax.Tree"

|-- IMPORT-RANK  /rank 17  /module "Koshucode.Baala.Base.Syntax.Infix"
|-- IMPORT-RANK  /rank 18  /module "Koshucode.Baala.Base.Syntax"
|-- IMPORT-RANK  /rank 19  /module "Koshucode.Baala.Base"
|-- IMPORT-RANK  /rank 20  /module "Koshucode.Baala.Core.Resource.Message"
|-- IMPORT-RANK  /rank 20  /module "Koshucode.Baala.Data.Class.Message"
*** 75

|-- IMPORT-RANK  /rank 20  /module "Koshucode.Baala.Data.Type.Decimal.Decimal"
|-- IMPORT-RANK  /rank 20  /module "Koshucode.Baala.Data.Type.Message"
|-- IMPORT-RANK  /rank 20  /module "Koshucode.Baala.Rop.Flat.PoScale"
|-- IMPORT-RANK  /rank 20  /module "Koshucode.Baala.Syntax.Para.Para"
|-- IMPORT-RANK  /rank 20  /module "Koshucode.Baala.Syntax.Symbol.Message"

|-- IMPORT-RANK  /rank 20  /module "Koshucode.Baala.Syntax.Token.Message"
|-- IMPORT-RANK  /rank 21  /module "Koshucode.Baala.Data.Type.Decimal.Fraction"
|-- IMPORT-RANK  /rank 21  /module "Koshucode.Baala.Data.Type.Time.Date"
|-- IMPORT-RANK  /rank 21  /module "Koshucode.Baala.Syntax.Para.Get"
|-- IMPORT-RANK  /rank 21  /module "Koshucode.Baala.Syntax.Para.ParaSpec"

|-- IMPORT-RANK  /rank 21  /module "Koshucode.Baala.Syntax.Symbol.Next"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Data.Type.Decimal.Coder"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Data.Type.Time.Clock"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Syntax.Para"
|-- IMPORT-RANK  /rank 22  /module "Koshucode.Baala.Syntax.Symbol.Short"

|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Data.Type.Decimal.BinaryAb"
|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Data.Type.Time.ClockCalc"
|-- IMPORT-RANK  /rank 23  /module "Koshucode.Baala.Syntax.Symbol"
|-- IMPORT-RANK  /rank 24  /module "Koshucode.Baala.Data.Type.Decimal"
|-- IMPORT-RANK  /rank 24  /module "Koshucode.Baala.Data.Type.Time.Time"

|-- IMPORT-RANK  /rank 24  /module "Koshucode.Baala.Syntax.Token.Token"
|-- IMPORT-RANK  /rank 25  /module "Koshucode.Baala.Data.Type.Time.TimeCalc"
|-- IMPORT-RANK  /rank 25  /module "Koshucode.Baala.Syntax.Token.Pattern"
|-- IMPORT-RANK  /rank 25  /module "Koshucode.Baala.Syntax.Token.Utility"
|-- IMPORT-RANK  /rank 26  /module "Koshucode.Baala.Data.Type.Time"
*** 100

|-- IMPORT-RANK  /rank 26  /module "Koshucode.Baala.Syntax.Token.Nipper"
|-- IMPORT-RANK  /rank 27  /module "Koshucode.Baala.Syntax.Token.Section"
|-- IMPORT-RANK  /rank 28  /module "Koshucode.Baala.Syntax.Token.Rel"
|-- IMPORT-RANK  /rank 29  /module "Koshucode.Baala.Syntax.Token.Clause"
|-- IMPORT-RANK  /rank 30  /module "Koshucode.Baala.Syntax.Token"

|-- IMPORT-RANK  /rank 31  /module "Koshucode.Baala.Syntax.TTree.Bracket"
|-- IMPORT-RANK  /rank 31  /module "Koshucode.Baala.Syntax.TTree.Pattern"
|-- IMPORT-RANK  /rank 32  /module "Koshucode.Baala.Syntax.TTree.TokenTree"
|-- IMPORT-RANK  /rank 33  /module "Koshucode.Baala.Syntax.TTree.Parse"
|-- IMPORT-RANK  /rank 34  /module "Koshucode.Baala.Syntax.TTree"

|-- IMPORT-RANK  /rank 35  /module "Koshucode.Baala.Syntax.Attr.Message"
|-- IMPORT-RANK  /rank 36  /module "Koshucode.Baala.Syntax.Attr.Attr"
|-- IMPORT-RANK  /rank 36  /module "Koshucode.Baala.Syntax.Attr.Slot"
|-- IMPORT-RANK  /rank 36  /module "Koshucode.Baala.Syntax.Message"
|-- IMPORT-RANK  /rank 37  /module "Koshucode.Baala.Syntax.Attr.AttrEd"

|-- IMPORT-RANK  /rank 37  /module "Koshucode.Baala.Syntax.Attr.Parse"
|-- IMPORT-RANK  /rank 38  /module "Koshucode.Baala.Syntax.Attr"
|-- IMPORT-RANK  /rank 39  /module "Koshucode.Baala.Syntax"
|-- IMPORT-RANK  /rank 40  /module "Koshucode.Baala.Core.Assert.Message"
|-- IMPORT-RANK  /rank 40  /module "Koshucode.Baala.Core.Lexmap.Lexmap"

|-- IMPORT-RANK  /rank 40  /module "Koshucode.Baala.Core.Relkit.Message"
|-- IMPORT-RANK  /rank 40  /module "Koshucode.Baala.Data.Content.Message"
|-- IMPORT-RANK  /rank 40  /module "Koshucode.Baala.Data.Type.Judge.JudgeClass"
|-- IMPORT-RANK  /rank 41  /module "Koshucode.Baala.Data.Content.Term"
|-- IMPORT-RANK  /rank 41  /module "Koshucode.Baala.Data.Type.Judge.Judge"
*** 125

|-- IMPORT-RANK  /rank 42  /module "Koshucode.Baala.Data.Type.Judge.Interp"
|-- IMPORT-RANK  /rank 43  /module "Koshucode.Baala.Data.Type.Judge"
|-- IMPORT-RANK  /rank 44  /module "Koshucode.Baala.Data.Type.Rel.TermPicker"
|-- IMPORT-RANK  /rank 44  /module "Koshucode.Baala.Data.Type.Type"
|-- IMPORT-RANK  /rank 45  /module "Koshucode.Baala.Data.Type.Rel.Head"

|-- IMPORT-RANK  /rank 45  /module "Koshucode.Baala.Data.Type.Type.TypeCalc"
|-- IMPORT-RANK  /rank 46  /module "Koshucode.Baala.Data.Type.Rel.Rel"
|-- IMPORT-RANK  /rank 47  /module "Koshucode.Baala.Data.Type.Rel.Mono"
|-- IMPORT-RANK  /rank 48  /module "Koshucode.Baala.Data.Type.Rel"
|-- IMPORT-RANK  /rank 49  /module "Koshucode.Baala.Data.Type"

|-- IMPORT-RANK  /rank 50  /module "Koshucode.Baala.Data.Church.Message"
|-- IMPORT-RANK  /rank 50  /module "Koshucode.Baala.Data.Class.Singleton"
|-- IMPORT-RANK  /rank 50  /module "Koshucode.Baala.Data.Content.Tree"
|-- IMPORT-RANK  /rank 51  /module "Koshucode.Baala.Data.Church.Cox"
|-- IMPORT-RANK  /rank 51  /module "Koshucode.Baala.Data.Class.Simple"

|-- IMPORT-RANK  /rank 51  /module "Koshucode.Baala.Data.Message"
|-- IMPORT-RANK  /rank 52  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"
|-- IMPORT-RANK  /rank 52  /module "Koshucode.Baala.Core.Lexmap.Message"
|-- IMPORT-RANK  /rank 52  /module "Koshucode.Baala.Data.Church.Cop"
|-- IMPORT-RANK  /rank 52  /module "Koshucode.Baala.Data.Class.Complex"

|-- IMPORT-RANK  /rank 53  /module "Koshucode.Baala.Core.Lexmap.Construct"
|-- IMPORT-RANK  /rank 53  /module "Koshucode.Baala.Data.Class.Content"
|-- IMPORT-RANK  /rank 54  /module "Koshucode.Baala.Core.Lexmap"
|-- IMPORT-RANK  /rank 54  /module "Koshucode.Baala.Data.Class"
|-- IMPORT-RANK  /rank 55  /module "Koshucode.Baala.Data.Content.Decode"
*** 150

|-- IMPORT-RANK  /rank 55  /module "Koshucode.Baala.Data.Content.Utility"
|-- IMPORT-RANK  /rank 56  /module "Koshucode.Baala.Data.Content.BaalaC"
|-- IMPORT-RANK  /rank 57  /module "Koshucode.Baala.Data.Content"
|-- IMPORT-RANK  /rank 58  /module "Koshucode.Baala.Data.Church.Build"
|-- IMPORT-RANK  /rank 59  /module "Koshucode.Baala.Data.Church.Run"

|-- IMPORT-RANK  /rank 60  /module "Koshucode.Baala.Data.Church"
|-- IMPORT-RANK  /rank 61  /module "Koshucode.Baala.Data"
|-- IMPORT-RANK  /rank 62  /module "Koshucode.Baala.Cop.Coxhand"
|-- IMPORT-RANK  /rank 62  /module "Koshucode.Baala.Core.Assert.RelTable"
|-- IMPORT-RANK  /rank 62  /module "Koshucode.Baala.Core.Relkit.Relkit"

|-- IMPORT-RANK  /rank 62  /module "Koshucode.Baala.Core.Relmap.Message"
|-- IMPORT-RANK  /rank 62  /module "Koshucode.Baala.Core.Relmap.Result"
|-- IMPORT-RANK  /rank 62  /module "Koshucode.Baala.Core.Resource.Clause"
|-- IMPORT-RANK  /rank 62  /module "Koshucode.Baala.DataPlus"
|-- IMPORT-RANK  /rank 62  /module "Koshucode.Baala.Toolkit.Library.RDF"

|-- IMPORT-RANK  /rank 63  /module "Koshucode.Baala.Cop.Logic"
|-- IMPORT-RANK  /rank 63  /module "Koshucode.Baala.Core.Message"
|-- IMPORT-RANK  /rank 63  /module "Koshucode.Baala.Core.Relkit.Construct"
|-- IMPORT-RANK  /rank 63  /module "Koshucode.Baala.Core.Relkit.Run"
|-- IMPORT-RANK  /rank 63  /module "Koshucode.Baala.Core.Relmap.Option"

|-- IMPORT-RANK  /rank 63  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"
|-- IMPORT-RANK  /rank 64  /module "Koshucode.Baala.Core.Relkit"
|-- IMPORT-RANK  /rank 64  /module "Koshucode.Baala.Rop.Base.Message"
|-- IMPORT-RANK  /rank 65  /module "Koshucode.Baala.Core.Assert.Dataset"
|-- IMPORT-RANK  /rank 65  /module "Koshucode.Baala.Core.Relmap.Relmap"
*** 175

|-- IMPORT-RANK  /rank 65  /module "Koshucode.Baala.Rop.Base.Term"
|-- IMPORT-RANK  /rank 66  /module "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT-RANK  /rank 66  /module "Koshucode.Baala.Core.Relmap.Specialize"
|-- IMPORT-RANK  /rank 67  /module "Koshucode.Baala.Core.Relmap.Construct"
|-- IMPORT-RANK  /rank 67  /module "Koshucode.Baala.Core.Relmap.Global"

|-- IMPORT-RANK  /rank 68  /module "Koshucode.Baala.Core.Relmap"
|-- IMPORT-RANK  /rank 69  /module "Koshucode.Baala.Core.Assert.Assert"
|-- IMPORT-RANK  /rank 70  /module "Koshucode.Baala.Core.Assert.Run"
|-- IMPORT-RANK  /rank 71  /module "Koshucode.Baala.Core.Assert"
|-- IMPORT-RANK  /rank 72  /module "Koshucode.Baala.Core.Resource.Resource"

|-- IMPORT-RANK  /rank 73  /module "Koshucode.Baala.Core.Resource.Concrete"
|-- IMPORT-RANK  /rank 73  /module "Koshucode.Baala.Core.Resource.Include"
|-- IMPORT-RANK  /rank 73  /module "Koshucode.Baala.Core.Resource.Run"
|-- IMPORT-RANK  /rank 74  /module "Koshucode.Baala.Core.Resource.Read"
|-- IMPORT-RANK  /rank 75  /module "Koshucode.Baala.Core.Resource"

|-- IMPORT-RANK  /rank 76  /module "Koshucode.Baala.Core"
|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Rop.Base.Define"
|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Rop.Base.Get"
|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Rop.Flat.Message"
|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Toolkit.Library.Element"

|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Toolkit.Library.Input"
|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"
|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Writer.Csv"
|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Writer.Html"
|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Writer.Json"
*** 200

|-- IMPORT-RANK  /rank 77  /module "Koshucode.Baala.Writer.Judge"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Cop.Message"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Rop.Base.Rop"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Rop.Cox.Message"
|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Rop.Nest.Message"

|-- IMPORT-RANK  /rank 78  /module "Koshucode.Baala.Writer.Koshu"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Arith"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.List"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Misc"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Order"

|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Text"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Time"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Cop.Type"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Rop.Base"
|-- IMPORT-RANK  /rank 79  /module "Koshucode.Baala.Writer"

|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Cop.Bundle"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Cox.Get"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Flat.Elem"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Flat.Lattice.Tropashko"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Flat.Meta"

|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Flat.Order"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Flat.Resource"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Flat.Source"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Flat.Subtext"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Flat.Term"
*** 225

|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Rop.Nest.Flow"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Toolkit.Library.Change"
|-- IMPORT-RANK  /rank 80  /module "Koshucode.Baala.Toolkit.Library.Run"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Cop"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Cox.Accessor"

|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Cox.Calc"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Cox.Filter"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Cox.Gadget"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Cox.Range"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Flat.Gadget"

|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Flat.Lattice.Restrict"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Flat.Peripheral"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Rop.Flat.TermGadget"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"
|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"

|-- IMPORT-RANK  /rank 81  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"
|-- IMPORT-RANK  /rank 82  /module "Koshucode.Baala.Rop.Flat.Lattice.Rop"
|-- IMPORT-RANK  /rank 83  /module "Koshucode.Baala.Rop.Flat.Lattice"
|-- IMPORT-RANK  /rank 84  /module "Koshucode.Baala.Rop.Flat.Check"
|-- IMPORT-RANK  /rank 84  /module "Koshucode.Baala.Rop.Flat.Control"

|-- IMPORT-RANK  /rank 85  /module "Koshucode.Baala.Rop.Flat.Bundle"
|-- IMPORT-RANK  /rank 86  /module "Koshucode.Baala.Rop.Flat"
|-- IMPORT-RANK  /rank 87  /module "Koshucode.Baala.Rop.Cox.Empty"
|-- IMPORT-RANK  /rank 87  /module "Koshucode.Baala.Rop.Nest.Confl"
|-- IMPORT-RANK  /rank 88  /module "Koshucode.Baala.Rop.Cox.Bundle"
*** 250

|-- IMPORT-RANK  /rank 88  /module "Koshucode.Baala.Rop.Nest.Deriv"
|-- IMPORT-RANK  /rank 89  /module "Koshucode.Baala.Rop.Cox"
|-- IMPORT-RANK  /rank 89  /module "Koshucode.Baala.Rop.Nest.Rop"
|-- IMPORT-RANK  /rank 90  /module "Koshucode.Baala.Rop.Nest"
|-- IMPORT-RANK  /rank 91  /module "Koshucode.Baala.Toolkit.Library.Global"

*** 255 judges

**
**  SUMMARY
**     255 judges on IMPORT-RANK
**     255 judges in total
**
```



## command

This document is produced by the command:

```
koshu-inout.sh -o IMPORT-RANK.md ./import-rank.k ../base/data/IMPORT.k ../calculator/data/IMPORT.k ../cop/data/IMPORT.k ../core/data/IMPORT.k ../data-plus/data/IMPORT.k ../data/data/IMPORT.k ../overture/data/IMPORT.k ../rop-cox/data/IMPORT.k ../rop-flat/data/IMPORT.k ../rop-nested/data/IMPORT.k ../subtext/data/IMPORT.k ../syntax/data/IMPORT.k ../toolkit/data/IMPORT.k ../writer/data/IMPORT.k
```

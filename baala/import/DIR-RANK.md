# I/O List

- [./dir-rank.k](#dir-rankk)
- ./dir-rank.k [../base/IMPORT.k](#baseimportk)
- ./dir-rank.k [../core/IMPORT.k](#coreimportk)
- ./dir-rank.k [../operator/IMPORT.k](#operatorimportk)
- ./dir-rank.k [../content/IMPORT.k](#contentimportk)
- ./dir-rank.k [../calculator/IMPORT.k](#calculatorimportk)



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
    | pick-group /dir -to /base-rank
    | for /base-rank ( pick /rank /base /import-dir )
    | add /dir-rank ( max /base-rank/rank )
    | interp <<< Module directory /dir has dependent rank /dir-rank ,
                 /base-rank exists under the /dir . >>>

dep : imp
    | dependent-rank /module /import -rank /rank
    | group ( imp | meet imp-dir ) -to /=import
    | add /import-dir ( sort /=import/dirname )
    | wipe
    | interp <<< /module has dependent rank /rank .
                 The module imports directory modules /import-dir . >>>

imp-dir : imp
    | pick /module
    | add /import   ( dir-part  <dot> /module )
    | add /dirname  ( base-part <dot> /import )
    | pick /import /dirname
    | interp <<< Module directory /import can be imported from other module.
                 The base part of /import is /dirname . >>>

imp : source IMPORT /module /import
    | keep part "Koshucode" /import
```



## ../base/IMPORT.k

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

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Abortable"      /import "Koshucode.Baala.Base.Abort.Reason"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Reason"         /import "Koshucode.Baala.Base.Text"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "System.Exit"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Abort.Reason"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort.Report"         /import "Koshucode.Baala.Base.Text"

|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Abortable"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Reason"
|-- IMPORT  /module "Koshucode.Baala.Base.Abort"                /import "Koshucode.Baala.Base.Abort.Report"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.About"           /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.About"           /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.About"           /import "Koshucode.Baala.Base.Data.Judge"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Clock"           /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Clock"           /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Clock"           /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Clock"           /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Date"            /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Date"            /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Date"            /import "Data.Time.Calendar.OrdinalDate"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Date"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Date"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Date"            /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Date"            /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Head"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Head"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Head"            /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Head"            /import "Koshucode.Baala.Base.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Head"            /import "Koshucode.Baala.Base.Data.Type"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Interp"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Interp"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Interp"          /import "Koshucode.Baala.Base.Token"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Data.Rel"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Data.Head"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Mono"            /import "Koshucode.Baala.Base.Data.Rel"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Mono"            /import "Koshucode.Baala.Base.Abort"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Koshucode.Baala.Base.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Koshucode.Baala.Base.Data.Judge"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Data.Head"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Time"            /import "Data.Time.Calendar"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Time"            /import "Data.Time.Calendar.WeekDate"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Time"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Time"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Time"            /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Time"            /import "Koshucode.Baala.Base.Data.Clock"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Time"            /import "Koshucode.Baala.Base.Data.Date"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Time"            /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Type"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Type"            /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Type"            /import "Koshucode.Baala.Base.Token"

|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.About"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Clock"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Date"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Head"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Interp"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Judge"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Mono"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Output"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Para"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Rel"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Time"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Type"

|-- IMPORT  /module "Koshucode.Baala.Base.Message"              /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Message"              /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Message"              /import "Koshucode.Baala.Base.Text"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Assoc"        /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Assoc"        /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Assoc"        /import "Koshucode.Baala.Base.Prelude.Class"


|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Tuple"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Data.Monoid"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Import"       /import "Text.PrettyPrint"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.List"         /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.List"         /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.List"         /import "Koshucode.Baala.Base.Prelude.Class"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Order"        /import "Koshucode.Baala.Base.Prelude.Class"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Order"        /import "Koshucode.Baala.Base.Prelude.Import"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Order"        /import "Koshucode.Baala.Base.Prelude.Snip"
|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Order"        /import "Koshucode.Baala.Base.Prelude.Pair"

|-- IMPORT  /module "Koshucode.Baala.Base.Prelude.Pair"         /import "Control.Applicative"
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

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Code"          /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Code"          /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Code"          /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Code"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Code"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Code"          /import "Koshucode.Baala.Base.Syntax.Line"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Code"          /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Infix"         /import "Koshucode.Baala.Base.Syntax.Tree"


|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax.Tree"          /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Code"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Infix"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Line"
|-- IMPORT  /module "Koshucode.Baala.Base.Syntax"               /import "Koshucode.Baala.Base.Syntax.Tree"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.CodePt"          /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.CodePt"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.CodePt"          /import "Koshucode.Baala.Base.Text.IOPoint"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Comment"         /import "System.IO"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Data.ByteString.Char8"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Data.ByteString.Lazy.Char8"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Control.Exception"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Network.HTTP.Conduit"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Network.HTTP.Types.Status"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Text.URI"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Http"            /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.IOPoint"         /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.IOPoint"         /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Unicode"         /import "Data.Char"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Data.List"
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

|-- IMPORT  /module "Koshucode.Baala.Base.Token.AngleText"      /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.AngleText"      /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.AngleText"      /import "Koshucode.Baala.Base.Text"

|-- IMPORT  /module "Koshucode.Baala.Base.Token.Short"          /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.Short"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.Short"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.Short"          /import "Koshucode.Baala.Base.Token.AngleText"

|-- IMPORT  /module "Koshucode.Baala.Base.Token.Token"          /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.Token"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.Token"          /import "Koshucode.Baala.Base.Text"

|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenClause"    /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenClause"    /import "Koshucode.Baala.Base.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenClause"    /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenClause"    /import "Koshucode.Baala.Base.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenClause"    /import "Koshucode.Baala.Base.Token.TokenLine"

|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Koshucode.Baala.Base.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Koshucode.Baala.Base.Token.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Koshucode.Baala.Base.Token.Short"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Koshucode.Baala.Base.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenLine"      /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenTree"      /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenTree"      /import "Text.PrettyPrint"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenTree"      /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenTree"      /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenTree"      /import "Koshucode.Baala.Base.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenTree"      /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenTree"      /import "Koshucode.Baala.Base.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Token.TokenTree"      /import "Koshucode.Baala.Base.Token.TokenLine"

|-- IMPORT  /module "Koshucode.Baala.Base.Token"                /import "Koshucode.Baala.Base.Token.AngleText"
|-- IMPORT  /module "Koshucode.Baala.Base.Token"                /import "Koshucode.Baala.Base.Token.Short"
|-- IMPORT  /module "Koshucode.Baala.Base.Token"                /import "Koshucode.Baala.Base.Token.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Token"                /import "Koshucode.Baala.Base.Token.TokenClause"
|-- IMPORT  /module "Koshucode.Baala.Base.Token"                /import "Koshucode.Baala.Base.Token.TokenLine"
|-- IMPORT  /module "Koshucode.Baala.Base.Token"                /import "Koshucode.Baala.Base.Token.TokenTree"

|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Data"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Syntax"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base"                      /import "Koshucode.Baala.Base.Token"

```

Command `./dir-rank.k ../base/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../base/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Base.Prelude  /base-rank {| /rank /base /import-dir [ 0 | 'Class | [ ] ] [ 0 | 'Import | [ ] ] [ 1 | 'Assoc | [ ] ] [ 1 | 'List | [ ] ] [ 1 | 'Pair | [ ] ] [ 1 | 'Snip | [ ] ] [ 2 | 'Order | [ ] ] |}
|-- DIR-RANK  /dir-rank 5  /dir 'Koshucode.Baala.Base.Text  /base-rank {| /rank /base /import-dir [ 0 | 'Comment | [ ] ] [ 0 | 'Unicode | [ ] ] [ 4 | 'Http | [ 'Prelude ] ] [ 4 | 'IOPoint | [ 'Prelude ] ] [ 4 | 'TextTable | [ 'Prelude ] ] [ 4 | 'Utility | [ 'Prelude ] ] [ 4 | 'Write | [ 'Prelude ] ] [ 5 | 'CodePt | [ 'Prelude ] ] |}
|-- DIR-RANK  /dir-rank 8  /dir 'Koshucode.Baala.Base.Abort  /base-rank {| /rank /base /import-dir [ 7 | 'Reason | [ 'Text ] ] [ 8 | 'Abortable | [ 'Prelude | 'Text ] ] [ 8 | 'Report | [ 'Prelude | 'Text ] ] |}
|-- DIR-RANK  /dir-rank 12  /dir 'Koshucode.Baala.Base.Syntax  /base-rank {| /rank /base /import-dir [ 0 | 'Line | [ ] ] [ 11 | 'Code | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Tree | [ 'Abort | 'Prelude | 'Text ] ] [ 12 | 'Infix | [ 'Prelude ] ] |}
|-- DIR-RANK  /dir-rank 15  /dir 'Koshucode.Baala.Base.Token  /base-rank {| /rank /base /import-dir [ 7 | 'AngleText | [ 'Prelude | 'Text ] ] [ 7 | 'Token | [ 'Prelude | 'Text ] ] [ 8 | 'Short | [ 'Prelude | 'Text ] ] [ 14 | 'TokenLine | [ 'Abort | 'Prelude | 'Syntax | 'Text ] ] [ 15 | 'TokenClause | [ 'Prelude | 'Syntax | 'Text ] ] [ 15 | 'TokenTree | [ 'Abort | 'Prelude | 'Syntax | 'Text ] ] |}

|-- DIR-RANK  /dir-rank 21  /dir 'Koshucode.Baala.Base.Data  /base-rank {| /rank /base /import-dir [ 11 | 'Clock | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Date | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Decimal | [ 'Abort | 'Prelude ] ] [ 11 | 'Para | [ 'Abort | 'Prelude ] ] [ 12 | 'Time | [ 'Abort | 'Prelude | 'Text ] ] [ 17 | 'Interp | [ 'Prelude | 'Text | 'Token ] ] [ 17 | 'Type | [ 'Prelude | 'Text | 'Token ] ] [ 18 | 'Head | [ 'Abort | 'Prelude | 'Text | 'Token ] ] [ 19 | 'Rel | [ 'Prelude | 'Text ] ] [ 20 | 'Judge | [ 'Abort | 'Prelude | 'Text | 'Token ] ] [ 20 | 'Mono | [ 'Abort ] ] [ 21 | 'About | [ 'Prelude | 'Text ] ] [ 21 | 'Output | [ 'Abort | 'Prelude | 'Text | 'Token ] ] |}
|-- DIR-RANK  /dir-rank 22  /dir 'Koshucode.Baala.Base  /base-rank {| /rank /base /import-dir [ 3 | 'Prelude | [ ] ] [ 6 | 'Text | [ ] ] [ 9 | 'Abort | [ ] ] [ 10 | 'Message | [ 'Abort | 'Prelude | 'Text ] ] [ 13 | 'Syntax | [ ] ] [ 16 | 'Token | [ ] ] [ 22 | 'Data | [ ] ] |}
|-- DIR-RANK  /dir-rank 23  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 23 | 'Base | [ 'Abort | 'Data | 'Prelude | 'Syntax | 'Text | 'Token ] ] |}

*** 8 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                            /base-rank
  --------- ------------------------------- -------------------------------------------------------------------------------
  2         'Koshucode.Baala.Base.Prelude   /rank /base          /import-dir
                                            ----- -------------- ----------------------------------------------------------
                                            0     'Class         [ ]
                                            0     'Import        [ ]
                                            1     'Assoc         [ ]
                                            1     'List          [ ]
                                            1     'Pair          [ ]
                                            1     'Snip          [ ]
                                            2     'Order         [ ]
                                            
  5         'Koshucode.Baala.Base.Text      /rank /base          /import-dir
                                            ----- -------------- ----------------------------------------------------------
                                            0     'Comment       [ ]
                                            0     'Unicode       [ ]
                                            4     'Http          [ 'Prelude ]
                                            4     'IOPoint       [ 'Prelude ]
                                            4     'TextTable     [ 'Prelude ]
                                            4     'Utility       [ 'Prelude ]
                                            4     'Write         [ 'Prelude ]
                                            5     'CodePt        [ 'Prelude ]
                                            
  8         'Koshucode.Baala.Base.Abort     /rank /base          /import-dir
                                            ----- -------------- ----------------------------------------------------------
                                            7     'Reason        [ 'Text ]
                                            8     'Abortable     [ 'Prelude | 'Text ]
                                            8     'Report        [ 'Prelude | 'Text ]
                                            
  12        'Koshucode.Baala.Base.Syntax    /rank /base          /import-dir
                                            ----- -------------- ----------------------------------------------------------
                                            0     'Line          [ ]
                                            11    'Code          [ 'Abort | 'Prelude | 'Text ]
                                            11    'Tree          [ 'Abort | 'Prelude | 'Text ]
                                            12    'Infix         [ 'Prelude ]
                                            
  15        'Koshucode.Baala.Base.Token     /rank /base          /import-dir
                                            ----- -------------- ----------------------------------------------------------
                                            7     'AngleText     [ 'Prelude | 'Text ]
                                            7     'Token         [ 'Prelude | 'Text ]
                                            8     'Short         [ 'Prelude | 'Text ]
                                            14    'TokenLine     [ 'Abort | 'Prelude | 'Syntax | 'Text ]
                                            15    'TokenClause   [ 'Prelude | 'Syntax | 'Text ]
                                            15    'TokenTree     [ 'Abort | 'Prelude | 'Syntax | 'Text ]
                                            
  21        'Koshucode.Baala.Base.Data      /rank /base          /import-dir
                                            ----- -------------- ----------------------------------------------------------
                                            11    'Clock         [ 'Abort | 'Prelude | 'Text ]
                                            11    'Date          [ 'Abort | 'Prelude | 'Text ]
                                            11    'Decimal       [ 'Abort | 'Prelude ]
                                            11    'Para          [ 'Abort | 'Prelude ]
                                            12    'Time          [ 'Abort | 'Prelude | 'Text ]
                                            17    'Interp        [ 'Prelude | 'Text | 'Token ]
                                            17    'Type          [ 'Prelude | 'Text | 'Token ]
                                            18    'Head          [ 'Abort | 'Prelude | 'Text | 'Token ]
                                            19    'Rel           [ 'Prelude | 'Text ]
                                            20    'Judge         [ 'Abort | 'Prelude | 'Text | 'Token ]
                                            20    'Mono          [ 'Abort ]
                                            21    'About         [ 'Prelude | 'Text ]
                                            21    'Output        [ 'Abort | 'Prelude | 'Text | 'Token ]
                                            
  22        'Koshucode.Baala.Base           /rank /base          /import-dir
                                            ----- -------------- ----------------------------------------------------------
                                            3     'Prelude       [ ]
                                            6     'Text          [ ]
                                            9     'Abort         [ ]
                                            10    'Message       [ 'Abort | 'Prelude | 'Text ]
                                            13    'Syntax        [ ]
                                            16    'Token         [ ]
                                            22    'Data          [ ]
                                            
  23        'Koshucode.Baala                /rank /base          /import-dir
                                            ----- -------------- ----------------------------------------------------------
                                            23    'Base          [ 'Abort | 'Data | 'Prelude | 'Syntax | 'Text | 'Token ]
                                            

=== rel

**
**  SUMMARY
**       8 judges on DIR-RANK
**       8 judges in total
**
```



## ../core/IMPORT.k

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
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Core.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Core.Content"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.RelTable"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.Dataset"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.RelTable"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert"               /import "Koshucode.Baala.Core.Assert.Run"

|-- IMPORT  /module "Koshucode.Baala.Core.Content.Build"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Build"        /import "Koshucode.Baala.Core.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Build"        /import "Koshucode.Baala.Core.Content.Cop"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Build"        /import "Koshucode.Baala.Core.Content.Cox"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Build"        /import "Koshucode.Baala.Core.Content.Literal"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Build"        /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Content.Class"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Class"        /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Content.Cop"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Cop"          /import "Koshucode.Baala.Core.Content.Cox"

|-- IMPORT  /module "Koshucode.Baala.Core.Content.Cox"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Cox"          /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Content.Literal"      /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Literal"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Literal"      /import "Koshucode.Baala.Core.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Literal"      /import "Koshucode.Baala.Core.Content.Tree"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Literal"      /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Build"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Cop"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Cox"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Literal"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Content.Tree"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Tree"         /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Build"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Cop"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Cox"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Literal"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Run"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Tree"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attr"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attr"          /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attr"          /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Core.Lexmap.Slot"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrEd"        /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrPos"       /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrPos"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.AttrPos"       /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.AttrEd"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Attr"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.LexmapTrees"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Lexmap.Slot"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Construct"     /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Core.Lexmap.Attr"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Core.Lexmap.AttrEd"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Core.Lexmap.Attr"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.LexmapTrees"   /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Attr"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.AttrEd"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.AttrPos"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.LexmapTrees"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Slot"

|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Construct"     /import "Koshucode.Baala.Core.Relkit.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Relkit"        /import "Koshucode.Baala.Core.Lexmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Relkit.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit.Run"           /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relkit"               /import "Koshucode.Baala.Core.Relkit.Run"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Option"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Rop"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Option"        /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relmap"        /import "Koshucode.Baala.Core.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Rop"           /import "Koshucode.Baala.Core.Relmap.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Relmap.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Global"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Option"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Rop"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Specialize"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Clause"      /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Include"     /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Quoter"      /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Quoter"      /import "Language.Haskell.TH"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Quoter"      /import "Language.Haskell.TH.Quote"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Quoter"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Quoter"      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Quoter"      /import "Koshucode.Baala.Core.Resource.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Quoter"      /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Quoter"      /import "Koshucode.Baala.Core.Resource.Include"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "System.Directory"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "System.FilePath"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Control.Monad.State"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Resource.Include"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Read"        /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Resource"    /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Resource.Resource"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource.Run"         /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Include"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Quoter"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Read"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Run"
|-- IMPORT  /module "Koshucode.Baala.Core.Resource"             /import "Koshucode.Baala.Core.Resource.Resource"

|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Resource"

```

Command `./dir-rank.k ../core/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../core/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala.Base  /base-rank {| /rank /base /import-dir [ 0 | 'Message | [ ] ] |}
|-- DIR-RANK  /dir-rank 5  /dir 'Koshucode.Baala.Core.Content  /base-rank {| /rank /base /import-dir [ 2 | 'Class | [ ] ] [ 2 | 'Cox | [ ] ] [ 2 | 'Tree | [ ] ] [ 3 | 'Cop | [ ] ] [ 3 | 'Literal | [ ] ] [ 4 | 'Build | [ ] ] [ 5 | 'Run | [ ] ] |}
|-- DIR-RANK  /dir-rank 6  /dir 'Koshucode.Baala.Core.Lexmap  /base-rank {| /rank /base /import-dir [ 2 | 'AttrPos | [ ] ] [ 3 | 'Attr | [ ] ] [ 3 | 'Slot | [ ] ] [ 4 | 'AttrEd | [ ] ] [ 4 | 'Lexmap | [ ] ] [ 5 | 'LexmapTrees | [ ] ] [ 6 | 'Construct | [ ] ] |}
|-- DIR-RANK  /dir-rank 10  /dir 'Koshucode.Baala.Core.Relkit  /base-rank {| /rank /base /import-dir [ 8 | 'Relkit | [ 'Lexmap ] ] [ 9 | 'Construct | [ ] ] [ 10 | 'Run | [ 'Content ] ] |}
|-- DIR-RANK  /dir-rank 14  /dir 'Koshucode.Baala.Core.Relmap  /base-rank {| /rank /base /import-dir [ 7 | 'Option | [ 'Content ] ] [ 12 | 'Relmap | [ 'Lexmap | 'Relkit ] ] [ 13 | 'Rop | [ 'Lexmap ] ] [ 13 | 'Specialize | [ 'Lexmap | 'Relkit ] ] [ 14 | 'Construct | [ 'Lexmap | 'Relkit ] ] [ 14 | 'Global | [ 'Content ] ] |}

|-- DIR-RANK  /dir-rank 17  /dir 'Koshucode.Baala.Core.Assert  /base-rank {| /rank /base /import-dir [ 7 | 'RelTable | [ 'Content ] ] [ 12 | 'Dataset | [ 'Content | 'Relkit ] ] [ 16 | 'Assert | [ 'Content | 'Lexmap | 'Relmap ] ] [ 17 | 'Run | [ 'Content | 'Lexmap | 'Relkit | 'Relmap ] ] |}
|-- DIR-RANK  /dir-rank 21  /dir 'Koshucode.Baala.Core.Resource  /base-rank {| /rank /base /import-dir [ 8 | 'Clause | [ 'Content | 'Lexmap ] ] [ 19 | 'Resource | [ 'Assert | 'Content | 'Lexmap | 'Relkit | 'Relmap ] ] [ 20 | 'Include | [ 'Assert | 'Content | 'Lexmap | 'Relmap ] ] [ 20 | 'Run | [ 'Assert | 'Content | 'Lexmap | 'Relmap ] ] [ 21 | 'Quoter | [ 'Lexmap ] ] [ 21 | 'Read | [ 'Content | 'Relmap ] ] |}
|-- DIR-RANK  /dir-rank 22  /dir 'Koshucode.Baala.Core  /base-rank {| /rank /base /import-dir [ 1 | 'Message | [ ] ] [ 6 | 'Content | [ ] ] [ 7 | 'Lexmap | [ ] ] [ 11 | 'Relkit | [ ] ] [ 15 | 'Relmap | [ ] ] [ 18 | 'Assert | [ ] ] [ 22 | 'Resource | [ ] ] |}
|-- DIR-RANK  /dir-rank 23  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 0 | 'Base | [ ] ] [ 23 | 'Core | [ 'Assert | 'Content | 'Lexmap | 'Relkit | 'Relmap | 'Resource ] ] |}

*** 9 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                             /base-rank
  --------- -------------------------------- ---------------------------------------------------------------------------------------
  0         'Koshucode.Baala.Base            /rank /base          /import-dir
                                             ----- -------------- ------------------------------------------------------------------
                                             0     'Message       [ ]
                                             
  5         'Koshucode.Baala.Core.Content    /rank /base          /import-dir
                                             ----- -------------- ------------------------------------------------------------------
                                             2     'Class         [ ]
                                             2     'Cox           [ ]
                                             2     'Tree          [ ]
                                             3     'Cop           [ ]
                                             3     'Literal       [ ]
                                             4     'Build         [ ]
                                             5     'Run           [ ]
                                             
  6         'Koshucode.Baala.Core.Lexmap     /rank /base          /import-dir
                                             ----- -------------- ------------------------------------------------------------------
                                             2     'AttrPos       [ ]
                                             3     'Attr          [ ]
                                             3     'Slot          [ ]
                                             4     'AttrEd        [ ]
                                             4     'Lexmap        [ ]
                                             5     'LexmapTrees   [ ]
                                             6     'Construct     [ ]
                                             
  10        'Koshucode.Baala.Core.Relkit     /rank /base          /import-dir
                                             ----- -------------- ------------------------------------------------------------------
                                             8     'Relkit        [ 'Lexmap ]
                                             9     'Construct     [ ]
                                             10    'Run           [ 'Content ]
                                             
  14        'Koshucode.Baala.Core.Relmap     /rank /base          /import-dir
                                             ----- -------------- ------------------------------------------------------------------
                                             7     'Option        [ 'Content ]
                                             12    'Relmap        [ 'Lexmap | 'Relkit ]
                                             13    'Rop           [ 'Lexmap ]
                                             13    'Specialize    [ 'Lexmap | 'Relkit ]
                                             14    'Construct     [ 'Lexmap | 'Relkit ]
                                             14    'Global        [ 'Content ]
                                             
  17        'Koshucode.Baala.Core.Assert     /rank /base          /import-dir
                                             ----- -------------- ------------------------------------------------------------------
                                             7     'RelTable      [ 'Content ]
                                             12    'Dataset       [ 'Content | 'Relkit ]
                                             16    'Assert        [ 'Content | 'Lexmap | 'Relmap ]
                                             17    'Run           [ 'Content | 'Lexmap | 'Relkit | 'Relmap ]
                                             
  21        'Koshucode.Baala.Core.Resource   /rank /base          /import-dir
                                             ----- -------------- ------------------------------------------------------------------
                                             8     'Clause        [ 'Content | 'Lexmap ]
                                             19    'Resource      [ 'Assert | 'Content | 'Lexmap | 'Relkit | 'Relmap ]
                                             20    'Include       [ 'Assert | 'Content | 'Lexmap | 'Relmap ]
                                             20    'Run           [ 'Assert | 'Content | 'Lexmap | 'Relmap ]
                                             21    'Quoter        [ 'Lexmap ]
                                             21    'Read          [ 'Content | 'Relmap ]
                                             
  22        'Koshucode.Baala.Core            /rank /base          /import-dir
                                             ----- -------------- ------------------------------------------------------------------
                                             1     'Message       [ ]
                                             6     'Content       [ ]
                                             7     'Lexmap        [ ]
                                             11    'Relkit        [ ]
                                             15    'Relmap        [ ]
                                             18    'Assert        [ ]
                                             22    'Resource      [ ]
                                             
  23        'Koshucode.Baala                 /rank /base          /import-dir
                                             ----- -------------- ------------------------------------------------------------------
                                             0     'Base          [ ]
                                             23    'Core          [ 'Assert | 'Content | 'Lexmap | 'Relkit | 'Relmap | 'Resource ]
                                             

=== rel

**
**  SUMMARY
**       9 judges on DIR-RANK
**       9 judges in total
**
```



## ../operator/IMPORT.k

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
**    116 judges
**

|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Define"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Define"         /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Get"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Get"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Get"            /import "Koshucode.Baala.Op.Builtin.Term"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Get"            /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Rop"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Rop"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Rop"            /import "Koshucode.Baala.Op.Builtin.Define"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Rop"            /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Term"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin.Term"           /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Builtin"                /import "Koshucode.Baala.Op.Builtin.Define"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin"                /import "Koshucode.Baala.Op.Builtin.Get"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin"                /import "Koshucode.Baala.Op.Builtin.Rop"
|-- IMPORT  /module "Koshucode.Baala.Op.Builtin"                /import "Koshucode.Baala.Op.Builtin.Term"

|-- IMPORT  /module "Koshucode.Baala.Op.Check"                  /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Op.Check"                  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Check"                  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Check"                  /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Check"                  /import "Koshucode.Baala.Op.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Op.Check"                  /import "Koshucode.Baala.Op.Term"
|-- IMPORT  /module "Koshucode.Baala.Op.Check"                  /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Control"                /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Control"                /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Control"                /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Control"                /import "Koshucode.Baala.Op.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Op.Control"                /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.DepRank"                /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Op.DepRank"                /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Op.DepRank"
|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Restrict"       /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Restrict"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Restrict"       /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Restrict"       /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Restrict"       /import "Koshucode.Baala.Op.Lattice.Tropashko"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Restrict"       /import "Koshucode.Baala.Op.Term"

|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Rop"            /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Rop"            /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Rop"            /import "Koshucode.Baala.Op.Lattice.Restrict"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Rop"            /import "Koshucode.Baala.Op.Lattice.Tropashko"

|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Tropashko"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Tropashko"      /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice.Tropashko"      /import "Koshucode.Baala.Op.Builtin"

|-- IMPORT  /module "Koshucode.Baala.Op.Lattice"                /import "Koshucode.Baala.Op.Lattice.Restrict"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice"                /import "Koshucode.Baala.Op.Lattice.Rop"
|-- IMPORT  /module "Koshucode.Baala.Op.Lattice"                /import "Koshucode.Baala.Op.Lattice.Tropashko"

|-- IMPORT  /module "Koshucode.Baala.Op.Message"                /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Message"                /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Message"                /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Meta"                   /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Op.Meta"                   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Meta"                   /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Meta"                   /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Meta"                   /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Confl"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Confl"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Confl"             /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Confl"             /import "Koshucode.Baala.Op.Nest.Flow"

|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Deriv"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Deriv"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Deriv"             /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Deriv"             /import "Koshucode.Baala.Op.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Deriv"             /import "Koshucode.Baala.Op.Term"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Deriv"             /import "Koshucode.Baala.Op.Nest.Confl"

|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Flow"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Flow"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Flow"              /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Flow"              /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Rop"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Rop"               /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Rop"               /import "Koshucode.Baala.Op.Nest.Confl"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Rop"               /import "Koshucode.Baala.Op.Nest.Deriv"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Rop"               /import "Koshucode.Baala.Op.Nest.Flow"

|-- IMPORT  /module "Koshucode.Baala.Op.Nest"                   /import "Koshucode.Baala.Op.Nest.Confl"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest"                   /import "Koshucode.Baala.Op.Nest.Deriv"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest"                   /import "Koshucode.Baala.Op.Nest.Flow"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest"                   /import "Koshucode.Baala.Op.Nest.Rop"

|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Op.Term"
|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Resource"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Resource"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Resource"               /import "Koshucode.Baala.Op.Builtin"

|-- IMPORT  /module "Koshucode.Baala.Op.Source"                 /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Source"                 /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Source"                 /import "Koshucode.Baala.Op.Builtin"

|-- IMPORT  /module "Koshucode.Baala.Op.Term"                   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Term"                   /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Term"                   /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Term"                   /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.TermGadget"             /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Op.TermGadget"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.TermGadget"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.TermGadget"             /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.TermGadget"             /import "Koshucode.Baala.Op.Term"

|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Check"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Control"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.DepRank"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Meta"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Nest"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Peripheral"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Resource"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Source"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Term"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.TermGadget"

```

Command `./dir-rank.k ../operator/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../operator/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala.Core  /base-rank {| /rank /base /import-dir [ 0 | 'Message | [ ] ] |}
|-- DIR-RANK  /dir-rank 3  /dir 'Koshucode.Baala.Op.Builtin  /base-rank {| /rank /base /import-dir [ 1 | 'Define | [ ] ] [ 2 | 'Rop | [ ] ] [ 2 | 'Term | [ ] ] [ 3 | 'Get | [ ] ] |}
|-- DIR-RANK  /dir-rank 7  /dir 'Koshucode.Baala.Op.Lattice  /base-rank {| /rank /base /import-dir [ 5 | 'Tropashko | [ 'Builtin ] ] [ 6 | 'Restrict | [ 'Builtin ] ] [ 7 | 'Rop | [ 'Builtin ] ] |}
|-- DIR-RANK  /dir-rank 10  /dir 'Koshucode.Baala.Op.Nest  /base-rank {| /rank /base /import-dir [ 5 | 'Flow | [ 'Builtin ] ] [ 6 | 'Confl | [ 'Builtin ] ] [ 9 | 'Deriv | [ 'Builtin | 'Lattice ] ] [ 10 | 'Rop | [ 'Builtin ] ] |}
|-- DIR-RANK  /dir-rank 11  /dir 'Koshucode.Baala.Op  /base-rank {| /rank /base /import-dir [ 1 | 'DepRank | [ ] ] [ 1 | 'Message | [ ] ] [ 4 | 'Builtin | [ ] ] [ 5 | 'Gadget | [ 'Builtin ] ] [ 5 | 'Meta | [ 'Builtin ] ] [ 5 | 'Resource | [ 'Builtin ] ] [ 5 | 'Source | [ 'Builtin ] ] [ 5 | 'Term | [ 'Builtin ] ] [ 6 | 'Peripheral | [ 'Builtin ] ] [ 6 | 'TermGadget | [ 'Builtin ] ] [ 8 | 'Lattice | [ ] ] [ 9 | 'Check | [ 'Builtin | 'Lattice ] ] [ 9 | 'Control | [ 'Builtin | 'Lattice ] ] [ 11 | 'Nest | [ ] ] |}

|-- DIR-RANK  /dir-rank 12  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 0 | 'Base | [ ] ] [ 0 | 'Core | [ ] ] [ 12 | 'Op | [ 'Builtin | 'Lattice | 'Nest ] ] |}

*** 6 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                          /base-rank
  --------- ----------------------------- -----------------------------------------------------
  0         'Koshucode.Baala.Core         /rank /base         /import-dir
                                          ----- ------------- ---------------------------------
                                          0     'Message      [ ]
                                          
  3         'Koshucode.Baala.Op.Builtin   /rank /base         /import-dir
                                          ----- ------------- ---------------------------------
                                          1     'Define       [ ]
                                          2     'Rop          [ ]
                                          2     'Term         [ ]
                                          3     'Get          [ ]
                                          
  7         'Koshucode.Baala.Op.Lattice   /rank /base         /import-dir
                                          ----- ------------- ---------------------------------
                                          5     'Tropashko    [ 'Builtin ]
                                          6     'Restrict     [ 'Builtin ]
                                          7     'Rop          [ 'Builtin ]
                                          
  10        'Koshucode.Baala.Op.Nest      /rank /base         /import-dir
                                          ----- ------------- ---------------------------------
                                          5     'Flow         [ 'Builtin ]
                                          6     'Confl        [ 'Builtin ]
                                          9     'Deriv        [ 'Builtin | 'Lattice ]
                                          10    'Rop          [ 'Builtin ]
                                          
  11        'Koshucode.Baala.Op           /rank /base         /import-dir
                                          ----- ------------- ---------------------------------
                                          1     'DepRank      [ ]
                                          1     'Message      [ ]
                                          4     'Builtin      [ ]
                                          5     'Gadget       [ 'Builtin ]
                                          5     'Meta         [ 'Builtin ]
                                          5     'Resource     [ 'Builtin ]
                                          5     'Source       [ 'Builtin ]
                                          5     'Term         [ 'Builtin ]
                                          6     'Peripheral   [ 'Builtin ]
                                          6     'TermGadget   [ 'Builtin ]
                                          8     'Lattice      [ ]
                                          9     'Check        [ 'Builtin | 'Lattice ]
                                          9     'Control      [ 'Builtin | 'Lattice ]
                                          11    'Nest         [ ]
                                          
  12        'Koshucode.Baala              /rank /base         /import-dir
                                          ----- ------------- ---------------------------------
                                          0     'Base         [ ]
                                          0     'Core         [ ]
                                          12    'Op           [ 'Builtin | 'Lattice | 'Nest ]
                                          

=== rel

**
**  SUMMARY
**       6 judges on DIR-RANK
**       6 judges in total
**
```



## ../content/IMPORT.k

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
**    90 judges
**

|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Arith"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Arith"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Arith"              /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Coxhand"            /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Coxhand"            /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop.List"               /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.List"               /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.List"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.List"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.List"               /import "Koshucode.Baala.Op.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.List"               /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Logic"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Logic"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Logic"              /import "Koshucode.Baala.Op.Cop.Coxhand"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Misc"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Misc"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Misc"               /import "Koshucode.Baala.Op.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Misc"               /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Order"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Order"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Order"              /import "Koshucode.Baala.Op.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Order"              /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Time"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Time"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Time"               /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Type"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Type"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Type"               /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.Arith"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.List"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.Logic"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.Misc"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.Order"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.Time"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.Type"

|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Accessor"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Accessor"           /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Accessor"           /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Accessor"           /import "Koshucode.Baala.Op.Cox.Get"

|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Calc"               /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Calc"               /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Calc"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Calc"               /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Calc"               /import "Koshucode.Baala.Op.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Calc"               /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Empty"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Empty"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Empty"              /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Empty"              /import "Koshucode.Baala.Op.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Empty"              /import "Koshucode.Baala.Op.Cox.Get"

|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Filter"             /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Filter"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Filter"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Filter"             /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Filter"             /import "Koshucode.Baala.Op.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Filter"             /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Gadget"             /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Gadget"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Gadget"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Gadget"             /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Gadget"             /import "Koshucode.Baala.Op.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Gadget"             /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Get"                /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Get"                /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Get"                /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Get"                /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Get"                /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Range"              /import "Prelude"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Range"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Range"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Range"              /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox.Range"              /import "Koshucode.Baala.Op.Cox.Get"

|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Accessor"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Calc"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Empty"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Filter"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Get"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Range"

|-- IMPORT  /module "Koshucode.Baala.Op.Global"                 /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Global"                 /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Global"                 /import "Koshucode.Baala.Op"
|-- IMPORT  /module "Koshucode.Baala.Op.Global"                 /import "Koshucode.Baala.Op.Cox"
|-- IMPORT  /module "Koshucode.Baala.Op.Global"                 /import "Koshucode.Baala.Op.Cop"

|-- IMPORT  /module "Koshucode.Baala.Op.Quoter"                 /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Quoter"                 /import "Koshucode.Baala.Type.Vanilla"

|-- IMPORT  /module "Koshucode.Baala.Type.Vanilla"              /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Type.Vanilla"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Type.Vanilla"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Type.Vanilla"              /import "Koshucode.Baala.Op.Message"

```

Command `./dir-rank.k ../content/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../content/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 0 | 'Base | [ ] ] [ 0 | 'Core | [ ] ] [ 0 | 'Op | [ ] ] |}
|-- DIR-RANK  /dir-rank 1  /dir 'Koshucode.Baala.Type  /base-rank {| /rank /base /import-dir [ 1 | 'Vanilla | [ ] ] |}
|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Op.Cop  /base-rank {| /rank /base /import-dir [ 1 | 'Arith | [ ] ] [ 1 | 'Coxhand | [ ] ] [ 1 | 'Time | [ ] ] [ 1 | 'Type | [ ] ] [ 2 | 'List | [ ] ] [ 2 | 'Logic | [ ] ] [ 2 | 'Misc | [ ] ] [ 2 | 'Order | [ ] ] |}
|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Op.Cox  /base-rank {| /rank /base /import-dir [ 1 | 'Get | [ ] ] [ 2 | 'Accessor | [ ] ] [ 2 | 'Calc | [ ] ] [ 2 | 'Empty | [ ] ] [ 2 | 'Filter | [ ] ] [ 2 | 'Gadget | [ ] ] [ 2 | 'Range | [ ] ] |}
|-- DIR-RANK  /dir-rank 4  /dir 'Koshucode.Baala.Op  /base-rank {| /rank /base /import-dir [ 0 | 'Builtin | [ ] ] [ 0 | 'Lattice | [ ] ] [ 0 | 'Message | [ ] ] [ 2 | 'Quoter | [ ] ] [ 3 | 'Cop | [ ] ] [ 3 | 'Cox | [ ] ] [ 4 | 'Global | [ 'Cop | 'Cox | 'Op ] ] |}

*** 5 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                      /base-rank
  --------- ------------------------- -----------------------------------------
  0         'Koshucode.Baala          /rank /base       /import-dir
                                      ----- ----------- -----------------------
                                      0     'Base       [ ]
                                      0     'Core       [ ]
                                      0     'Op         [ ]
                                      
  1         'Koshucode.Baala.Type     /rank /base       /import-dir
                                      ----- ----------- -----------------------
                                      1     'Vanilla    [ ]
                                      
  2         'Koshucode.Baala.Op.Cop   /rank /base       /import-dir
                                      ----- ----------- -----------------------
                                      1     'Arith      [ ]
                                      1     'Coxhand    [ ]
                                      1     'Time       [ ]
                                      1     'Type       [ ]
                                      2     'List       [ ]
                                      2     'Logic      [ ]
                                      2     'Misc       [ ]
                                      2     'Order      [ ]
                                      
  2         'Koshucode.Baala.Op.Cox   /rank /base       /import-dir
                                      ----- ----------- -----------------------
                                      1     'Get        [ ]
                                      2     'Accessor   [ ]
                                      2     'Calc       [ ]
                                      2     'Empty      [ ]
                                      2     'Filter     [ ]
                                      2     'Gadget     [ ]
                                      2     'Range      [ ]
                                      
  4         'Koshucode.Baala.Op       /rank /base       /import-dir
                                      ----- ----------- -----------------------
                                      0     'Builtin    [ ]
                                      0     'Lattice    [ ]
                                      0     'Message    [ ]
                                      2     'Quoter     [ ]
                                      3     'Cop        [ ]
                                      3     'Cox        [ ]
                                      4     'Global     [ 'Cop | 'Cox | 'Op ]
                                      

=== rel

**
**  SUMMARY
**       5 judges on DIR-RANK
**       5 judges in total
**
```



## ../calculator/IMPORT.k

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
**    26 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "GHC.IO.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "System.Environment"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "System.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "Koshucode.Baala.Base"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "System.Environment"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Version"   /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Version"   /import "Paths_koshucode_baala_calculator"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Data.Time"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Toolkit.Library.Run"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuFilter"  /import "Koshucode.Baala.Toolkit.Library.Exit"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Data.Time"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Element"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Run"

```

Command `./dir-rank.k ../calculator/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../calculator/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 0 | 'Base | [ ] ] [ 0 | 'Core | [ ] ] |}
|-- DIR-RANK  /dir-rank 1  /dir 'Koshucode.Baala.Toolkit.Library  /base-rank {| /rank /base /import-dir [ 1 | 'Element | [ ] ] [ 1 | 'Exit | [ ] ] [ 1 | 'Run | [ ] ] |}
|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Toolkit.Main  /base-rank {| /rank /base /import-dir [ 2 | 'KoshuFilter | [ ] ] [ 2 | 'KoshuMain | [ ] ] |}

*** 3 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                               /base-rank
  --------- ---------------------------------- --------------------------------
  0         'Koshucode.Baala                   /rank /base          /import-dir
                                               ----- -------------- -----------
                                               0     'Base          [ ]
                                               0     'Core          [ ]
                                               
  1         'Koshucode.Baala.Toolkit.Library   /rank /base          /import-dir
                                               ----- -------------- -----------
                                               1     'Element       [ ]
                                               1     'Exit          [ ]
                                               1     'Run           [ ]
                                               
  2         'Koshucode.Baala.Toolkit.Main      /rank /base          /import-dir
                                               ----- -------------- -----------
                                               2     'KoshuFilter   [ ]
                                               2     'KoshuMain     [ ]
                                               

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

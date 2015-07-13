# I/O List

- [./import-rank.k](#import-rankk)
- [../base/IMPORT.k](#baseimportk)
- [../calculator/IMPORT.k](#calculatorimportk)
- [../content/IMPORT.k](#contentimportk)
- [../core/IMPORT.k](#coreimportk)
- [../operator/IMPORT.k](#operatorimportk)
- [../toolkit/IMPORT.k](#toolkitimportk)
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
  | keep part "Koshucode" /import
  | dependent-rank /module /import -rank /rank
  | interp <<< /module has dependent rank /rank . >>>
  --order --forward /rank
```



## [../base/IMPORT.k](../base/IMPORT.k)

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
**    214 judges
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

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Token"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Mono"            /import "Koshucode.Baala.Base.Data.Rel"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Mono"            /import "Koshucode.Baala.Base.Abort"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Data.Map.Strict"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Para"            /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Data.Head"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Data.Judge"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Result"          /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Result"          /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Result"          /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Result"          /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Result"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Result"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Result"          /import "Koshucode.Baala.Base.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Result"          /import "Koshucode.Baala.Base.Data.Judge"

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
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Para"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Rel"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Result"
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
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "System.IO"
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



## [../calculator/IMPORT.k](../calculator/IMPORT.k)

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



## [../content/IMPORT.k](../content/IMPORT.k)

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



## [../core/IMPORT.k](../core/IMPORT.k)

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



## [../operator/IMPORT.k](../operator/IMPORT.k)

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



## [../toolkit/IMPORT.k](../toolkit/IMPORT.k)

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
**    32 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Data.Set"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Type.Vanilla"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Change"    /import "Koshucode.Baala.Toolkit.Library.Input"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Input"     /import "Koshucode.Baala.Type.Vanilla"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Data.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.RDF"       /import "Data.Text"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuChange"  /import "System.Console.GetOpt"
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
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Type.Vanilla"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.RDF"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuRdf"     /import "Koshucode.Baala.Toolkit.Library.Version"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Type.Vanilla"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Toolkit.Library.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuSyntax"  /import "Koshucode.Baala.Toolkit.Library.Version"

```



## output


Command `./import-rank.k ../base/IMPORT.k ../calculator/IMPORT.k ../content/IMPORT.k ../core/IMPORT.k ../operator/IMPORT.k ../toolkit/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./import-rank.k
**    ../base/IMPORT.k
**    ../calculator/IMPORT.k
**    ../content/IMPORT.k
**    ../core/IMPORT.k
**    ../operator/IMPORT.k
**    ../toolkit/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- IMPORT-RANK  /rank 0  /module 'Koshucode.Baala.Base.Prelude.Class
|-- IMPORT-RANK  /rank 0  /module 'Koshucode.Baala.Base.Prelude.Import
|-- IMPORT-RANK  /rank 0  /module 'Koshucode.Baala.Base.Syntax.Line
|-- IMPORT-RANK  /rank 0  /module 'Koshucode.Baala.Base.Text.Comment
|-- IMPORT-RANK  /rank 0  /module 'Koshucode.Baala.Base.Text.Unicode

|-- IMPORT-RANK  /rank 0  /module 'Koshucode.Baala.Toolkit.Library.Version
|-- IMPORT-RANK  /rank 1  /module 'Koshucode.Baala.Base.Prelude.Assoc
|-- IMPORT-RANK  /rank 1  /module 'Koshucode.Baala.Base.Prelude.List
|-- IMPORT-RANK  /rank 1  /module 'Koshucode.Baala.Base.Prelude.Pair
|-- IMPORT-RANK  /rank 1  /module 'Koshucode.Baala.Base.Prelude.Snip

|-- IMPORT-RANK  /rank 2  /module 'Koshucode.Baala.Base.Prelude.Order
|-- IMPORT-RANK  /rank 3  /module 'Koshucode.Baala.Base.Prelude
|-- IMPORT-RANK  /rank 4  /module 'Koshucode.Baala.Base.Text.Http
|-- IMPORT-RANK  /rank 4  /module 'Koshucode.Baala.Base.Text.IOPoint
|-- IMPORT-RANK  /rank 4  /module 'Koshucode.Baala.Base.Text.TextTable

|-- IMPORT-RANK  /rank 4  /module 'Koshucode.Baala.Base.Text.Utility
|-- IMPORT-RANK  /rank 4  /module 'Koshucode.Baala.Base.Text.Write
|-- IMPORT-RANK  /rank 5  /module 'Koshucode.Baala.Base.Text.CodePt
|-- IMPORT-RANK  /rank 6  /module 'Koshucode.Baala.Base.Text
|-- IMPORT-RANK  /rank 7  /module 'Koshucode.Baala.Base.Abort.Reason

|-- IMPORT-RANK  /rank 7  /module 'Koshucode.Baala.Base.Token.AngleText
|-- IMPORT-RANK  /rank 7  /module 'Koshucode.Baala.Base.Token.Token
|-- IMPORT-RANK  /rank 8  /module 'Koshucode.Baala.Base.Abort.Abortable
|-- IMPORT-RANK  /rank 8  /module 'Koshucode.Baala.Base.Abort.Report
|-- IMPORT-RANK  /rank 8  /module 'Koshucode.Baala.Base.Token.Short
*** 25

|-- IMPORT-RANK  /rank 9  /module 'Koshucode.Baala.Base.Abort
|-- IMPORT-RANK  /rank 10  /module 'Koshucode.Baala.Base.Message
|-- IMPORT-RANK  /rank 11  /module 'Koshucode.Baala.Base.Data.Clock
|-- IMPORT-RANK  /rank 11  /module 'Koshucode.Baala.Base.Data.Date
|-- IMPORT-RANK  /rank 11  /module 'Koshucode.Baala.Base.Data.Decimal

|-- IMPORT-RANK  /rank 11  /module 'Koshucode.Baala.Base.Data.Para
|-- IMPORT-RANK  /rank 11  /module 'Koshucode.Baala.Base.Syntax.Code
|-- IMPORT-RANK  /rank 11  /module 'Koshucode.Baala.Base.Syntax.Tree
|-- IMPORT-RANK  /rank 12  /module 'Koshucode.Baala.Base.Data.Time
|-- IMPORT-RANK  /rank 12  /module 'Koshucode.Baala.Base.Syntax.Infix

|-- IMPORT-RANK  /rank 13  /module 'Koshucode.Baala.Base.Syntax
|-- IMPORT-RANK  /rank 14  /module 'Koshucode.Baala.Base.Token.TokenLine
|-- IMPORT-RANK  /rank 15  /module 'Koshucode.Baala.Base.Token.TokenClause
|-- IMPORT-RANK  /rank 15  /module 'Koshucode.Baala.Base.Token.TokenTree
|-- IMPORT-RANK  /rank 16  /module 'Koshucode.Baala.Base.Token

|-- IMPORT-RANK  /rank 17  /module 'Koshucode.Baala.Base.Data.Interp
|-- IMPORT-RANK  /rank 17  /module 'Koshucode.Baala.Base.Data.Judge
|-- IMPORT-RANK  /rank 17  /module 'Koshucode.Baala.Base.Data.Type
|-- IMPORT-RANK  /rank 18  /module 'Koshucode.Baala.Base.Data.About
|-- IMPORT-RANK  /rank 18  /module 'Koshucode.Baala.Base.Data.Head

|-- IMPORT-RANK  /rank 18  /module 'Koshucode.Baala.Base.Data.Result
|-- IMPORT-RANK  /rank 19  /module 'Koshucode.Baala.Base.Data.Rel
|-- IMPORT-RANK  /rank 20  /module 'Koshucode.Baala.Base.Data.Mono
|-- IMPORT-RANK  /rank 21  /module 'Koshucode.Baala.Base.Data
|-- IMPORT-RANK  /rank 22  /module 'Koshucode.Baala.Base
*** 50

|-- IMPORT-RANK  /rank 23  /module 'Koshucode.Baala.Core.Message
|-- IMPORT-RANK  /rank 23  /module 'Koshucode.Baala.Op.DepRank
|-- IMPORT-RANK  /rank 23  /module 'Koshucode.Baala.Toolkit.Library.Exit
|-- IMPORT-RANK  /rank 24  /module 'Koshucode.Baala.Core.Content.Class
|-- IMPORT-RANK  /rank 24  /module 'Koshucode.Baala.Core.Content.Cox

|-- IMPORT-RANK  /rank 24  /module 'Koshucode.Baala.Core.Content.Tree
|-- IMPORT-RANK  /rank 24  /module 'Koshucode.Baala.Core.Lexmap.AttrPos
|-- IMPORT-RANK  /rank 25  /module 'Koshucode.Baala.Core.Content.Cop
|-- IMPORT-RANK  /rank 25  /module 'Koshucode.Baala.Core.Content.Literal
|-- IMPORT-RANK  /rank 25  /module 'Koshucode.Baala.Core.Lexmap.Attr

|-- IMPORT-RANK  /rank 25  /module 'Koshucode.Baala.Core.Lexmap.Slot
|-- IMPORT-RANK  /rank 26  /module 'Koshucode.Baala.Core.Content.Build
|-- IMPORT-RANK  /rank 26  /module 'Koshucode.Baala.Core.Lexmap.AttrEd
|-- IMPORT-RANK  /rank 26  /module 'Koshucode.Baala.Core.Lexmap.Lexmap
|-- IMPORT-RANK  /rank 27  /module 'Koshucode.Baala.Core.Content.Run

|-- IMPORT-RANK  /rank 27  /module 'Koshucode.Baala.Core.Lexmap.LexmapTrees
|-- IMPORT-RANK  /rank 28  /module 'Koshucode.Baala.Core.Content
|-- IMPORT-RANK  /rank 28  /module 'Koshucode.Baala.Core.Lexmap.Construct
|-- IMPORT-RANK  /rank 29  /module 'Koshucode.Baala.Core.Assert.RelTable
|-- IMPORT-RANK  /rank 29  /module 'Koshucode.Baala.Core.Lexmap

|-- IMPORT-RANK  /rank 29  /module 'Koshucode.Baala.Core.Relmap.Option
|-- IMPORT-RANK  /rank 30  /module 'Koshucode.Baala.Core.Relkit.Relkit
|-- IMPORT-RANK  /rank 30  /module 'Koshucode.Baala.Core.Resource.Clause
|-- IMPORT-RANK  /rank 31  /module 'Koshucode.Baala.Core.Relkit.Construct
|-- IMPORT-RANK  /rank 32  /module 'Koshucode.Baala.Core.Relkit.Run
*** 75

|-- IMPORT-RANK  /rank 33  /module 'Koshucode.Baala.Core.Relkit
|-- IMPORT-RANK  /rank 34  /module 'Koshucode.Baala.Core.Assert.Dataset
|-- IMPORT-RANK  /rank 34  /module 'Koshucode.Baala.Core.Relmap.Relmap
|-- IMPORT-RANK  /rank 35  /module 'Koshucode.Baala.Core.Relmap.Rop
|-- IMPORT-RANK  /rank 35  /module 'Koshucode.Baala.Core.Relmap.Specialize

|-- IMPORT-RANK  /rank 36  /module 'Koshucode.Baala.Core.Relmap.Construct
|-- IMPORT-RANK  /rank 36  /module 'Koshucode.Baala.Core.Relmap.Global
|-- IMPORT-RANK  /rank 37  /module 'Koshucode.Baala.Core.Relmap
|-- IMPORT-RANK  /rank 38  /module 'Koshucode.Baala.Core.Assert.Assert
|-- IMPORT-RANK  /rank 39  /module 'Koshucode.Baala.Core.Assert.Run

|-- IMPORT-RANK  /rank 40  /module 'Koshucode.Baala.Core.Assert
|-- IMPORT-RANK  /rank 41  /module 'Koshucode.Baala.Core.Resource.Resource
|-- IMPORT-RANK  /rank 42  /module 'Koshucode.Baala.Core.Resource.Include
|-- IMPORT-RANK  /rank 42  /module 'Koshucode.Baala.Core.Resource.Run
|-- IMPORT-RANK  /rank 43  /module 'Koshucode.Baala.Core.Resource.Quoter

|-- IMPORT-RANK  /rank 43  /module 'Koshucode.Baala.Core.Resource.Read
|-- IMPORT-RANK  /rank 44  /module 'Koshucode.Baala.Core.Resource
|-- IMPORT-RANK  /rank 45  /module 'Koshucode.Baala.Core
|-- IMPORT-RANK  /rank 46  /module 'Koshucode.Baala.Op.Builtin.Define
|-- IMPORT-RANK  /rank 46  /module 'Koshucode.Baala.Op.Cop.Coxhand

|-- IMPORT-RANK  /rank 46  /module 'Koshucode.Baala.Op.Message
|-- IMPORT-RANK  /rank 46  /module 'Koshucode.Baala.Toolkit.Library.Element
|-- IMPORT-RANK  /rank 46  /module 'Koshucode.Baala.Toolkit.Library.RDF
|-- IMPORT-RANK  /rank 46  /module 'Koshucode.Baala.Toolkit.Library.Run
|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Op.Builtin.Rop
*** 100

|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Op.Builtin.Term
|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Op.Cop.Arith
|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Op.Cop.List
|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Op.Cop.Logic
|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Op.Cop.Misc

|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Op.Cop.Order
|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Op.Cop.Time
|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Op.Cop.Type
|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Toolkit.Main.KoshuFilter
|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Toolkit.Main.KoshuMain

|-- IMPORT-RANK  /rank 47  /module 'Koshucode.Baala.Type.Vanilla
|-- IMPORT-RANK  /rank 48  /module 'Koshucode.Baala.Op.Builtin.Get
|-- IMPORT-RANK  /rank 48  /module 'Koshucode.Baala.Op.Cop
|-- IMPORT-RANK  /rank 48  /module 'Koshucode.Baala.Op.Quoter
|-- IMPORT-RANK  /rank 48  /module 'Koshucode.Baala.Toolkit.Library.Input

|-- IMPORT-RANK  /rank 48  /module 'Koshucode.Baala.Toolkit.Main.KoshuRdf
|-- IMPORT-RANK  /rank 48  /module 'Koshucode.Baala.Toolkit.Main.KoshuSyntax
|-- IMPORT-RANK  /rank 49  /module 'Koshucode.Baala.Op.Builtin
|-- IMPORT-RANK  /rank 49  /module 'Koshucode.Baala.Toolkit.Library.Change
|-- IMPORT-RANK  /rank 50  /module 'Koshucode.Baala.Op.Cox.Get

|-- IMPORT-RANK  /rank 50  /module 'Koshucode.Baala.Op.Gadget
|-- IMPORT-RANK  /rank 50  /module 'Koshucode.Baala.Op.Lattice.Tropashko
|-- IMPORT-RANK  /rank 50  /module 'Koshucode.Baala.Op.Meta
|-- IMPORT-RANK  /rank 50  /module 'Koshucode.Baala.Op.Nest.Flow
|-- IMPORT-RANK  /rank 50  /module 'Koshucode.Baala.Op.Resource
*** 125

|-- IMPORT-RANK  /rank 50  /module 'Koshucode.Baala.Op.Source
|-- IMPORT-RANK  /rank 50  /module 'Koshucode.Baala.Op.Term
|-- IMPORT-RANK  /rank 50  /module 'Koshucode.Baala.Toolkit.Main.KoshuChange
|-- IMPORT-RANK  /rank 51  /module 'Koshucode.Baala.Op.Cox.Accessor
|-- IMPORT-RANK  /rank 51  /module 'Koshucode.Baala.Op.Cox.Calc

|-- IMPORT-RANK  /rank 51  /module 'Koshucode.Baala.Op.Cox.Filter
|-- IMPORT-RANK  /rank 51  /module 'Koshucode.Baala.Op.Cox.Gadget
|-- IMPORT-RANK  /rank 51  /module 'Koshucode.Baala.Op.Cox.Range
|-- IMPORT-RANK  /rank 51  /module 'Koshucode.Baala.Op.Lattice.Restrict
|-- IMPORT-RANK  /rank 51  /module 'Koshucode.Baala.Op.Nest.Confl

|-- IMPORT-RANK  /rank 51  /module 'Koshucode.Baala.Op.Peripheral
|-- IMPORT-RANK  /rank 51  /module 'Koshucode.Baala.Op.TermGadget
|-- IMPORT-RANK  /rank 52  /module 'Koshucode.Baala.Op.Lattice.Rop
|-- IMPORT-RANK  /rank 53  /module 'Koshucode.Baala.Op.Lattice
|-- IMPORT-RANK  /rank 54  /module 'Koshucode.Baala.Op.Check

|-- IMPORT-RANK  /rank 54  /module 'Koshucode.Baala.Op.Control
|-- IMPORT-RANK  /rank 54  /module 'Koshucode.Baala.Op.Cox.Empty
|-- IMPORT-RANK  /rank 54  /module 'Koshucode.Baala.Op.Nest.Deriv
|-- IMPORT-RANK  /rank 55  /module 'Koshucode.Baala.Op.Cox
|-- IMPORT-RANK  /rank 55  /module 'Koshucode.Baala.Op.Nest.Rop

|-- IMPORT-RANK  /rank 56  /module 'Koshucode.Baala.Op.Nest
|-- IMPORT-RANK  /rank 57  /module 'Koshucode.Baala.Op
|-- IMPORT-RANK  /rank 58  /module 'Koshucode.Baala.Op.Global

*** 148 judges

**
**  SUMMARY
**     148 judges on IMPORT-RANK
**     148 judges in total
**
```



## command

This document is produced by the command:

```
koshu-inout.sh -o IMPORT-RANK.md ./import-rank.k ../base/IMPORT.k ../calculator/IMPORT.k ../content/IMPORT.k ../core/IMPORT.k ../operator/IMPORT.k ../toolkit/IMPORT.k
```

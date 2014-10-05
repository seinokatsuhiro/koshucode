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
**    168 judges
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

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Koshucode.Baala.Base.Message"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Decimal"         /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Interp"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Interp"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Interp"          /import "Koshucode.Baala.Base.Token"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Data.Term"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Data.Rel"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Judge"           /import "Koshucode.Baala.Base.Data.Relhead"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Mono"            /import "Koshucode.Baala.Base.Data.Rel"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Mono"            /import "Koshucode.Baala.Base.Abort"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Control.Monad"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Koshucode.Baala.Base.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Output"          /import "Koshucode.Baala.Base.Data.Judge"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Rel"             /import "Koshucode.Baala.Base.Data.Relhead"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Relhead"         /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Relhead"         /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Relhead"         /import "Koshucode.Baala.Base.Token"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Relhead"         /import "Koshucode.Baala.Base.Data.Term"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Term"            /import "Koshucode.Baala.Base.Abort"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Term"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Term"            /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Term"            /import "Koshucode.Baala.Base.Token"

|-- IMPORT  /module "Koshucode.Baala.Base.Data.Type"            /import "Koshucode.Baala.Base.Prelude"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Type"            /import "Koshucode.Baala.Base.Text"
|-- IMPORT  /module "Koshucode.Baala.Base.Data.Type"            /import "Koshucode.Baala.Base.Token"

|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Decimal"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Interp"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Judge"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Mono"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Output"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Rel"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Relhead"
|-- IMPORT  /module "Koshucode.Baala.Base.Data"                 /import "Koshucode.Baala.Base.Data.Term"
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

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Comment"         /import "System.IO"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Resource"        /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Resource"        /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.TextTable"       /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Unicode"         /import "Data.Char"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "Data.Char"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Utility"         /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Text.PrettyPrint"
|-- IMPORT  /module "Koshucode.Baala.Base.Text.Write"           /import "Koshucode.Baala.Base.Prelude"

|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Comment"
|-- IMPORT  /module "Koshucode.Baala.Base.Text"                 /import "Koshucode.Baala.Base.Text.Resource"
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

|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Base.Prelude  /base-rank {| /rank : /base | 0 : 'Class | 0 : 'Import | 0 : 'Pair | 1 : 'Assoc | 1 : 'List | 1 : 'Snip | 2 : 'Order |}
|-- DIR-RANK  /dir-rank 4  /dir 'Koshucode.Baala.Base.Text  /base-rank {| /rank : /base | 0 : 'Comment | 0 : 'Unicode | 4 : 'Resource | 4 : 'TextTable | 4 : 'Utility | 4 : 'Write |}
|-- DIR-RANK  /dir-rank 7  /dir 'Koshucode.Baala.Base.Abort  /base-rank {| /rank : /base | 6 : 'Reason | 7 : 'Abortable | 7 : 'Report |}
|-- DIR-RANK  /dir-rank 11  /dir 'Koshucode.Baala.Base.Syntax  /base-rank {| /rank : /base | 0 : 'Line | 10 : 'Code | 10 : 'Tree | 11 : 'Infix |}
|-- DIR-RANK  /dir-rank 14  /dir 'Koshucode.Baala.Base.Token  /base-rank {| /rank : /base | 6 : 'AngleText | 6 : 'Token | 7 : 'Short | 13 : 'TokenLine | 14 : 'TokenClause | 14 : 'TokenTree |}

|-- DIR-RANK  /dir-rank 20  /dir 'Koshucode.Baala.Base.Data  /base-rank {| /rank : /base | 10 : 'Decimal | 16 : 'Interp | 16 : 'Term | 16 : 'Type | 17 : 'Relhead | 18 : 'Rel | 19 : 'Judge | 19 : 'Mono | 20 : 'Output |}
|-- DIR-RANK  /dir-rank 21  /dir 'Koshucode.Baala.Base  /base-rank {| /rank : /base | 3 : 'Prelude | 5 : 'Text | 8 : 'Abort | 9 : 'Message | 12 : 'Syntax | 15 : 'Token | 21 : 'Data |}
|-- DIR-RANK  /dir-rank 22  /dir 'Koshucode.Baala  /base-rank {| /rank : /base | 22 : 'Base |}

*** 8 judges

**  TABLE : DIR-RANK
**
**    /dir-rank /dir                            /base-rank
**    --------- ------------------------------- --------------------
**    2         'Koshucode.Baala.Base.Prelude   /rank /base
**                                              ----- --------------
**                                              0     'Class
**                                              0     'Import
**                                              0     'Pair
**                                              1     'Assoc
**                                              1     'List
**                                              1     'Snip
**                                              2     'Order
**                                              
**    4         'Koshucode.Baala.Base.Text      /rank /base
**                                              ----- --------------
**                                              0     'Comment
**                                              0     'Unicode
**                                              4     'Resource
**                                              4     'TextTable
**                                              4     'Utility
**                                              4     'Write
**                                              
**    7         'Koshucode.Baala.Base.Abort     /rank /base
**                                              ----- --------------
**                                              6     'Reason
**                                              7     'Abortable
**                                              7     'Report
**                                              
**    11        'Koshucode.Baala.Base.Syntax    /rank /base
**                                              ----- --------------
**                                              0     'Line
**                                              10    'Code
**                                              10    'Tree
**                                              11    'Infix
**                                              
**    14        'Koshucode.Baala.Base.Token     /rank /base
**                                              ----- --------------
**                                              6     'AngleText
**                                              6     'Token
**                                              7     'Short
**                                              13    'TokenLine
**                                              14    'TokenClause
**                                              14    'TokenTree
**                                              
**    20        'Koshucode.Baala.Base.Data      /rank /base
**                                              ----- --------------
**                                              10    'Decimal
**                                              16    'Interp
**                                              16    'Term
**                                              16    'Type
**                                              17    'Relhead
**                                              18    'Rel
**                                              19    'Judge
**                                              19    'Mono
**                                              20    'Output
**                                              
**    21        'Koshucode.Baala.Base           /rank /base
**                                              ----- --------------
**                                              3     'Prelude
**                                              5     'Text
**                                              8     'Abort
**                                              9     'Message
**                                              12    'Syntax
**                                              15    'Token
**                                              21    'Data
**                                              
**    22        'Koshucode.Baala                /rank /base
**                                              ----- --------------
**                                              22    'Base
**                                              

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
**    160 judges
**

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Assert"        /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Data.Maybe"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Dataset"       /import "Koshucode.Baala.Core.Relmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Data.Map"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.RelTable"      /import "Koshucode.Baala.Core.Content"

|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Assert.Run"           /import "Koshucode.Baala.Core.Assert.Dataset"
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

|-- IMPORT  /module "Koshucode.Baala.Core.Content.Literal"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Literal"      /import "Koshucode.Baala.Core.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Literal"      /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Build"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Cop"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Cox"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Content.Literal"
|-- IMPORT  /module "Koshucode.Baala.Core.Content.Run"          /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Build"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Class"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Cop"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Cox"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Literal"
|-- IMPORT  /module "Koshucode.Baala.Core.Content"              /import "Koshucode.Baala.Core.Content.Run"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attribute"     /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attribute"     /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attribute"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Attribute"     /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Core.Lexmap.Attribute"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Core.Lexmap.Roamap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Core.Lexmap.Slot"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Lexmap"        /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Roamap"        /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Roamap"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Roamap"        /import "Koshucode.Baala.Core.Lexmap.Attribute"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Roamap"        /import "Koshucode.Baala.Core.Lexmap.Slot"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Roamap"        /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Core.Lexmap.Attribute"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Slot"          /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Sorter"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Sorter"        /import "Koshucode.Baala.Core.Lexmap.Attribute"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap.Sorter"        /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Attribute"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Roamap"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Slot"
|-- IMPORT  /module "Koshucode.Baala.Core.Lexmap"               /import "Koshucode.Baala.Core.Lexmap.Sorter"

|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Message"              /import "Koshucode.Baala.Base.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Global"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Relmap.Operator"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Construct"     /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Global"        /import "Koshucode.Baala.Core.Relmap.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Operator"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Operator"      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Operator"      /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Operator"      /import "Koshucode.Baala.Core.Relmap.Global"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Operator"      /import "Koshucode.Baala.Core.Relmap.Relkit"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relkit"        /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Relkit"        /import "Koshucode.Baala.Core.Lexmap"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Run"           /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Run"           /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Run"           /import "Koshucode.Baala.Core.Relmap.Global"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Run"           /import "Koshucode.Baala.Core.Relmap.Operator"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Run"           /import "Koshucode.Baala.Core.Relmap.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Run"           /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Relmap.Operator"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Relmap.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap.Specialize"    /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Construct"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Global"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Operator"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Relkit"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Run"
|-- IMPORT  /module "Koshucode.Baala.Core.Relmap"               /import "Koshucode.Baala.Core.Relmap.Specialize"

|-- IMPORT  /module "Koshucode.Baala.Core.Section.Bundle"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Bundle"       /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Bundle"       /import "Koshucode.Baala.Core.Section.Section"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Bundle"       /import "Koshucode.Baala.Core.Section.Read"

|-- IMPORT  /module "Koshucode.Baala.Core.Section.Clause"       /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Clause"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Clause"       /import "Koshucode.Baala.Core.Assert"

|-- IMPORT  /module "Koshucode.Baala.Core.Section.Quoter"       /import "Data.Generics"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Quoter"       /import "Language.Haskell.TH"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Quoter"       /import "Language.Haskell.TH.Quote"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Quoter"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Quoter"       /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Quoter"       /import "Koshucode.Baala.Core.Section.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Quoter"       /import "Koshucode.Baala.Core.Section.Section"

|-- IMPORT  /module "Koshucode.Baala.Core.Section.Read"         /import "System.Directory"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Read"         /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Read"         /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Read"         /import "Koshucode.Baala.Core.Section.Section"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Read"         /import "Koshucode.Baala.Core.Section.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Read"         /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Section.Run"          /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Run"          /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Run"          /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Run"          /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Run"          /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Run"          /import "Koshucode.Baala.Core.Section.Section"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Run"          /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Section.Section"      /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Section"      /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Section"      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Section"      /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Section"      /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Section"      /import "Koshucode.Baala.Core.Section.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Section.Section"      /import "Koshucode.Baala.Core.Message"

|-- IMPORT  /module "Koshucode.Baala.Core.Section"              /import "Koshucode.Baala.Core.Section.Bundle"
|-- IMPORT  /module "Koshucode.Baala.Core.Section"              /import "Koshucode.Baala.Core.Section.Clause"
|-- IMPORT  /module "Koshucode.Baala.Core.Section"              /import "Koshucode.Baala.Core.Section.Quoter"
|-- IMPORT  /module "Koshucode.Baala.Core.Section"              /import "Koshucode.Baala.Core.Section.Read"
|-- IMPORT  /module "Koshucode.Baala.Core.Section"              /import "Koshucode.Baala.Core.Section.Run"
|-- IMPORT  /module "Koshucode.Baala.Core.Section"              /import "Koshucode.Baala.Core.Section.Section"

|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Assert"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Content"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Lexmap"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Relmap"
|-- IMPORT  /module "Koshucode.Baala.Core"                      /import "Koshucode.Baala.Core.Section"

```

Command `./dir-rank.k ../core/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../core/IMPORT.k
**

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala.Base  /base-rank {| /rank : /base | 0 : 'Message |}
|-- DIR-RANK  /dir-rank 5  /dir 'Koshucode.Baala.Core.Content  /base-rank {| /rank : /base | 2 : 'Class | 2 : 'Cox | 3 : 'Cop | 3 : 'Literal | 4 : 'Build | 5 : 'Run |}
|-- DIR-RANK  /dir-rank 5  /dir 'Koshucode.Baala.Core.Lexmap  /base-rank {| /rank : /base | 2 : 'Attribute | 3 : 'Slot | 3 : 'Sorter | 4 : 'Roamap | 5 : 'Lexmap |}
|-- DIR-RANK  /dir-rank 10  /dir 'Koshucode.Baala.Core.Relmap  /base-rank {| /rank : /base | 7 : 'Relkit | 8 : 'Global | 9 : 'Operator | 10 : 'Construct | 10 : 'Run | 10 : 'Specialize |}
|-- DIR-RANK  /dir-rank 13  /dir 'Koshucode.Baala.Core.Assert  /base-rank {| /rank : /base | 7 : 'RelTable | 12 : 'Assert | 12 : 'Dataset | 13 : 'Run |}

|-- DIR-RANK  /dir-rank 18  /dir 'Koshucode.Baala.Core.Section  /base-rank {| /rank : /base | 15 : 'Clause | 16 : 'Section | 17 : 'Quoter | 17 : 'Read | 17 : 'Run | 18 : 'Bundle |}
|-- DIR-RANK  /dir-rank 19  /dir 'Koshucode.Baala.Core  /base-rank {| /rank : /base | 1 : 'Message | 6 : 'Content | 6 : 'Lexmap | 11 : 'Relmap | 14 : 'Assert | 19 : 'Section |}
|-- DIR-RANK  /dir-rank 20  /dir 'Koshucode.Baala  /base-rank {| /rank : /base | 0 : 'Base | 20 : 'Core |}

*** 8 judges

**  TABLE : DIR-RANK
**
**    /dir-rank /dir                            /base-rank
**    --------- ------------------------------- -------------------
**    0         'Koshucode.Baala.Base           /rank /base
**                                              ----- -------------
**                                              0     'Message
**                                              
**    5         'Koshucode.Baala.Core.Content   /rank /base
**                                              ----- -------------
**                                              2     'Class
**                                              2     'Cox
**                                              3     'Cop
**                                              3     'Literal
**                                              4     'Build
**                                              5     'Run
**                                              
**    5         'Koshucode.Baala.Core.Lexmap    /rank /base
**                                              ----- -------------
**                                              2     'Attribute
**                                              3     'Slot
**                                              3     'Sorter
**                                              4     'Roamap
**                                              5     'Lexmap
**                                              
**    10        'Koshucode.Baala.Core.Relmap    /rank /base
**                                              ----- -------------
**                                              7     'Relkit
**                                              8     'Global
**                                              9     'Operator
**                                              10    'Construct
**                                              10    'Run
**                                              10    'Specialize
**                                              
**    13        'Koshucode.Baala.Core.Assert    /rank /base
**                                              ----- -------------
**                                              7     'RelTable
**                                              12    'Assert
**                                              12    'Dataset
**                                              13    'Run
**                                              
**    18        'Koshucode.Baala.Core.Section   /rank /base
**                                              ----- -------------
**                                              15    'Clause
**                                              16    'Section
**                                              17    'Quoter
**                                              17    'Read
**                                              17    'Run
**                                              18    'Bundle
**                                              
**    19        'Koshucode.Baala.Core           /rank /base
**                                              ----- -------------
**                                              1     'Message
**                                              6     'Content
**                                              6     'Lexmap
**                                              11    'Relmap
**                                              14    'Assert
**                                              19    'Section
**                                              
**    20        'Koshucode.Baala                /rank /base
**                                              ----- -------------
**                                              0     'Base
**                                              20    'Core
**                                              

**
**  SUMMARY
**       8 judges on DIR-RANK
**       8 judges in total
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
**    98 judges
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

|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Op.DepRank"
|-- IMPORT  /module "Koshucode.Baala.Op.Gadget"                 /import "Koshucode.Baala.Op.Message"

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
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Confl"             /import "Koshucode.Baala.Op.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Confl"             /import "Koshucode.Baala.Op.Term"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Confl"             /import "Koshucode.Baala.Op.Nest.Flow"

|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Flow"              /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Flow"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Flow"              /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Flow"              /import "Koshucode.Baala.Op.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Flow"              /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Rop"               /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Rop"               /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Rop"               /import "Koshucode.Baala.Op.Nest.Confl"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest.Rop"               /import "Koshucode.Baala.Op.Nest.Flow"

|-- IMPORT  /module "Koshucode.Baala.Op.Nest"                   /import "Koshucode.Baala.Op.Nest.Confl"
|-- IMPORT  /module "Koshucode.Baala.Op.Nest"                   /import "Koshucode.Baala.Op.Nest.Rop"

|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Op.Term"
|-- IMPORT  /module "Koshucode.Baala.Op.Peripheral"             /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Source"                 /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Source"                 /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Source"                 /import "Koshucode.Baala.Op.Builtin"

|-- IMPORT  /module "Koshucode.Baala.Op.Term"                   /import "Data.List"
|-- IMPORT  /module "Koshucode.Baala.Op.Term"                   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Op.Term"                   /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Term"                   /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op.Term"                   /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Builtin"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Check"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Control"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.DepRank"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Lattice"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Meta"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Nest"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Peripheral"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Source"
|-- IMPORT  /module "Koshucode.Baala.Op"                        /import "Koshucode.Baala.Op.Term"

```

Command `./dir-rank.k ../operator/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../operator/IMPORT.k
**

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala.Core  /base-rank {| /rank : /base | 0 : 'Message |}
|-- DIR-RANK  /dir-rank 3  /dir 'Koshucode.Baala.Op.Builtin  /base-rank {| /rank : /base | 1 : 'Define | 2 : 'Rop | 2 : 'Term | 3 : 'Get |}
|-- DIR-RANK  /dir-rank 7  /dir 'Koshucode.Baala.Op.Lattice  /base-rank {| /rank : /base | 5 : 'Tropashko | 6 : 'Restrict | 7 : 'Rop |}
|-- DIR-RANK  /dir-rank 11  /dir 'Koshucode.Baala.Op.Nest  /base-rank {| /rank : /base | 9 : 'Flow | 10 : 'Confl | 11 : 'Rop |}
|-- DIR-RANK  /dir-rank 12  /dir 'Koshucode.Baala.Op  /base-rank {| /rank : /base | 1 : 'DepRank | 1 : 'Message | 4 : 'Builtin | 5 : 'Gadget | 5 : 'Meta | 5 : 'Source | 5 : 'Term | 6 : 'Peripheral | 8 : 'Lattice | 9 : 'Check | 9 : 'Control | 12 : 'Nest |}

|-- DIR-RANK  /dir-rank 13  /dir 'Koshucode.Baala  /base-rank {| /rank : /base | 0 : 'Base | 0 : 'Core | 13 : 'Op |}

*** 6 judges

**  TABLE : DIR-RANK
**
**    /dir-rank /dir                          /base-rank
**    --------- ----------------------------- -------------------
**    0         'Koshucode.Baala.Core         /rank /base
**                                            ----- -------------
**                                            0     'Message
**                                            
**    3         'Koshucode.Baala.Op.Builtin   /rank /base
**                                            ----- -------------
**                                            1     'Define
**                                            2     'Rop
**                                            2     'Term
**                                            3     'Get
**                                            
**    7         'Koshucode.Baala.Op.Lattice   /rank /base
**                                            ----- -------------
**                                            5     'Tropashko
**                                            6     'Restrict
**                                            7     'Rop
**                                            
**    11        'Koshucode.Baala.Op.Nest      /rank /base
**                                            ----- -------------
**                                            9     'Flow
**                                            10    'Confl
**                                            11    'Rop
**                                            
**    12        'Koshucode.Baala.Op           /rank /base
**                                            ----- -------------
**                                            1     'DepRank
**                                            1     'Message
**                                            4     'Builtin
**                                            5     'Gadget
**                                            5     'Meta
**                                            5     'Source
**                                            5     'Term
**                                            6     'Peripheral
**                                            8     'Lattice
**                                            9     'Check
**                                            9     'Control
**                                            12    'Nest
**                                            
**    13        'Koshucode.Baala              /rank /base
**                                            ----- -------------
**                                            0     'Base
**                                            0     'Core
**                                            13    'Op
**                                            

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
**    65 judges
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
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Logic"              /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Order"              /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Order"              /import "Koshucode.Baala.Op.Cop.Coxhand"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop.Order"              /import "Koshucode.Baala.Op.Message"

|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.Arith"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.List"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.Logic"
|-- IMPORT  /module "Koshucode.Baala.Op.Cop"                    /import "Koshucode.Baala.Op.Cop.Order"

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

|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Calc"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Empty"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Filter"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Gadget"
|-- IMPORT  /module "Koshucode.Baala.Op.Cox"                    /import "Koshucode.Baala.Op.Cox.Get"

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

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala  /base-rank {| /rank : /base | 0 : 'Base | 0 : 'Core | 0 : 'Op |}
|-- DIR-RANK  /dir-rank 1  /dir 'Koshucode.Baala.Type  /base-rank {| /rank : /base | 1 : 'Vanilla |}
|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Op.Cop  /base-rank {| /rank : /base | 1 : 'Arith | 1 : 'Coxhand | 1 : 'Logic | 2 : 'List | 2 : 'Order |}
|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Op.Cox  /base-rank {| /rank : /base | 1 : 'Get | 2 : 'Calc | 2 : 'Empty | 2 : 'Filter | 2 : 'Gadget |}
|-- DIR-RANK  /dir-rank 4  /dir 'Koshucode.Baala.Op  /base-rank {| /rank : /base | 0 : 'Builtin | 0 : 'Lattice | 0 : 'Message | 2 : 'Quoter | 3 : 'Cop | 3 : 'Cox | 4 : 'Global |}

*** 5 judges

**  TABLE : DIR-RANK
**
**    /dir-rank /dir                      /base-rank
**    --------- ------------------------- ----------------
**    0         'Koshucode.Baala          /rank /base
**                                        ----- ----------
**                                        0     'Base
**                                        0     'Core
**                                        0     'Op
**                                        
**    1         'Koshucode.Baala.Type     /rank /base
**                                        ----- ----------
**                                        1     'Vanilla
**                                        
**    2         'Koshucode.Baala.Op.Cop   /rank /base
**                                        ----- ----------
**                                        1     'Arith
**                                        1     'Coxhand
**                                        1     'Logic
**                                        2     'List
**                                        2     'Order
**                                        
**    2         'Koshucode.Baala.Op.Cox   /rank /base
**                                        ----- ----------
**                                        1     'Get
**                                        2     'Calc
**                                        2     'Empty
**                                        2     'Filter
**                                        2     'Gadget
**                                        
**    4         'Koshucode.Baala.Op       /rank /base
**                                        ----- ----------
**                                        0     'Builtin
**                                        0     'Lattice
**                                        0     'Message
**                                        2     'Quoter
**                                        3     'Cop
**                                        3     'Cox
**                                        4     'Global
**                                        

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
**    20 judges
**

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Element"   /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "GHC.IO.Encoding"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "System.Environment"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "System.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Exit"      /import "System.IO"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "System.IO"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "System.FilePath"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "System.Directory"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Run"       /import "Koshucode.Baala.Core"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Version"   /import "Data.Version"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Library.Version"   /import "Paths_koshucode_baala_calculator"

|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "System.Console.GetOpt"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Base"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Core"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Element"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Exit"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Run"
|-- IMPORT  /module "Koshucode.Baala.Toolkit.Main.KoshuMain"    /import "Koshucode.Baala.Toolkit.Library.Version"

```

Command `./dir-rank.k ../calculator/IMPORT.k` produces:

```
** -*- koshu -*-
**
**  INPUT
**    ./dir-rank.k
**    ../calculator/IMPORT.k
**

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala  /base-rank {| /rank : /base | 0 : 'Base | 0 : 'Core |}
|-- DIR-RANK  /dir-rank 1  /dir 'Koshucode.Baala.Toolkit.Library  /base-rank {| /rank : /base | 0 : 'Exit | 0 : 'Version | 1 : 'Element | 1 : 'Run |}
|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Toolkit.Main  /base-rank {| /rank : /base | 2 : 'KoshuMain |}

*** 3 judges

**  TABLE : DIR-RANK
**
**    /dir-rank /dir                               /base-rank
**    --------- ---------------------------------- ------------------
**    0         'Koshucode.Baala                   /rank /base
**                                                 ----- ------------
**                                                 0     'Base
**                                                 0     'Core
**                                                 
**    1         'Koshucode.Baala.Toolkit.Library   /rank /base
**                                                 ----- ------------
**                                                 0     'Exit
**                                                 0     'Version
**                                                 1     'Element
**                                                 1     'Run
**                                                 
**    2         'Koshucode.Baala.Toolkit.Main      /rank /base
**                                                 ----- ------------
**                                                 2     'KoshuMain
**                                                 

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

# I/O List

- [./dir-rank.k](#dir-rankk)
- ./dir-rank.k [../base/data/IMPORT.k](#basedataimportk)
- ./dir-rank.k [../core/data/IMPORT.k](#coredataimportk)
- ./dir-rank.k [../operator/data/IMPORT.k](#operatordataimportk)
- ./dir-rank.k [../content/data/IMPORT.k](#contentdataimportk)
- ./dir-rank.k [../calculator/data/IMPORT.k](#calculatordataimportk)



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

|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Base.Prelude  /base-rank {| /rank /base /import-dir [ 0 | 'Class | [ ] ] [ 0 | 'Import | [ ] ] [ 1 | 'Assoc | [ ] ] [ 1 | 'List | [ ] ] [ 1 | 'Pair | [ ] ] [ 1 | 'Snip | [ ] ] [ 2 | 'Order | [ ] ] |}
|-- DIR-RANK  /dir-rank 5  /dir 'Koshucode.Baala.Base.Text  /base-rank {| /rank /base /import-dir [ 0 | 'Comment | [ ] ] [ 0 | 'Unicode | [ ] ] [ 4 | 'IOPoint | [ 'Prelude ] ] [ 4 | 'TextTable | [ 'Prelude ] ] [ 4 | 'Utility | [ 'Prelude ] ] [ 4 | 'Write | [ 'Prelude ] ] [ 5 | 'CodePt | [ 'Prelude ] ] [ 5 | 'Http | [ 'Prelude ] ] |}
|-- DIR-RANK  /dir-rank 10  /dir 'Koshucode.Baala.Base.Abort  /base-rank {| /rank /base /import-dir [ 7 | 'Reason | [ 'Text ] ] [ 8 | 'Abortable | [ 'Prelude | 'Text ] ] [ 8 | 'Report | [ 'Prelude | 'Text ] ] [ 10 | 'Message | [ 'Abort ] ] |}
|-- DIR-RANK  /dir-rank 12  /dir 'Koshucode.Baala.Base.Syntax  /base-rank {| /rank /base /import-dir [ 10 | 'Message | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Line | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Tree | [ 'Abort | 'Prelude | 'Text ] ] [ 12 | 'Clause | [ 'Prelude | 'Text ] ] [ 12 | 'Infix | [ 'Prelude ] ] |}
|-- DIR-RANK  /dir-rank 13  /dir 'Koshucode.Baala.Base  /base-rank {| /rank /base /import-dir [ 3 | 'Prelude | [ ] ] [ 6 | 'Text | [ ] ] [ 9 | 'Abort | [ ] ] [ 11 | 'Message | [ ] ] [ 13 | 'Syntax | [ ] ] |}

|-- DIR-RANK  /dir-rank 14  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 14 | 'Base | [ 'Abort | 'Prelude | 'Syntax | 'Text ] ] |}

*** 6 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                            /base-rank
  --------- ------------------------------- ------------------------------------------------------------
  2         'Koshucode.Baala.Base.Prelude   /rank /base        /import-dir
                                            ----- ------------ -----------------------------------------
                                            0     'Class       [ ]
                                            0     'Import      [ ]
                                            1     'Assoc       [ ]
                                            1     'List        [ ]
                                            1     'Pair        [ ]
                                            1     'Snip        [ ]
                                            2     'Order       [ ]
                                            
  5         'Koshucode.Baala.Base.Text      /rank /base        /import-dir
                                            ----- ------------ -----------------------------------------
                                            0     'Comment     [ ]
                                            0     'Unicode     [ ]
                                            4     'IOPoint     [ 'Prelude ]
                                            4     'TextTable   [ 'Prelude ]
                                            4     'Utility     [ 'Prelude ]
                                            4     'Write       [ 'Prelude ]
                                            5     'CodePt      [ 'Prelude ]
                                            5     'Http        [ 'Prelude ]
                                            
  10        'Koshucode.Baala.Base.Abort     /rank /base        /import-dir
                                            ----- ------------ -----------------------------------------
                                            7     'Reason      [ 'Text ]
                                            8     'Abortable   [ 'Prelude | 'Text ]
                                            8     'Report      [ 'Prelude | 'Text ]
                                            10    'Message     [ 'Abort ]
                                            
  12        'Koshucode.Baala.Base.Syntax    /rank /base        /import-dir
                                            ----- ------------ -----------------------------------------
                                            10    'Message     [ 'Abort | 'Prelude | 'Text ]
                                            11    'Line        [ 'Abort | 'Prelude | 'Text ]
                                            11    'Tree        [ 'Abort | 'Prelude | 'Text ]
                                            12    'Clause      [ 'Prelude | 'Text ]
                                            12    'Infix       [ 'Prelude ]
                                            
  13        'Koshucode.Baala.Base           /rank /base        /import-dir
                                            ----- ------------ -----------------------------------------
                                            3     'Prelude     [ ]
                                            6     'Text        [ ]
                                            9     'Abort       [ ]
                                            11    'Message     [ ]
                                            13    'Syntax      [ ]
                                            
  14        'Koshucode.Baala                /rank /base        /import-dir
                                            ----- ------------ -----------------------------------------
                                            14    'Base        [ 'Abort | 'Prelude | 'Syntax | 'Text ]
                                            

=== rel

**
**  SUMMARY
**       6 judges on DIR-RANK
**       6 judges in total
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

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala.Base  /base-rank {| /rank /base /import-dir [ 0 | 'Message | [ ] ] |}
|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala.Data  /base-rank {| /rank /base /import-dir [ 0 | 'Message | [ ] ] |}
|-- DIR-RANK  /dir-rank 6  /dir 'Koshucode.Baala.Core.Lexmap  /base-rank {| /rank /base /import-dir [ 1 | 'Message | [ ] ] [ 2 | 'AttrPos | [ ] ] [ 3 | 'Attr | [ ] ] [ 3 | 'Slot | [ ] ] [ 4 | 'AttrEd | [ ] ] [ 4 | 'Lexmap | [ ] ] [ 5 | 'LexmapTrees | [ ] ] [ 6 | 'Construct | [ ] ] |}
|-- DIR-RANK  /dir-rank 10  /dir 'Koshucode.Baala.Core.Relkit  /base-rank {| /rank /base /import-dir [ 1 | 'Message | [ ] ] [ 8 | 'Relkit | [ 'Lexmap ] ] [ 9 | 'Construct | [ ] ] [ 10 | 'Run | [ ] ] |}
|-- DIR-RANK  /dir-rank 14  /dir 'Koshucode.Baala.Core.Relmap  /base-rank {| /rank /base /import-dir [ 1 | 'Message | [ ] ] [ 1 | 'Result | [ ] ] [ 2 | 'Option | [ ] ] [ 12 | 'Relmap | [ 'Lexmap | 'Relkit ] ] [ 13 | 'Rop | [ 'Lexmap ] ] [ 13 | 'Specialize | [ 'Lexmap | 'Relkit ] ] [ 14 | 'Construct | [ 'Lexmap | 'Relkit ] ] [ 14 | 'Global | [ ] ] |}

|-- DIR-RANK  /dir-rank 17  /dir 'Koshucode.Baala.Core.Assert  /base-rank {| /rank /base /import-dir [ 1 | 'Message | [ ] ] [ 1 | 'RelTable | [ ] ] [ 12 | 'Dataset | [ 'Relkit ] ] [ 16 | 'Assert | [ 'Lexmap | 'Relmap ] ] [ 17 | 'Run | [ 'Lexmap | 'Relkit | 'Relmap ] ] |}
|-- DIR-RANK  /dir-rank 21  /dir 'Koshucode.Baala.Core.Resource  /base-rank {| /rank /base /import-dir [ 1 | 'Message | [ ] ] [ 8 | 'Clause | [ 'Lexmap ] ] [ 19 | 'Resource | [ 'Assert | 'Lexmap | 'Relkit | 'Relmap ] ] [ 20 | 'Include | [ 'Assert | 'Lexmap | 'Relmap ] ] [ 20 | 'Run | [ 'Assert | 'Lexmap | 'Relmap ] ] [ 21 | 'Read | [ 'Relmap ] ] |}
|-- DIR-RANK  /dir-rank 22  /dir 'Koshucode.Baala.Core  /base-rank {| /rank /base /import-dir [ 2 | 'Message | [ ] ] [ 7 | 'Lexmap | [ ] ] [ 11 | 'Relkit | [ ] ] [ 15 | 'Relmap | [ ] ] [ 18 | 'Assert | [ ] ] [ 22 | 'Resource | [ ] ] |}
|-- DIR-RANK  /dir-rank 23  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 0 | 'Base | [ ] ] [ 0 | 'Data | [ ] ] [ 23 | 'Core | [ 'Assert | 'Lexmap | 'Relkit | 'Relmap | 'Resource ] ] |}

*** 9 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                             /base-rank
  --------- -------------------------------- ----------------------------------------------------------------------------
  0         'Koshucode.Baala.Base            /rank /base          /import-dir
                                             ----- -------------- -------------------------------------------------------
                                             0     'Message       [ ]
                                             
  0         'Koshucode.Baala.Data            /rank /base          /import-dir
                                             ----- -------------- -------------------------------------------------------
                                             0     'Message       [ ]
                                             
  6         'Koshucode.Baala.Core.Lexmap     /rank /base          /import-dir
                                             ----- -------------- -------------------------------------------------------
                                             1     'Message       [ ]
                                             2     'AttrPos       [ ]
                                             3     'Attr          [ ]
                                             3     'Slot          [ ]
                                             4     'AttrEd        [ ]
                                             4     'Lexmap        [ ]
                                             5     'LexmapTrees   [ ]
                                             6     'Construct     [ ]
                                             
  10        'Koshucode.Baala.Core.Relkit     /rank /base          /import-dir
                                             ----- -------------- -------------------------------------------------------
                                             1     'Message       [ ]
                                             8     'Relkit        [ 'Lexmap ]
                                             9     'Construct     [ ]
                                             10    'Run           [ ]
                                             
  14        'Koshucode.Baala.Core.Relmap     /rank /base          /import-dir
                                             ----- -------------- -------------------------------------------------------
                                             1     'Message       [ ]
                                             1     'Result        [ ]
                                             2     'Option        [ ]
                                             12    'Relmap        [ 'Lexmap | 'Relkit ]
                                             13    'Rop           [ 'Lexmap ]
                                             13    'Specialize    [ 'Lexmap | 'Relkit ]
                                             14    'Construct     [ 'Lexmap | 'Relkit ]
                                             14    'Global        [ ]
                                             
  17        'Koshucode.Baala.Core.Assert     /rank /base          /import-dir
                                             ----- -------------- -------------------------------------------------------
                                             1     'Message       [ ]
                                             1     'RelTable      [ ]
                                             12    'Dataset       [ 'Relkit ]
                                             16    'Assert        [ 'Lexmap | 'Relmap ]
                                             17    'Run           [ 'Lexmap | 'Relkit | 'Relmap ]
                                             
  21        'Koshucode.Baala.Core.Resource   /rank /base          /import-dir
                                             ----- -------------- -------------------------------------------------------
                                             1     'Message       [ ]
                                             8     'Clause        [ 'Lexmap ]
                                             19    'Resource      [ 'Assert | 'Lexmap | 'Relkit | 'Relmap ]
                                             20    'Include       [ 'Assert | 'Lexmap | 'Relmap ]
                                             20    'Run           [ 'Assert | 'Lexmap | 'Relmap ]
                                             21    'Read          [ 'Relmap ]
                                             
  22        'Koshucode.Baala.Core            /rank /base          /import-dir
                                             ----- -------------- -------------------------------------------------------
                                             2     'Message       [ ]
                                             7     'Lexmap        [ ]
                                             11    'Relkit        [ ]
                                             15    'Relmap        [ ]
                                             18    'Assert        [ ]
                                             22    'Resource      [ ]
                                             
  23        'Koshucode.Baala                 /rank /base          /import-dir
                                             ----- -------------- -------------------------------------------------------
                                             0     'Base          [ ]
                                             0     'Data          [ ]
                                             23    'Core          [ 'Assert | 'Lexmap | 'Relkit | 'Relmap | 'Resource ]
                                             

=== rel

**
**  SUMMARY
**       9 judges on DIR-RANK
**       9 judges in total
**
```



## ../operator/data/IMPORT.k


Command `./dir-rank.k ../operator/data/IMPORT.k` exits with 2 and produces:

```
**
**  ABORTED  File not found
**  -------- ------------------------- -
**  Detail   ../operator/data/IMPORT.k
**  Command  koshu
**           ./dir-rank.k
**           ../operator/data/IMPORT.k
**
**  Exit with status 2
**
```



## ../content/data/IMPORT.k


Command `./dir-rank.k ../content/data/IMPORT.k` exits with 2 and produces:

```
**
**  ABORTED  File not found
**  -------- ------------------------ -
**  Detail   ../content/data/IMPORT.k
**  Command  koshu
**           ./dir-rank.k
**           ../content/data/IMPORT.k
**
**  Exit with status 2
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

|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 0 | 'Base | [ ] ] [ 0 | 'Cop | [ ] ] [ 0 | 'Core | [ ] ] [ 0 | 'Data | [ ] ] [ 0 | 'Writer | [ ] ] |}
|-- DIR-RANK  /dir-rank 0  /dir 'Koshucode.Baala.Rop  /base-rank {| /rank /base /import-dir [ 0 | 'Cox | [ ] ] [ 0 | 'Flat | [ ] ] [ 0 | 'Nest | [ ] ] |}
|-- DIR-RANK  /dir-rank 1  /dir 'Koshucode.Baala.Toolkit.Library  /base-rank {| /rank /base /import-dir [ 0 | 'SimpleOption | [ ] ] [ 1 | 'Element | [ ] ] [ 1 | 'Exit | [ ] ] [ 1 | 'Global | [ ] ] [ 1 | 'Run | [ ] ] |}
|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Toolkit.Main  /base-rank {| /rank /base /import-dir [ 2 | 'KoshuFilter | [ ] ] [ 2 | 'KoshuMain | [ ] ] |}

*** 4 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                               /base-rank
  --------- ---------------------------------- ---------------------------------
  0         'Koshucode.Baala                   /rank /base           /import-dir
                                               ----- --------------- -----------
                                               0     'Base           [ ]
                                               0     'Cop            [ ]
                                               0     'Core           [ ]
                                               0     'Data           [ ]
                                               0     'Writer         [ ]
                                               
  0         'Koshucode.Baala.Rop               /rank /base           /import-dir
                                               ----- --------------- -----------
                                               0     'Cox            [ ]
                                               0     'Flat           [ ]
                                               0     'Nest           [ ]
                                               
  1         'Koshucode.Baala.Toolkit.Library   /rank /base           /import-dir
                                               ----- --------------- -----------
                                               0     'SimpleOption   [ ]
                                               1     'Element        [ ]
                                               1     'Exit           [ ]
                                               1     'Global         [ ]
                                               1     'Run            [ ]
                                               
  2         'Koshucode.Baala.Toolkit.Main      /rank /base           /import-dir
                                               ----- --------------- -----------
                                               2     'KoshuFilter    [ ]
                                               2     'KoshuMain      [ ]
                                               

=== rel

**
**  SUMMARY
**       4 judges on DIR-RANK
**       4 judges in total
**
```



## command

This document is produced by the command:

```
koshu-inout.sh -o DIR-RANK.md -f PATH ./dir-rank.k
```

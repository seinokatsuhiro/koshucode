** -*- koshu -*-
**
**  INPUT
**    dir-rank.k
**    ../base/data/IMPORT.k
**    ../calculator/data/IMPORT.k
**    ../cop/data/IMPORT.k
**    ../core/data/IMPORT.k
**    ../data/data/IMPORT.k
**    ../rop-cox/data/IMPORT.k
**    ../rop-flat/data/IMPORT.k
**    ../rop-nested/data/IMPORT.k
**    ../syntax/data/IMPORT.k
**    ../toolkit/data/IMPORT.k
**    ../writer/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Case" | [ ]
    ] [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Pair" | [ ] ] [ 1 | "Queue" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Split" | [ ] ]
    [ 1 | "Dots" | [ ] ] [ 3 | "Assoc" | [ "Prelude" ] ] [ 3 | "List" | [ "Prelude" ] ] [ 3 | "Snip" | [ "Prelude" ] ] [
    4 | "Order" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ]
    ] [ 0 | "LineBreak" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 3 | "Utility" | [ "Prelude" ] ] [ 4 | "MixText" | [ ] ] [ 5 |
    "MixClass" | [ ] ] [ 6 | "TextTable" | [ "List" | "Prelude" ] ] [ 6 | "ToDoc" | [ "List" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" |
    [ ] ] [ 3 | "BzFile" | [ "Prelude" ] ] [ 3 | "Exit" | [ "Prelude" ] ] [ 4 | "Http" | [ "Prelude" ] ] [ 6 | "IOPoint"
    | [ "List" | "Prelude" ] ] [ 7 | "CodePt" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 9 | "Reason" | [
    "IO" ] ] [ 10 | "Abortable" | [ "IO" | "Prelude" ] ] [ 10 | "Report" | [ "IO" | "List" | "Prelude" | "Text" ] ] [ 12
    | "Message" | [ "Abort" ] ] =}

|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Base.Syntax"  /base-rank {= /rank /base /import-dir [ 12 | "Message" |
    [ "Abort" | "IO" | "Prelude" ] ] [ 13 | "Line" | [ "Abort" | "IO" | "Prelude" ] ] [ 13 | "Tree" | [ "Abort" | "IO" |
    "List" | "Prelude" ] ] [ 14 | "Clause" | [ "IO" | "List" | "Prelude" ] ] [ 14 | "Infix" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 15  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 2 | "Prelude" | [ ] ] [
    5 | "List" | [ ] ] [ 7 | "Text" | [ ] ] [ 8 | "IO" | [ ] ] [ 11 | "Abort" | [ ] ] [ 13 | "Message" | [ ] ] [ 15 |
    "Syntax" | [ ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 17 | "Para" | [
    "Base" ] ] [ 18 | "Get" | [ "Base" ] ] [ 18 | "ParaSpec" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 19  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 0 | "Term" | [
    ] ] [ 17 | "AngleText" | [ "Base" ] ] [ 17 | "Message" | [ "Base" ] ] [ 18 | "Next" | [ "Base" ] ] [ 19 | "Short" |
    [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 20  /dir "Koshucode.Baala.Data.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 17 |
    "Decimal" | [ "Base" ] ] [ 17 | "Rational" | [ "Base" ] ] [ 18 | "Fraction" | [ "Base" ] ] [ 19 | "Coder" | [ "Base"
    ] ] [ 20 | "BinaryAb" | [ "Base" ] ] =}

|-- DIR-RANK  /dir-rank 20  /dir "Koshucode.Baala.Data.Type.Time"  /base-rank {= /rank /base /import-dir [ 18 | "Date" |
    [ "Base" ] ] [ 19 | "Clock" | [ "Base" ] ] [ 20 | "Time" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 24  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 17 | "Message"
    | [ "Base" ] ] [ 21 | "Token" | [ "Base" | "Symbol" ] ] [ 22 | "Pattern" | [ ] ] [ 22 | "Utility" | [ "Base" ] ] [
    23 | "TokenLine" | [ "Base" | "Symbol" ] ] [ 24 | "TokenClause" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 28  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 26 | "Bracket"
    | [ "Base" | "Token" ] ] [ 27 | "TokenTree" | [ "Base" | "Token" ] ] [ 28 | "Parse" | [ "Base" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 32  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 30 | "Message" | [ "Base" | "Para" | "Symbol" | "TTree" ] ] [ 31 | "Attr" | [ "Base" | "Para" | "TTree" ] ]
    [ 31 | "Slot" | [ "Base" | "TTree" | "Token" ] ] [ 32 | "AttrEd" | [ "Base" | "TTree" | "Token" ] ] [ 32 | "Parse" |
    [ "Base" | "Para" ] ] =}
|-- DIR-RANK  /dir-rank 33  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 19 | "Para" | [ ] ] [
    20 | "Symbol" | [ ] ] [ 25 | "Token" | [ ] ] [ 29 | "TTree" | [ ] ] [ 31 | "Message" | [ ] ] [ 33 | "Attr" | [ ] ]
    =}

|-- DIR-RANK  /dir-rank 39  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 37 | "Head" |
    [ "Base" | "Syntax" ] ] [ 38 | "Rel" | [ "Base" | "Syntax" ] ] [ 39 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 40  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 17 | "Message" | [
    "Base" ] ] [ 21 | "Decimal" | [ ] ] [ 21 | "Time" | [ ] ] [ 35 | "Judge" | [ "Base" | "Syntax" ] ] [ 36 | "Interp" |
    [ "Base" | "Syntax" ] ] [ 36 | "Type" | [ "Base" | "Syntax" ] ] [ 40 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 45  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 35 | "Lexmap" |
    [ "Base" | "Syntax" ] ] [ 44 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [ 44 | "Message" | [ "Base" | "Syntax" ] ] [
    45 | "Construct" | [ "Base" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 45  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 35 | "Message"
    | [ "Base" | "Syntax" ] ] [ 42 | "Singleton" | [ "Base" | "Type" ] ] [ 42 | "Tree" | [ "Base" | "Syntax" | "Type" ]
    ] [ 43 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 43 | "Simple" | [ "Base" | "Type" ] ] [ 44 | "Utility" | [
    "Base" | "Syntax" | "Type" ] ] [ 45 | "BaalaC" | [ "Base" | "Syntax" | "Type" ] ] [ 45 | "Decode" | [ "Base" |
    "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 48  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 42 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 43 | "Cox" | [ "Base" | "Syntax" ] ] [ 44 | "Cop" | [ "Base" | "Syntax" ] ] [ 47
    | "Build" | [ "Base" | "Content" | "Syntax" ] ] [ 48 | "Run" | [ "Base" | "Content" | "Syntax" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 49  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 41 | "Type" | [
    "Decimal" | "Rel" | "Time" ] ] [ 43 | "Message" | [ ] ] [ 46 | "Content" | [ ] ] [ 49 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 53  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 35 | "Message" |
    [ "Base" | "Syntax" ] ] [ 51 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 52 | "Construct" | [ "Base"
    | "Data" | "Syntax" ] ] [ 53 | "Run" | [ "Base" | "Data" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 57  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 51 | "Message" |
    [ "Base" | "Data" | "Syntax" ] ] [ 51 | "Result" | [ "Base" | "Data" | "Syntax" ] ] [ 52 | "Option" | [ "Base" |
    "Data" | "Syntax" ] ] [ 55 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 56 | "Rop" | [
    "Base" | "Lexmap" | "Syntax" ] ] [ 56 | "Specialize" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 57 |
    "Construct" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 57 | "Global" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 60  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 35 | "Message" |
    [ "Base" | "Syntax" ] ] [ 51 | "RelTable" | [ "Base" | "Data" | "Syntax" ] ] [ 55 | "Dataset" | [ "Base" | "Data" |
    "Relkit" | "Syntax" ] ] [ 59 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 60 | "Run" | [
    "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 64  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 17 | "Message"
    | [ "Base" ] ] [ 51 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 62 | "Resource" | [ "Assert" | "Base"
    | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] [ 63 | "Concrete" | [ "Data" | "Relmap" | "Syntax" ] ] [ 63
    | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 63 | "Run" | [ "Assert" | "Base" |
    "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 64 | "Read" | [ "Base" | "Data" | "Relmap" | "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 65  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 46 | "Lexmap" | [ ] ] [
    52 | "Message" | [ ] ] [ 54 | "Relkit" | [ ] ] [ 58 | "Relmap" | [ ] ] [ 61 | "Assert" | [ ] ] [ 65 | "Resource" | [
    ] ] =}
|-- DIR-RANK  /dir-rank 68  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 53 | "Message" | [
    "Base" ] ] [ 54 | "Term" | [ "Base" | "Data" | "Syntax" ] ] [ 67 | "Define" | [ "Core" | "Syntax" ] ] [ 67 | "Get" |
    [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 68 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 68  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 67 | "Csv" | [ "Base"
    | "Core" | "Data" | "Syntax" ] ] [ 67 | "Html" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 67 | "Json" | [ "Core"
    | "Data" | "Syntax" ] ] [ 67 | "Judge" | [ "Base" | "Core" | "Data" ] ] [ 68 | "Koshu" | [ "Base" | "Core" |
    "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    51 | "Coxhand" | [ "Base" | "Data" | "Syntax" ] ] [ 52 | "Logic" | [ "Base" | "Data" ] ] [ 68 | "Message" | [ ] ] [
    69 | "Arith" | [ "Base" | "Data" ] ] [ 69 | "List" | [ "Base" | "Data" ] ] [ 69 | "Misc" | [ "Base" | "Data" |
    "Syntax" ] ] [ 69 | "Order" | [ "Base" | "Data" ] ] [ 69 | "Text" | [ "Base" | "Data" | "Syntax" ] ] [ 69 | "Time" |
    [ "Base" | "Data" ] ] [ 69 | "Type" | [ "Base" | "Data" ] ] [ 70 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 71  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 16 | "Base" | [ "Abort" |
    "IO" | "List" | "Prelude" | "Syntax" | "Text" ] ] [ 34 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "TTree" | "Token"
    ] ] [ 50 | "Data" | [ "Church" | "Content" | "Type" ] ] [ 66 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap"
    | "Resource" ] ] [ 69 | "Writer" | [ ] ] [ 71 | "Cop" | [ ] ] =}

|-- DIR-RANK  /dir-rank 71  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 67 | "KoshuRdf"
    | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 67 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 |
    "KoshuChange" | [ "Base" ] ] [ 71 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 71 | "KoshuMain" | [ "Base" |
    "Core" | "Data" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 72  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 70 |
    "Tropashko" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "Restrict" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 72 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 75  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 17 | "PoScale" | [
    "Base" ] ] [ 67 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Elem" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 70 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Meta" | [ "Base"
    | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Order" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 |
    "Resource" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Source" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 70 | "Term" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "Peripheral" | [
    "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "TermGadget" | [ "Base" | "Core" | "Data" ] ] [ 73 |
    "Lattice" | [ ] ] [ 74 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ] ] [ 74 | "Control" |
    [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 75 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 68 | "Message" | [ ] ] [ 70 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "Accessor" | [
    "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ]
    [ 71 | "Filter" | [ "Base" | "Core" | "Data" ] ] [ 71 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
    ] [ 71 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 77 | "Empty" | [ "Base" | "Base" | "Core" |
    "Data" | "Flat" ] ] [ 78 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 68 | "Message" | [
    "Base" | "Data" | "Syntax" ] ] [ 70 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 77 | "Confl" | [
    "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ] ] [ 78 | "Deriv" | [ "Base" | "Base" | "Core" | "Data" |
    "Flat" | "Syntax" ] ] [ 79 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 69 | "Base" | [ ] ] [ 76
    | "Flat" | [ "Lattice" ] ] [ 79 | "Cox" | [ ] ] [ 80 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 51 | "RDF" | [ "Data" ] ] [ 67 | "Element" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 67 |
    "Input" | [ "Base" | "Core" ] ] [ 70 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 70 | "Run" | [ "Base"
    | "Core" | "Data" | "Writer" ] ] [ 81 | "Global" | [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ] ]
    =}

*** 37 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                  /base-rank
  --------- ------------------------------------- ---------------------------------------------------------------------------------------------------
  1         "Koshucode.Baala.Base.Prelude"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Case"           [ ]
                                                  0     "Class"          [ ]
                                                  0     "Import"         [ ]
                                                  1     "Pair"           [ ]
                                                  1     "Queue"          [ ]
                                                  
  4         "Koshucode.Baala.Base.List"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Split"          [ ]
                                                  1     "Dots"           [ ]
                                                  3     "Assoc"          [ "Prelude" ]
                                                  3     "List"           [ "Prelude" ]
                                                  3     "Snip"           [ "Prelude" ]
                                                  4     "Order"          [ "Prelude" ]
                                                  
  6         "Koshucode.Baala.Base.Text"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Comment"        [ ]
                                                  0     "LineBreak"      [ ]
                                                  0     "Unicode"        [ ]
                                                  3     "Utility"        [ "Prelude" ]
                                                  4     "MixText"        [ ]
                                                  5     "MixClass"       [ ]
                                                  6     "TextTable"      [ "List" | "Prelude" ]
                                                  6     "ToDoc"          [ "List" | "Prelude" ]
                                                  
  7         "Koshucode.Baala.Base.IO"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "SimpleOption"   [ ]
                                                  3     "BzFile"         [ "Prelude" ]
                                                  3     "Exit"           [ "Prelude" ]
                                                  4     "Http"           [ "Prelude" ]
                                                  6     "IOPoint"        [ "List" | "Prelude" ]
                                                  7     "CodePt"         [ "Prelude" ]
                                                  
  12        "Koshucode.Baala.Base.Abort"          /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  9     "Reason"         [ "IO" ]
                                                  10    "Abortable"      [ "IO" | "Prelude" ]
                                                  10    "Report"         [ "IO" | "List" | "Prelude" | "Text" ]
                                                  12    "Message"        [ "Abort" ]
                                                  
  14        "Koshucode.Baala.Base.Syntax"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  12    "Message"        [ "Abort" | "IO" | "Prelude" ]
                                                  13    "Line"           [ "Abort" | "IO" | "Prelude" ]
                                                  13    "Tree"           [ "Abort" | "IO" | "List" | "Prelude" ]
                                                  14    "Clause"         [ "IO" | "List" | "Prelude" ]
                                                  14    "Infix"          [ "Prelude" ]
                                                  
  15        "Koshucode.Baala.Base"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  2     "Prelude"        [ ]
                                                  5     "List"           [ ]
                                                  7     "Text"           [ ]
                                                  8     "IO"             [ ]
                                                  11    "Abort"          [ ]
                                                  13    "Message"        [ ]
                                                  15    "Syntax"         [ ]
                                                  
  18        "Koshucode.Baala.Syntax.Para"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Para"           [ "Base" ]
                                                  18    "Get"            [ "Base" ]
                                                  18    "ParaSpec"       [ "Base" ]
                                                  
  19        "Koshucode.Baala.Syntax.Symbol"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Term"           [ ]
                                                  17    "AngleText"      [ "Base" ]
                                                  17    "Message"        [ "Base" ]
                                                  18    "Next"           [ "Base" ]
                                                  19    "Short"          [ "Base" ]
                                                  
  20        "Koshucode.Baala.Data.Type.Decimal"   /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Decimal"        [ "Base" ]
                                                  17    "Rational"       [ "Base" ]
                                                  18    "Fraction"       [ "Base" ]
                                                  19    "Coder"          [ "Base" ]
                                                  20    "BinaryAb"       [ "Base" ]
                                                  
  20        "Koshucode.Baala.Data.Type.Time"      /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  18    "Date"           [ "Base" ]
                                                  19    "Clock"          [ "Base" ]
                                                  20    "Time"           [ "Base" ]
                                                  
  24        "Koshucode.Baala.Syntax.Token"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  21    "Token"          [ "Base" | "Symbol" ]
                                                  22    "Pattern"        [ ]
                                                  22    "Utility"        [ "Base" ]
                                                  23    "TokenLine"      [ "Base" | "Symbol" ]
                                                  24    "TokenClause"    [ "Base" ]
                                                  
  28        "Koshucode.Baala.Syntax.TTree"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  26    "Bracket"        [ "Base" | "Token" ]
                                                  27    "TokenTree"      [ "Base" | "Token" ]
                                                  28    "Parse"          [ "Base" | "Token" ]
                                                  
  32        "Koshucode.Baala.Syntax.Attr"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "AttrName"       [ ]
                                                  30    "Message"        [ "Base" | "Para" | "Symbol" | "TTree" ]
                                                  31    "Attr"           [ "Base" | "Para" | "TTree" ]
                                                  31    "Slot"           [ "Base" | "TTree" | "Token" ]
                                                  32    "AttrEd"         [ "Base" | "TTree" | "Token" ]
                                                  32    "Parse"          [ "Base" | "Para" ]
                                                  
  33        "Koshucode.Baala.Syntax"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  19    "Para"           [ ]
                                                  20    "Symbol"         [ ]
                                                  25    "Token"          [ ]
                                                  29    "TTree"          [ ]
                                                  31    "Message"        [ ]
                                                  33    "Attr"           [ ]
                                                  
  39        "Koshucode.Baala.Data.Type.Rel"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  37    "Head"           [ "Base" | "Syntax" ]
                                                  38    "Rel"            [ "Base" | "Syntax" ]
                                                  39    "Mono"           [ "Base" ]
                                                  
  40        "Koshucode.Baala.Data.Type"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  21    "Decimal"        [ ]
                                                  21    "Time"           [ ]
                                                  35    "Judge"          [ "Base" | "Syntax" ]
                                                  36    "Interp"         [ "Base" | "Syntax" ]
                                                  36    "Type"           [ "Base" | "Syntax" ]
                                                  40    "Rel"            [ ]
                                                  
  45        "Koshucode.Baala.Core.Lexmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  35    "Lexmap"         [ "Base" | "Syntax" ]
                                                  44    "LexmapTrees"    [ "Base" | "Syntax" ]
                                                  44    "Message"        [ "Base" | "Syntax" ]
                                                  45    "Construct"      [ "Base" | "Syntax" ]
                                                  
  45        "Koshucode.Baala.Data.Content"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  35    "Message"        [ "Base" | "Syntax" ]
                                                  42    "Singleton"      [ "Base" | "Type" ]
                                                  42    "Tree"           [ "Base" | "Syntax" | "Type" ]
                                                  43    "Complex"        [ "Base" | "Syntax" | "Type" ]
                                                  43    "Simple"         [ "Base" | "Type" ]
                                                  44    "Utility"        [ "Base" | "Syntax" | "Type" ]
                                                  45    "BaalaC"         [ "Base" | "Syntax" | "Type" ]
                                                  45    "Decode"         [ "Base" | "Syntax" | "Type" ]
                                                  
  48        "Koshucode.Baala.Data.Church"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  42    "Message"        [ "Base" | "Syntax" | "Type" ]
                                                  43    "Cox"            [ "Base" | "Syntax" ]
                                                  44    "Cop"            [ "Base" | "Syntax" ]
                                                  47    "Build"          [ "Base" | "Content" | "Syntax" ]
                                                  48    "Run"            [ "Base" | "Content" | "Syntax" | "Type" ]
                                                  
  49        "Koshucode.Baala.Data"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  41    "Type"           [ "Decimal" | "Rel" | "Time" ]
                                                  43    "Message"        [ ]
                                                  46    "Content"        [ ]
                                                  49    "Church"         [ ]
                                                  
  53        "Koshucode.Baala.Core.Relkit"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  35    "Message"        [ "Base" | "Syntax" ]
                                                  51    "Relkit"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  52    "Construct"      [ "Base" | "Data" | "Syntax" ]
                                                  53    "Run"            [ "Base" | "Data" | "Syntax" ]
                                                  
  57        "Koshucode.Baala.Core.Relmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  51    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  51    "Result"         [ "Base" | "Data" | "Syntax" ]
                                                  52    "Option"         [ "Base" | "Data" | "Syntax" ]
                                                  55    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  56    "Rop"            [ "Base" | "Lexmap" | "Syntax" ]
                                                  56    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  57    "Construct"      [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  57    "Global"         [ "Base" | "Data" ]
                                                  
  60        "Koshucode.Baala.Core.Assert"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  35    "Message"        [ "Base" | "Syntax" ]
                                                  51    "RelTable"       [ "Base" | "Data" | "Syntax" ]
                                                  55    "Dataset"        [ "Base" | "Data" | "Relkit" | "Syntax" ]
                                                  59    "Assert"         [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  60    "Run"            [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  64        "Koshucode.Baala.Core.Resource"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  51    "Clause"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  62    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  63    "Concrete"       [ "Data" | "Relmap" | "Syntax" ]
                                                  63    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  63    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  64    "Read"           [ "Base" | "Data" | "Relmap" | "Syntax" ]
                                                  
  65        "Koshucode.Baala.Core"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  46    "Lexmap"         [ ]
                                                  52    "Message"        [ ]
                                                  54    "Relkit"         [ ]
                                                  58    "Relmap"         [ ]
                                                  61    "Assert"         [ ]
                                                  65    "Resource"       [ ]
                                                  
  68        "Koshucode.Baala.Rop.Base"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  53    "Message"        [ "Base" ]
                                                  54    "Term"           [ "Base" | "Data" | "Syntax" ]
                                                  67    "Define"         [ "Core" | "Syntax" ]
                                                  67    "Get"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  68    "Rop"            [ "Base" | "Core" ]
                                                  
  68        "Koshucode.Baala.Writer"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  67    "Csv"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  67    "Html"           [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  67    "Json"           [ "Core" | "Data" | "Syntax" ]
                                                  67    "Judge"          [ "Base" | "Core" | "Data" ]
                                                  68    "Koshu"          [ "Base" | "Core" | "Syntax" ]
                                                  
  70        "Koshucode.Baala.Cop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Replace"        [ ]
                                                  51    "Coxhand"        [ "Base" | "Data" | "Syntax" ]
                                                  52    "Logic"          [ "Base" | "Data" ]
                                                  68    "Message"        [ ]
                                                  69    "Arith"          [ "Base" | "Data" ]
                                                  69    "List"           [ "Base" | "Data" ]
                                                  69    "Misc"           [ "Base" | "Data" | "Syntax" ]
                                                  69    "Order"          [ "Base" | "Data" ]
                                                  69    "Text"           [ "Base" | "Data" | "Syntax" ]
                                                  69    "Time"           [ "Base" | "Data" ]
                                                  69    "Type"           [ "Base" | "Data" ]
                                                  70    "Bundle"         [ "Base" | "Data" ]
                                                  
  71        "Koshucode.Baala"                     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  16    "Base"           [ "Abort" | "IO" | "List" | "Prelude" | "Syntax" | "Text" ]
                                                  34    "Syntax"         [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                                  50    "Data"           [ "Church" | "Content" | "Type" ]
                                                  66    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  69    "Writer"         [ ]
                                                  71    "Cop"            [ ]
                                                  
  71        "Koshucode.Baala.Toolkit.Main"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  67    "KoshuRdf"       [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  67    "KoshuSyntax"    [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "KoshuChange"    [ "Base" ]
                                                  71    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                  71    "KoshuMain"      [ "Base" | "Core" | "Data" | "Writer" ]
                                                  
  72        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  70    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Restrict"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Rop"            [ "Base" | "Core" ]
                                                  
  75        "Koshucode.Baala.Rop.Flat"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "PoScale"        [ "Base" ]
                                                  67    "Message"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Elem"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Meta"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Order"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Resource"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Source"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Term"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                  73    "Lattice"        [ ]
                                                  74    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  74    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                  75    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  78        "Koshucode.Baala.Rop.Cox"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "GeoDatumJp"     [ ]
                                                  68    "Message"        [ ]
                                                  70    "Get"            [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Accessor"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Calc"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Filter"         [ "Base" | "Core" | "Data" ]
                                                  71    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Range"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  77    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                  78    "Bundle"         [ "Core" | "Data" ]
                                                  
  79        "Koshucode.Baala.Rop.Nest"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  68    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  70    "Flow"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  77    "Confl"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  78    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  79    "Rop"            [ "Base" | "Core" | "Data" ]
                                                  
  80        "Koshucode.Baala.Rop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  69    "Base"           [ ]
                                                  76    "Flat"           [ "Lattice" ]
                                                  79    "Cox"            [ ]
                                                  80    "Nest"           [ ]
                                                  
  81        "Koshucode.Baala.Toolkit.Library"     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Version"        [ ]
                                                  51    "RDF"            [ "Data" ]
                                                  67    "Element"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  67    "Input"          [ "Base" | "Core" ]
                                                  70    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                  70    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                  81    "Global"         [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      37 judges on DIR-RANK
**      37 judges in total
**

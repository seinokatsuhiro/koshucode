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
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Set" | [ ] ] [
    0 | "Split" | [ ] ] [ 1 | "Dots" | [ ] ] [ 3 | "Assoc" | [ "Prelude" ] ] [ 3 | "List" | [ "Prelude" ] ] [ 3 | "Snip"
    | [ "Prelude" ] ] [ 4 | "Order" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ]
    ] [ 0 | "LineBreak" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 3 | "Utility" | [ "Prelude" ] ] [ 4 | "MixText" | [ ] ] [ 5 |
    "MixClass" | [ ] ] [ 6 | "PPrint" | [ "List" | "Prelude" ] ] [ 6 | "TextTable" | [ "List" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" |
    [ ] ] [ 3 | "BzFile" | [ "Prelude" ] ] [ 3 | "Exit" | [ "Prelude" ] ] [ 4 | "Http" | [ "Prelude" ] ] [ 6 | "IOPoint"
    | [ "List" | "Prelude" ] ] [ 7 | "CodePt" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 9 | "Reason" | [
    "IO" ] ] [ 10 | "Abortable" | [ "IO" | "Prelude" ] ] [ 10 | "Report" | [ "IO" | "List" | "Prelude" | "Text" ] ] [ 12
    | "Message" | [ "Abort" ] ] =}

|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Base.Syntax"  /base-rank {= /rank /base /import-dir [ 9 | "Line" | [
    "IO" | "Prelude" ] ] [ 10 | "Clause" | [ "IO" | "List" | "Prelude" ] ] [ 12 | "Message" | [ "Abort" | "IO" |
    "Prelude" ] ] [ 13 | "Tree" | [ "Abort" | "IO" | "List" | "Prelude" ] ] [ 14 | "Infix" | [ "Prelude" ] ] =}
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
|-- DIR-RANK  /dir-rank 25  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 17 | "Message"
    | [ "Base" ] ] [ 21 | "Token" | [ "Base" | "Symbol" ] ] [ 22 | "Pattern" | [ ] ] [ 22 | "Utility" | [ "Base" ] ] [
    23 | "Section" | [ "Base" | "Symbol" ] ] [ 24 | "Rel" | [ "Base" | "Symbol" ] ] [ 25 | "Clause" | [ "Base" |
    "Symbol" ] ] =}
|-- DIR-RANK  /dir-rank 29  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 27 | "Bracket"
    | [ "Base" | "Token" ] ] [ 28 | "TokenTree" | [ "Base" | "Token" ] ] [ 29 | "Parse" | [ "Base" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 33  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 31 | "Message" | [ "Base" | "Para" | "Symbol" | "TTree" ] ] [ 32 | "Attr" | [ "Base" | "Para" | "TTree" ] ]
    [ 32 | "Slot" | [ "Base" | "TTree" | "Token" ] ] [ 33 | "AttrEd" | [ "Base" | "TTree" | "Token" ] ] [ 33 | "Parse" |
    [ "Base" | "Para" ] ] =}
|-- DIR-RANK  /dir-rank 34  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 19 | "Para" | [ ] ] [
    20 | "Symbol" | [ ] ] [ 26 | "Token" | [ ] ] [ 30 | "TTree" | [ ] ] [ 32 | "Message" | [ ] ] [ 34 | "Attr" | [ ] ]
    =}

|-- DIR-RANK  /dir-rank 40  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 38 | "Head" |
    [ "Base" | "Syntax" ] ] [ 39 | "Rel" | [ "Base" | "Syntax" ] ] [ 40 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 41  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 17 | "Message" | [
    "Base" ] ] [ 21 | "Decimal" | [ ] ] [ 21 | "Time" | [ ] ] [ 36 | "Judge" | [ "Base" | "Syntax" ] ] [ 37 | "Interp" |
    [ "Base" | "Syntax" ] ] [ 37 | "Type" | [ "Base" | "Syntax" ] ] [ 41 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 46  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 36 | "Lexmap" |
    [ "Base" | "Syntax" ] ] [ 45 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [ 45 | "Message" | [ "Base" | "Syntax" ] ] [
    46 | "Construct" | [ "Base" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 46  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 36 | "Message"
    | [ "Base" | "Syntax" ] ] [ 43 | "Singleton" | [ "Base" | "Type" ] ] [ 43 | "Tree" | [ "Base" | "Syntax" | "Type" ]
    ] [ 44 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 44 | "Simple" | [ "Base" | "Type" ] ] [ 45 | "Utility" | [
    "Base" | "Syntax" | "Type" ] ] [ 46 | "BaalaC" | [ "Base" | "Syntax" | "Type" ] ] [ 46 | "Decode" | [ "Base" |
    "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 49  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 43 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 44 | "Cox" | [ "Base" | "Syntax" ] ] [ 45 | "Cop" | [ "Base" | "Syntax" ] ] [ 48
    | "Build" | [ "Base" | "Content" | "Syntax" ] ] [ 49 | "Run" | [ "Base" | "Content" | "Syntax" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 50  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 42 | "Type" | [
    "Decimal" | "Rel" | "Time" ] ] [ 44 | "Message" | [ ] ] [ 47 | "Content" | [ ] ] [ 50 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 54  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 36 | "Message" |
    [ "Base" | "Syntax" ] ] [ 52 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 53 | "Construct" | [ "Base"
    | "Data" | "Syntax" ] ] [ 54 | "Run" | [ "Base" | "Data" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 58  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 52 | "Message" |
    [ "Base" | "Data" | "Syntax" ] ] [ 52 | "Result" | [ "Base" | "Data" | "Syntax" ] ] [ 53 | "Option" | [ "Base" |
    "Data" | "Syntax" ] ] [ 56 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 57 | "Rop" | [
    "Base" | "Lexmap" | "Syntax" ] ] [ 57 | "Specialize" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 58 |
    "Construct" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 58 | "Global" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 36 | "Message" |
    [ "Base" | "Syntax" ] ] [ 52 | "RelTable" | [ "Base" | "Data" | "Syntax" ] ] [ 56 | "Dataset" | [ "Base" | "Data" |
    "Relkit" | "Syntax" ] ] [ 60 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 61 | "Run" | [
    "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 65  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 17 | "Message"
    | [ "Base" ] ] [ 52 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 63 | "Resource" | [ "Assert" | "Base"
    | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] [ 64 | "Concrete" | [ "Data" | "Relmap" | "Syntax" ] ] [ 64
    | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 64 | "Run" | [ "Assert" | "Base" |
    "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 65 | "Read" | [ "Base" | "Data" | "Relmap" | "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 66  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 47 | "Lexmap" | [ ] ] [
    53 | "Message" | [ ] ] [ 55 | "Relkit" | [ ] ] [ 59 | "Relmap" | [ ] ] [ 62 | "Assert" | [ ] ] [ 66 | "Resource" | [
    ] ] =}
|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 54 | "Message" | [
    "Base" ] ] [ 55 | "Term" | [ "Base" | "Data" | "Syntax" ] ] [ 68 | "Define" | [ "Core" | "Syntax" ] ] [ 68 | "Get" |
    [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 68 | "Csv" | [ "Base"
    | "Core" | "Data" | "Syntax" ] ] [ 68 | "Html" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 68 | "Json" | [ "Core"
    | "Data" | "Syntax" ] ] [ 68 | "Judge" | [ "Base" | "Core" | "Data" ] ] [ 69 | "Koshu" | [ "Base" | "Core" | "Data"
    | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 71  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    52 | "Coxhand" | [ "Base" | "Data" | "Syntax" ] ] [ 53 | "Logic" | [ "Base" | "Data" ] ] [ 69 | "Message" | [ ] ] [
    70 | "Arith" | [ "Base" | "Data" ] ] [ 70 | "List" | [ "Base" | "Data" ] ] [ 70 | "Misc" | [ "Base" | "Data" |
    "Syntax" ] ] [ 70 | "Order" | [ "Base" | "Data" ] ] [ 70 | "Text" | [ "Base" | "Data" | "Syntax" ] ] [ 70 | "Time" |
    [ "Base" | "Data" ] ] [ 70 | "Type" | [ "Base" | "Data" ] ] [ 71 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 72  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 16 | "Base" | [ "Abort" |
    "IO" | "List" | "Prelude" | "Syntax" | "Text" ] ] [ 35 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "TTree" | "Token"
    ] ] [ 51 | "Data" | [ "Church" | "Content" | "Type" ] ] [ 67 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap"
    | "Resource" ] ] [ 70 | "Writer" | [ ] ] [ 72 | "Cop" | [ ] ] =}

|-- DIR-RANK  /dir-rank 72  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 68 | "KoshuRdf"
    | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 68 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 |
    "KoshuChange" | [ "Base" ] ] [ 72 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 72 | "KoshuMain" | [ "Base" |
    "Core" | "Data" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 73  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 71 |
    "Tropashko" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Restrict" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 73 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 76  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 17 | "PoScale" | [
    "Base" ] ] [ 68 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "Elem" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 71 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "Meta" | [ "Base"
    | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "Order" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 |
    "Resource" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "Source" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 71 | "Term" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Peripheral" | [
    "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "TermGadget" | [ "Base" | "Core" | "Data" ] ] [ 74 |
    "Lattice" | [ ] ] [ 75 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ] ] [ 75 | "Control" |
    [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 76 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 69 | "Message" | [ ] ] [ 71 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Accessor" | [
    "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ]
    [ 72 | "Filter" | [ "Base" | "Core" | "Data" ] ] [ 72 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
    ] [ 72 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 78 | "Empty" | [ "Base" | "Base" | "Core" |
    "Data" | "Flat" ] ] [ 79 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 69 | "Message" | [
    "Base" | "Data" | "Syntax" ] ] [ 71 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 78 | "Confl" | [
    "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ] ] [ 79 | "Deriv" | [ "Base" | "Base" | "Core" | "Data" |
    "Flat" | "Syntax" ] ] [ 80 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 70 | "Base" | [ ] ] [ 77
    | "Flat" | [ "Lattice" ] ] [ 80 | "Cox" | [ ] ] [ 81 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 82  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 52 | "RDF" | [ "Data" ] ] [ 68 | "Element" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 68 |
    "Input" | [ "Base" | "Core" ] ] [ 71 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 71 | "Run" | [ "Base"
    | "Core" | "Data" | "Writer" ] ] [ 82 | "Global" | [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ] ]
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
                                                  0     "Set"            [ ]
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
                                                  6     "PPrint"         [ "List" | "Prelude" ]
                                                  6     "TextTable"      [ "List" | "Prelude" ]
                                                  
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
                                                  9     "Line"           [ "IO" | "Prelude" ]
                                                  10    "Clause"         [ "IO" | "List" | "Prelude" ]
                                                  12    "Message"        [ "Abort" | "IO" | "Prelude" ]
                                                  13    "Tree"           [ "Abort" | "IO" | "List" | "Prelude" ]
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
                                                  
  25        "Koshucode.Baala.Syntax.Token"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  21    "Token"          [ "Base" | "Symbol" ]
                                                  22    "Pattern"        [ ]
                                                  22    "Utility"        [ "Base" ]
                                                  23    "Section"        [ "Base" | "Symbol" ]
                                                  24    "Rel"            [ "Base" | "Symbol" ]
                                                  25    "Clause"         [ "Base" | "Symbol" ]
                                                  
  29        "Koshucode.Baala.Syntax.TTree"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  27    "Bracket"        [ "Base" | "Token" ]
                                                  28    "TokenTree"      [ "Base" | "Token" ]
                                                  29    "Parse"          [ "Base" | "Token" ]
                                                  
  33        "Koshucode.Baala.Syntax.Attr"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "AttrName"       [ ]
                                                  31    "Message"        [ "Base" | "Para" | "Symbol" | "TTree" ]
                                                  32    "Attr"           [ "Base" | "Para" | "TTree" ]
                                                  32    "Slot"           [ "Base" | "TTree" | "Token" ]
                                                  33    "AttrEd"         [ "Base" | "TTree" | "Token" ]
                                                  33    "Parse"          [ "Base" | "Para" ]
                                                  
  34        "Koshucode.Baala.Syntax"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  19    "Para"           [ ]
                                                  20    "Symbol"         [ ]
                                                  26    "Token"          [ ]
                                                  30    "TTree"          [ ]
                                                  32    "Message"        [ ]
                                                  34    "Attr"           [ ]
                                                  
  40        "Koshucode.Baala.Data.Type.Rel"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  38    "Head"           [ "Base" | "Syntax" ]
                                                  39    "Rel"            [ "Base" | "Syntax" ]
                                                  40    "Mono"           [ "Base" ]
                                                  
  41        "Koshucode.Baala.Data.Type"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  21    "Decimal"        [ ]
                                                  21    "Time"           [ ]
                                                  36    "Judge"          [ "Base" | "Syntax" ]
                                                  37    "Interp"         [ "Base" | "Syntax" ]
                                                  37    "Type"           [ "Base" | "Syntax" ]
                                                  41    "Rel"            [ ]
                                                  
  46        "Koshucode.Baala.Core.Lexmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  36    "Lexmap"         [ "Base" | "Syntax" ]
                                                  45    "LexmapTrees"    [ "Base" | "Syntax" ]
                                                  45    "Message"        [ "Base" | "Syntax" ]
                                                  46    "Construct"      [ "Base" | "Syntax" ]
                                                  
  46        "Koshucode.Baala.Data.Content"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  36    "Message"        [ "Base" | "Syntax" ]
                                                  43    "Singleton"      [ "Base" | "Type" ]
                                                  43    "Tree"           [ "Base" | "Syntax" | "Type" ]
                                                  44    "Complex"        [ "Base" | "Syntax" | "Type" ]
                                                  44    "Simple"         [ "Base" | "Type" ]
                                                  45    "Utility"        [ "Base" | "Syntax" | "Type" ]
                                                  46    "BaalaC"         [ "Base" | "Syntax" | "Type" ]
                                                  46    "Decode"         [ "Base" | "Syntax" | "Type" ]
                                                  
  49        "Koshucode.Baala.Data.Church"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  43    "Message"        [ "Base" | "Syntax" | "Type" ]
                                                  44    "Cox"            [ "Base" | "Syntax" ]
                                                  45    "Cop"            [ "Base" | "Syntax" ]
                                                  48    "Build"          [ "Base" | "Content" | "Syntax" ]
                                                  49    "Run"            [ "Base" | "Content" | "Syntax" | "Type" ]
                                                  
  50        "Koshucode.Baala.Data"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  42    "Type"           [ "Decimal" | "Rel" | "Time" ]
                                                  44    "Message"        [ ]
                                                  47    "Content"        [ ]
                                                  50    "Church"         [ ]
                                                  
  54        "Koshucode.Baala.Core.Relkit"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  36    "Message"        [ "Base" | "Syntax" ]
                                                  52    "Relkit"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  53    "Construct"      [ "Base" | "Data" | "Syntax" ]
                                                  54    "Run"            [ "Base" | "Data" | "Syntax" ]
                                                  
  58        "Koshucode.Baala.Core.Relmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  52    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  52    "Result"         [ "Base" | "Data" | "Syntax" ]
                                                  53    "Option"         [ "Base" | "Data" | "Syntax" ]
                                                  56    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  57    "Rop"            [ "Base" | "Lexmap" | "Syntax" ]
                                                  57    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  58    "Construct"      [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  58    "Global"         [ "Base" | "Data" ]
                                                  
  61        "Koshucode.Baala.Core.Assert"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  36    "Message"        [ "Base" | "Syntax" ]
                                                  52    "RelTable"       [ "Base" | "Data" | "Syntax" ]
                                                  56    "Dataset"        [ "Base" | "Data" | "Relkit" | "Syntax" ]
                                                  60    "Assert"         [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  61    "Run"            [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  65        "Koshucode.Baala.Core.Resource"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  52    "Clause"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  63    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  64    "Concrete"       [ "Data" | "Relmap" | "Syntax" ]
                                                  64    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  64    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  65    "Read"           [ "Base" | "Data" | "Relmap" | "Syntax" ]
                                                  
  66        "Koshucode.Baala.Core"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  47    "Lexmap"         [ ]
                                                  53    "Message"        [ ]
                                                  55    "Relkit"         [ ]
                                                  59    "Relmap"         [ ]
                                                  62    "Assert"         [ ]
                                                  66    "Resource"       [ ]
                                                  
  69        "Koshucode.Baala.Rop.Base"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  54    "Message"        [ "Base" ]
                                                  55    "Term"           [ "Base" | "Data" | "Syntax" ]
                                                  68    "Define"         [ "Core" | "Syntax" ]
                                                  68    "Get"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Rop"            [ "Base" | "Core" ]
                                                  
  69        "Koshucode.Baala.Writer"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  68    "Csv"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  68    "Html"           [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  68    "Json"           [ "Core" | "Data" | "Syntax" ]
                                                  68    "Judge"          [ "Base" | "Core" | "Data" ]
                                                  69    "Koshu"          [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  
  71        "Koshucode.Baala.Cop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Replace"        [ ]
                                                  52    "Coxhand"        [ "Base" | "Data" | "Syntax" ]
                                                  53    "Logic"          [ "Base" | "Data" ]
                                                  69    "Message"        [ ]
                                                  70    "Arith"          [ "Base" | "Data" ]
                                                  70    "List"           [ "Base" | "Data" ]
                                                  70    "Misc"           [ "Base" | "Data" | "Syntax" ]
                                                  70    "Order"          [ "Base" | "Data" ]
                                                  70    "Text"           [ "Base" | "Data" | "Syntax" ]
                                                  70    "Time"           [ "Base" | "Data" ]
                                                  70    "Type"           [ "Base" | "Data" ]
                                                  71    "Bundle"         [ "Base" | "Data" ]
                                                  
  72        "Koshucode.Baala"                     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  16    "Base"           [ "Abort" | "IO" | "List" | "Prelude" | "Syntax" | "Text" ]
                                                  35    "Syntax"         [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                                  51    "Data"           [ "Church" | "Content" | "Type" ]
                                                  67    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  70    "Writer"         [ ]
                                                  72    "Cop"            [ ]
                                                  
  72        "Koshucode.Baala.Toolkit.Main"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  68    "KoshuRdf"       [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  68    "KoshuSyntax"    [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "KoshuChange"    [ "Base" ]
                                                  72    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                  72    "KoshuMain"      [ "Base" | "Core" | "Data" | "Writer" ]
                                                  
  73        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  71    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Restrict"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "Rop"            [ "Base" | "Core" ]
                                                  
  76        "Koshucode.Baala.Rop.Flat"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "PoScale"        [ "Base" ]
                                                  68    "Message"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Elem"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Meta"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Order"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Resource"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Source"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Term"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                  74    "Lattice"        [ ]
                                                  75    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  75    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                  76    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  79        "Koshucode.Baala.Rop.Cox"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "GeoDatumJp"     [ ]
                                                  69    "Message"        [ ]
                                                  71    "Get"            [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Accessor"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Calc"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Filter"         [ "Base" | "Core" | "Data" ]
                                                  72    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Range"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  78    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                  79    "Bundle"         [ "Core" | "Data" ]
                                                  
  80        "Koshucode.Baala.Rop.Nest"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  69    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  71    "Flow"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  78    "Confl"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  79    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  80    "Rop"            [ "Base" | "Core" | "Data" ]
                                                  
  81        "Koshucode.Baala.Rop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  70    "Base"           [ ]
                                                  77    "Flat"           [ "Lattice" ]
                                                  80    "Cox"            [ ]
                                                  81    "Nest"           [ ]
                                                  
  82        "Koshucode.Baala.Toolkit.Library"     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Version"        [ ]
                                                  52    "RDF"            [ "Data" ]
                                                  68    "Element"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  68    "Input"          [ "Base" | "Core" ]
                                                  71    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                  71    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                  82    "Global"         [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      37 judges on DIR-RANK
**      37 judges in total
**

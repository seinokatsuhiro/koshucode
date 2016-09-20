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
**    ../subtext/data/IMPORT.k
**    ../syntax/data/IMPORT.k
**    ../toolkit/data/IMPORT.k
**    ../writer/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Subtext.Name"  /base-rank {= /rank /base /import-dir [ 0 | "String" | [
    ] ] =}
|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Case" | [ ]
    ] [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Pair" | [ ] ] [ 1 | "Queue" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Set" | [ ] ] [
    0 | "Split" | [ ] ] [ 1 | "Dots" | [ ] ] [ 3 | "Assoc" | [ "Prelude" ] ] [ 3 | "List" | [ "Prelude" ] ] [ 3 | "Snip"
    | [ "Prelude" ] ] [ 4 | "Order" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ]
    ] [ 0 | "LineBreak" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 3 | "Utility" | [ "Prelude" ] ] [ 4 | "MixText" | [ ] ] [ 5 |
    "MixClass" | [ ] ] [ 5 | "MixEncode" | [ ] ] [ 6 | "PPrint" | [ "List" | "Prelude" ] ] [ 6 | "TextTable" | [ "List"
    | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Subtext.Operator"  /base-rank {= /rank /base /import-dir [ 3 | "Basic"
    | [ ] ] [ 4 | "Combine" | [ ] ] [ 5 | "Repeat" | [ ] ] [ 6 | "Char" | [ ] ] =}

|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" |
    [ ] ] [ 3 | "BzFile" | [ "Prelude" ] ] [ 3 | "Exit" | [ "Prelude" ] ] [ 4 | "Http" | [ "Prelude" ] ] [ 6 | "IOPoint"
    | [ "List" | "Prelude" ] ] [ 7 | "CodePt" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 11  /dir "Koshucode.Baala.Subtext"  /base-rank {= /rank /base /import-dir [ 1 | "Fn" | [ ] ] [ 2
    | "Expr" | [ ] ] [ 7 | "Operator" | [ ] ] [ 8 | "Para" | [ "Operator" ] ] [ 9 | "Match" | [ "Operator" ] ] [ 11 |
    "Test where" | [ "Subtext" ] ] =}
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
|-- DIR-RANK  /dir-rank 26  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 17 | "Message"
    | [ "Base" ] ] [ 21 | "Token" | [ "Base" | "Symbol" ] ] [ 22 | "Pattern" | [ ] ] [ 22 | "Utility" | [ "Base" ] ] [
    23 | "Nipper" | [ "Base" | "Symbol" ] ] [ 24 | "Section" | [ "Base" | "Symbol" ] ] [ 25 | "Rel" | [ "Base" |
    "Symbol" ] ] [ 26 | "Clause" | [ "Base" | "Symbol" ] ] =}

|-- DIR-RANK  /dir-rank 30  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 28 | "Bracket"
    | [ "Base" | "Token" ] ] [ 29 | "TokenTree" | [ "Base" | "Token" ] ] [ 30 | "Parse" | [ "Base" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 34  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 32 | "Message" | [ "Base" | "Para" | "Symbol" | "TTree" ] ] [ 33 | "Attr" | [ "Base" | "Para" | "TTree" ] ]
    [ 33 | "Slot" | [ "Base" | "TTree" | "Token" ] ] [ 34 | "AttrEd" | [ "Base" | "TTree" | "Token" ] ] [ 34 | "Parse" |
    [ "Base" | "Para" ] ] =}
|-- DIR-RANK  /dir-rank 35  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 19 | "Para" | [ ] ] [
    20 | "Symbol" | [ ] ] [ 27 | "Token" | [ ] ] [ 31 | "TTree" | [ ] ] [ 33 | "Message" | [ ] ] [ 35 | "Attr" | [ ] ]
    =}
|-- DIR-RANK  /dir-rank 41  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 38 |
    "ShareSide" | [ "Base" | "Syntax" ] ] [ 39 | "Head" | [ "Base" | "Syntax" ] ] [ 40 | "Rel" | [ "Base" | "Syntax" ] ]
    [ 41 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 42  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 17 | "Message" | [
    "Base" ] ] [ 21 | "Decimal" | [ ] ] [ 21 | "Time" | [ ] ] [ 37 | "Judge" | [ "Base" | "Syntax" ] ] [ 38 | "Interp" |
    [ "Base" | "Syntax" ] ] [ 38 | "Type" | [ "Base" | "Syntax" ] ] [ 42 | "Rel" | [ ] ] =}

|-- DIR-RANK  /dir-rank 47  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 37 | "Lexmap" |
    [ "Base" | "Syntax" ] ] [ 46 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [ 46 | "Message" | [ "Base" | "Syntax" ] ] [
    47 | "Construct" | [ "Base" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 47  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 37 | "Message"
    | [ "Base" | "Syntax" ] ] [ 44 | "Singleton" | [ "Base" | "Type" ] ] [ 44 | "Tree" | [ "Base" | "Syntax" | "Type" ]
    ] [ 45 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 45 | "Simple" | [ "Base" | "Type" ] ] [ 46 | "Utility" | [
    "Base" | "Syntax" | "Type" ] ] [ 47 | "BaalaC" | [ "Base" | "Syntax" | "Type" ] ] [ 47 | "Decode" | [ "Base" |
    "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 50  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 44 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 45 | "Cox" | [ "Base" | "Syntax" ] ] [ 46 | "Cop" | [ "Base" | "Syntax" ] ] [ 49
    | "Build" | [ "Base" | "Content" | "Syntax" ] ] [ 50 | "Run" | [ "Base" | "Content" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 51  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 43 | "Type" | [
    "Decimal" | "Rel" | "Time" ] ] [ 45 | "Message" | [ ] ] [ 48 | "Content" | [ ] ] [ 51 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 55  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 37 | "Message" |
    [ "Base" | "Syntax" ] ] [ 53 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 54 | "Construct" | [ "Base"
    | "Data" | "Syntax" ] ] [ 55 | "Run" | [ "Base" | "Data" | "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 53 | "Message" |
    [ "Base" | "Data" | "Syntax" ] ] [ 53 | "Result" | [ "Base" | "Data" | "Syntax" ] ] [ 54 | "Option" | [ "Base" |
    "Data" | "Syntax" ] ] [ 57 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 58 | "Rop" | [
    "Base" | "Lexmap" | "Syntax" ] ] [ 58 | "Specialize" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 59 |
    "Construct" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 59 | "Global" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 62  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 37 | "Message" |
    [ "Base" | "Syntax" ] ] [ 53 | "RelTable" | [ "Base" | "Data" | "Syntax" ] ] [ 57 | "Dataset" | [ "Base" | "Data" |
    "Relkit" | "Syntax" ] ] [ 61 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 62 | "Run" | [
    "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 66  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 17 | "Message"
    | [ "Base" ] ] [ 53 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 64 | "Resource" | [ "Assert" | "Base"
    | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] [ 65 | "Concrete" | [ "Data" | "Relmap" | "Syntax" ] ] [ 65
    | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 65 | "Run" | [ "Assert" | "Base" |
    "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 66 | "Read" | [ "Base" | "Data" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 67  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 48 | "Lexmap" | [ ] ] [
    54 | "Message" | [ ] ] [ 56 | "Relkit" | [ ] ] [ 60 | "Relmap" | [ ] ] [ 63 | "Assert" | [ ] ] [ 67 | "Resource" | [
    ] ] =}
|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 55 | "Message" | [
    "Base" ] ] [ 56 | "Term" | [ "Base" | "Data" | "Syntax" ] ] [ 69 | "Define" | [ "Core" | "Syntax" ] ] [ 69 | "Get" |
    [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Rop" | [ "Base" | "Core" ] ] =}

|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 69 | "Csv" | [ "Base"
    | "Core" | "Data" | "Syntax" ] ] [ 69 | "Html" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "Json" | [ "Core"
    | "Data" | "Syntax" ] ] [ 69 | "Judge" | [ "Base" | "Core" | "Data" ] ] [ 70 | "Koshu" | [ "Base" | "Core" | "Data"
    | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 72  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    53 | "Coxhand" | [ "Base" | "Data" | "Syntax" ] ] [ 54 | "Logic" | [ "Base" | "Data" ] ] [ 70 | "Message" | [ ] ] [
    71 | "Arith" | [ "Base" | "Data" ] ] [ 71 | "List" | [ "Base" | "Data" ] ] [ 71 | "Misc" | [ "Base" | "Data" |
    "Syntax" ] ] [ 71 | "Order" | [ "Base" | "Data" ] ] [ 71 | "Text" | [ "Base" | "Data" | "Syntax" ] ] [ 71 | "Time" |
    [ "Base" | "Data" ] ] [ 71 | "Type" | [ "Base" | "Data" ] ] [ 72 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 73  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 10 | "Subtext" | [
    "Operator" ] ] [ 16 | "Base" | [ "Abort" | "IO" | "List" | "Prelude" | "Syntax" | "Text" ] ] [ 36 | "Syntax" | [
    "Attr" | "Para" | "Symbol" | "TTree" | "Token" ] ] [ 52 | "Data" | [ "Church" | "Content" | "Type" ] ] [ 68 | "Core"
    | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 71 | "Writer" | [ ] ] [ 73 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 73  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 69 | "KoshuRdf"
    | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 73 |
    "KoshuChange" | [ "Base" ] ] [ 73 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 73 | "KoshuMain" | [ "Base" |
    "Core" | "Data" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 74  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 72 |
    "Tropashko" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 73 | "Restrict" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 74 | "Rop" | [ "Base" | "Core" ] ] =}

|-- DIR-RANK  /dir-rank 77  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 17 | "PoScale" | [
    "Base" ] ] [ 69 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Elem" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 72 | "Meta" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Order" | [ "Base" |
    "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Resource" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72
    | "Source" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Subtext" | [ "Base" | "Base" | "Core" |
    "Data" | "Subtext" | "Syntax" ] ] [ 72 | "Term" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 73 | "Gadget"
    | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 73 | "Peripheral" | [ "Base" | "Base" | "Core" | "Data" |
    "Syntax" ] ] [ 73 | "TermGadget" | [ "Base" | "Core" | "Data" ] ] [ 75 | "Lattice" | [ ] ] [ 76 | "Check" | [ "Base"
    | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ] ] [ 76 | "Control" | [ "Base" | "Base" | "Core" | "Data" |
    "Lattice" ] ] [ 77 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 70 | "Message" | [ ] ] [ 72 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 73 | "Accessor" | [
    "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 73 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ]
    [ 73 | "Filter" | [ "Base" | "Core" | "Data" ] ] [ 73 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
    ] [ 73 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 79 | "Empty" | [ "Base" | "Base" | "Core" |
    "Data" | "Flat" ] ] [ 80 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 70 | "Message" | [
    "Base" | "Data" | "Syntax" ] ] [ 72 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 79 | "Confl" | [
    "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ] ] [ 80 | "Deriv" | [ "Base" | "Base" | "Core" | "Data" |
    "Flat" | "Syntax" ] ] [ 81 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 82  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 71 | "Base" | [ ] ] [ 78
    | "Flat" | [ "Lattice" ] ] [ 81 | "Cox" | [ ] ] [ 82 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 83  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 53 | "RDF" | [ "Data" ] ] [ 69 | "Element" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 |
    "Input" | [ "Base" | "Core" ] ] [ 72 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 72 | "Run" | [ "Base"
    | "Core" | "Data" | "Writer" ] ] [ 83 | "Global" | [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ] ]
    =}

*** 40 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                  /base-rank
  --------- ------------------------------------- ---------------------------------------------------------------------------------------------------
  0         "Koshucode.Baala.Subtext.Name"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "String"         [ ]
                                                  
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
                                                  5     "MixEncode"      [ ]
                                                  6     "PPrint"         [ "List" | "Prelude" ]
                                                  6     "TextTable"      [ "List" | "Prelude" ]
                                                  
  6         "Koshucode.Baala.Subtext.Operator"    /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  3     "Basic"          [ ]
                                                  4     "Combine"        [ ]
                                                  5     "Repeat"         [ ]
                                                  6     "Char"           [ ]
                                                  
  7         "Koshucode.Baala.Base.IO"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "SimpleOption"   [ ]
                                                  3     "BzFile"         [ "Prelude" ]
                                                  3     "Exit"           [ "Prelude" ]
                                                  4     "Http"           [ "Prelude" ]
                                                  6     "IOPoint"        [ "List" | "Prelude" ]
                                                  7     "CodePt"         [ "Prelude" ]
                                                  
  11        "Koshucode.Baala.Subtext"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  1     "Fn"             [ ]
                                                  2     "Expr"           [ ]
                                                  7     "Operator"       [ ]
                                                  8     "Para"           [ "Operator" ]
                                                  9     "Match"          [ "Operator" ]
                                                  11    "Test where"     [ "Subtext" ]
                                                  
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
                                                  10    "Scan"           [ "IO" | "Prelude" ]
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
                                                  
  26        "Koshucode.Baala.Syntax.Token"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  21    "Token"          [ "Base" | "Symbol" ]
                                                  22    "Pattern"        [ ]
                                                  22    "Utility"        [ "Base" ]
                                                  23    "Nipper"         [ "Base" | "Symbol" ]
                                                  24    "Section"        [ "Base" | "Symbol" ]
                                                  25    "Rel"            [ "Base" | "Symbol" ]
                                                  26    "Clause"         [ "Base" | "Symbol" ]
                                                  
  30        "Koshucode.Baala.Syntax.TTree"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  28    "Bracket"        [ "Base" | "Token" ]
                                                  29    "TokenTree"      [ "Base" | "Token" ]
                                                  30    "Parse"          [ "Base" | "Token" ]
                                                  
  34        "Koshucode.Baala.Syntax.Attr"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "AttrName"       [ ]
                                                  32    "Message"        [ "Base" | "Para" | "Symbol" | "TTree" ]
                                                  33    "Attr"           [ "Base" | "Para" | "TTree" ]
                                                  33    "Slot"           [ "Base" | "TTree" | "Token" ]
                                                  34    "AttrEd"         [ "Base" | "TTree" | "Token" ]
                                                  34    "Parse"          [ "Base" | "Para" ]
                                                  
  35        "Koshucode.Baala.Syntax"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  19    "Para"           [ ]
                                                  20    "Symbol"         [ ]
                                                  27    "Token"          [ ]
                                                  31    "TTree"          [ ]
                                                  33    "Message"        [ ]
                                                  35    "Attr"           [ ]
                                                  
  41        "Koshucode.Baala.Data.Type.Rel"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  38    "ShareSide"      [ "Base" | "Syntax" ]
                                                  39    "Head"           [ "Base" | "Syntax" ]
                                                  40    "Rel"            [ "Base" | "Syntax" ]
                                                  41    "Mono"           [ "Base" ]
                                                  
  42        "Koshucode.Baala.Data.Type"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  21    "Decimal"        [ ]
                                                  21    "Time"           [ ]
                                                  37    "Judge"          [ "Base" | "Syntax" ]
                                                  38    "Interp"         [ "Base" | "Syntax" ]
                                                  38    "Type"           [ "Base" | "Syntax" ]
                                                  42    "Rel"            [ ]
                                                  
  47        "Koshucode.Baala.Core.Lexmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  37    "Lexmap"         [ "Base" | "Syntax" ]
                                                  46    "LexmapTrees"    [ "Base" | "Syntax" ]
                                                  46    "Message"        [ "Base" | "Syntax" ]
                                                  47    "Construct"      [ "Base" | "Syntax" ]
                                                  
  47        "Koshucode.Baala.Data.Content"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  37    "Message"        [ "Base" | "Syntax" ]
                                                  44    "Singleton"      [ "Base" | "Type" ]
                                                  44    "Tree"           [ "Base" | "Syntax" | "Type" ]
                                                  45    "Complex"        [ "Base" | "Syntax" | "Type" ]
                                                  45    "Simple"         [ "Base" | "Type" ]
                                                  46    "Utility"        [ "Base" | "Syntax" | "Type" ]
                                                  47    "BaalaC"         [ "Base" | "Syntax" | "Type" ]
                                                  47    "Decode"         [ "Base" | "Syntax" | "Type" ]
                                                  
  50        "Koshucode.Baala.Data.Church"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  44    "Message"        [ "Base" | "Syntax" | "Type" ]
                                                  45    "Cox"            [ "Base" | "Syntax" ]
                                                  46    "Cop"            [ "Base" | "Syntax" ]
                                                  49    "Build"          [ "Base" | "Content" | "Syntax" ]
                                                  50    "Run"            [ "Base" | "Content" | "Syntax" | "Type" ]
                                                  
  51        "Koshucode.Baala.Data"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  43    "Type"           [ "Decimal" | "Rel" | "Time" ]
                                                  45    "Message"        [ ]
                                                  48    "Content"        [ ]
                                                  51    "Church"         [ ]
                                                  
  55        "Koshucode.Baala.Core.Relkit"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  37    "Message"        [ "Base" | "Syntax" ]
                                                  53    "Relkit"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  54    "Construct"      [ "Base" | "Data" | "Syntax" ]
                                                  55    "Run"            [ "Base" | "Data" | "Syntax" ]
                                                  
  59        "Koshucode.Baala.Core.Relmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  53    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  53    "Result"         [ "Base" | "Data" | "Syntax" ]
                                                  54    "Option"         [ "Base" | "Data" | "Syntax" ]
                                                  57    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  58    "Rop"            [ "Base" | "Lexmap" | "Syntax" ]
                                                  58    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  59    "Construct"      [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  59    "Global"         [ "Base" | "Data" ]
                                                  
  62        "Koshucode.Baala.Core.Assert"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  37    "Message"        [ "Base" | "Syntax" ]
                                                  53    "RelTable"       [ "Base" | "Data" | "Syntax" ]
                                                  57    "Dataset"        [ "Base" | "Data" | "Relkit" | "Syntax" ]
                                                  61    "Assert"         [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  62    "Run"            [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  66        "Koshucode.Baala.Core.Resource"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  53    "Clause"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  64    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  65    "Concrete"       [ "Data" | "Relmap" | "Syntax" ]
                                                  65    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  65    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  66    "Read"           [ "Base" | "Data" | "Relmap" | "Syntax" ]
                                                  
  67        "Koshucode.Baala.Core"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  48    "Lexmap"         [ ]
                                                  54    "Message"        [ ]
                                                  56    "Relkit"         [ ]
                                                  60    "Relmap"         [ ]
                                                  63    "Assert"         [ ]
                                                  67    "Resource"       [ ]
                                                  
  70        "Koshucode.Baala.Rop.Base"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  55    "Message"        [ "Base" ]
                                                  56    "Term"           [ "Base" | "Data" | "Syntax" ]
                                                  69    "Define"         [ "Core" | "Syntax" ]
                                                  69    "Get"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Rop"            [ "Base" | "Core" ]
                                                  
  70        "Koshucode.Baala.Writer"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  69    "Csv"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Html"           [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Json"           [ "Core" | "Data" | "Syntax" ]
                                                  69    "Judge"          [ "Base" | "Core" | "Data" ]
                                                  70    "Koshu"          [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  
  72        "Koshucode.Baala.Cop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Replace"        [ ]
                                                  53    "Coxhand"        [ "Base" | "Data" | "Syntax" ]
                                                  54    "Logic"          [ "Base" | "Data" ]
                                                  70    "Message"        [ ]
                                                  71    "Arith"          [ "Base" | "Data" ]
                                                  71    "List"           [ "Base" | "Data" ]
                                                  71    "Misc"           [ "Base" | "Data" | "Syntax" ]
                                                  71    "Order"          [ "Base" | "Data" ]
                                                  71    "Text"           [ "Base" | "Data" | "Syntax" ]
                                                  71    "Time"           [ "Base" | "Data" ]
                                                  71    "Type"           [ "Base" | "Data" ]
                                                  72    "Bundle"         [ "Base" | "Data" ]
                                                  
  73        "Koshucode.Baala"                     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  10    "Subtext"        [ "Operator" ]
                                                  16    "Base"           [ "Abort" | "IO" | "List" | "Prelude" | "Syntax" | "Text" ]
                                                  36    "Syntax"         [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                                  52    "Data"           [ "Church" | "Content" | "Type" ]
                                                  68    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  71    "Writer"         [ ]
                                                  73    "Cop"            [ ]
                                                  
  73        "Koshucode.Baala.Toolkit.Main"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  69    "KoshuRdf"       [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "KoshuSyntax"    [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "KoshuChange"    [ "Base" ]
                                                  73    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                  73    "KoshuMain"      [ "Base" | "Core" | "Data" | "Writer" ]
                                                  
  74        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  72    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "Restrict"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  74    "Rop"            [ "Base" | "Core" ]
                                                  
  77        "Koshucode.Baala.Rop.Flat"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "PoScale"        [ "Base" ]
                                                  69    "Message"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Elem"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Meta"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Order"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Resource"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Source"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Subtext"        [ "Base" | "Base" | "Core" | "Data" | "Subtext" | "Syntax" ]
                                                  72    "Term"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                  75    "Lattice"        [ ]
                                                  76    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  76    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                  77    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  80        "Koshucode.Baala.Rop.Cox"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "GeoDatumJp"     [ ]
                                                  70    "Message"        [ ]
                                                  72    "Get"            [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "Accessor"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "Calc"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "Filter"         [ "Base" | "Core" | "Data" ]
                                                  73    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  73    "Range"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                  80    "Bundle"         [ "Core" | "Data" ]
                                                  
  81        "Koshucode.Baala.Rop.Nest"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  70    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  72    "Flow"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Confl"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  80    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  81    "Rop"            [ "Base" | "Core" | "Data" ]
                                                  
  82        "Koshucode.Baala.Rop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  71    "Base"           [ ]
                                                  78    "Flat"           [ "Lattice" ]
                                                  81    "Cox"            [ ]
                                                  82    "Nest"           [ ]
                                                  
  83        "Koshucode.Baala.Toolkit.Library"     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Version"        [ ]
                                                  53    "RDF"            [ "Data" ]
                                                  69    "Element"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Input"          [ "Base" | "Core" ]
                                                  72    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                  72    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                  83    "Global"         [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      40 judges on DIR-RANK
**      40 judges in total
**

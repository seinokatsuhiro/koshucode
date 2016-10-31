** -*- koshu -*-
**
**  INPUT
**    dir-rank.k
**    ../base/data/IMPORT.k
**    ../calculator/data/IMPORT.k
**    ../cop/data/IMPORT.k
**    ../core/data/IMPORT.k
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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Overture.Name"  /base-rank {= /rank /base /import-dir [ 0 | "String" |
    [ ] ] =}
|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Overture.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Category"
    | [ ] ] [ 0 | "Unicode" | [ ] ] [ 1 | "Utility" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Overture"  /base-rank {= /rank /base /import-dir [ 0 | "Type" | [ ] ] [
    1 | "Fn" | [ ] ] [ 2 | "Text" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Case" | [ ]
    ] [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Queue" | [ ] ] [ 4 | "Pair" | [ "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Subtext.Operator"  /base-rank {= /rank /base /import-dir [ 3 | "Basic"
    | [ ] ] [ 4 | "Combine" | [ ] ] [ 5 | "Repeat" | [ ] ] [ 6 | "Char" | [ "Overture" ] ] =}

|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Set" | [ ] ] [
    0 | "Split" | [ ] ] [ 1 | "Dots" | [ ] ] [ 4 | "Assoc" | [ "Overture" ] ] [ 4 | "List" | [ "Overture" ] ] [ 6 |
    "Snip" | [ "Overture" | "Prelude" ] ] [ 7 | "Order" | [ "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 9  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ]
    ] [ 0 | "LineBreak" | [ ] ] [ 4 | "MixText" | [ "Overture" ] ] [ 5 | "MixClass" | [ ] ] [ 5 | "MixEncode" | [ ] ] [
    9 | "PPrint" | [ "List" | "Prelude" ] ] [ 9 | "Suffix" | [ "List" | "Overture" ] ] [ 9 | "TextTable" | [ "List" |
    "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" |
    [ ] ] [ 6 | "BzFile" | [ "Prelude" ] ] [ 6 | "Exit" | [ "Prelude" ] ] [ 6 | "Http" | [ "Overture" | "Prelude" ] ] [
    9 | "IOPoint" | [ "List" | "Prelude" ] ] [ 10 | "CodePt" | [ "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Subtext"  /base-rank {= /rank /base /import-dir [ 0 | "MinMax" | [ ] ]
    [ 2 | "Expr" | [ ] ] [ 7 | "Operator" | [ ] ] [ 8 | "Bundle" | [ "Operator" ] ] [ 9 | "Para" | [ "Operator" ] ] [ 10
    | "Match" | [ "Operator" ] ] =}
|-- DIR-RANK  /dir-rank 15  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 12 | "Reason" | [
    "IO" ] ] [ 13 | "Abortable" | [ "IO" | "Overture" ] ] [ 13 | "Report" | [ "IO" | "List" | "Overture" | "Prelude" |
    "Text" ] ] [ 15 | "Message" | [ "Abort" ] ] =}

|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Base.Syntax"  /base-rank {= /rank /base /import-dir [ 12 | "Line" | [
    "IO" | "Prelude" ] ] [ 13 | "Clause" | [ "IO" | "List" | "Prelude" ] ] [ 13 | "Scan" | [ "IO" | "Overture" |
    "Prelude" ] ] [ 15 | "Message" | [ "Abort" | "IO" | "Overture" ] ] [ 16 | "Tree" | [ "Abort" | "IO" | "List" |
    "Overture" | "Prelude" ] ] [ 17 | "Infix" | [ "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 5 | "Prelude" | [ ] ] [
    8 | "List" | [ ] ] [ 10 | "Text" | [ ] ] [ 11 | "IO" | [ ] ] [ 14 | "Abort" | [ ] ] [ 16 | "Message" | [ ] ] [ 18 |
    "Syntax" | [ ] ] =}
|-- DIR-RANK  /dir-rank 21  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 20 | "Para" | [
    "Base" | "Overture" ] ] [ 21 | "Get" | [ "Base" ] ] [ 21 | "ParaSpec" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 22  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 4 |
    "AngleText" | [ "Overture" ] ] [ 4 | "Term" | [ "Overture" ] ] [ 20 | "Message" | [ "Base" ] ] [ 21 | "Next" | [
    "Base" | "Overture" ] ] [ 22 | "Short" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 23  /dir "Koshucode.Baala.Data.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 20 |
    "Decimal" | [ "Base" | "Overture" ] ] [ 20 | "Rational" | [ "Base" | "Overture" ] ] [ 21 | "Fraction" | [ "Base" |
    "Overture" ] ] [ 22 | "Coder" | [ "Base" | "Overture" ] ] [ 23 | "BinaryAb" | [ "Base" ] ] =}

|-- DIR-RANK  /dir-rank 25  /dir "Koshucode.Baala.Data.Type.Time"  /base-rank {= /rank /base /import-dir [ 21 | "Date" |
    [ "Base" | "Overture" ] ] [ 22 | "Clock" | [ "Base" | "Overture" ] ] [ 23 | "ClockCalc" | [ "Base" | "Overture" ] ]
    [ 24 | "Time" | [ "Base" | "Overture" ] ] [ 25 | "TimeCalc" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 29  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 20 | "Message"
    | [ "Base" | "Overture" ] ] [ 24 | "Token" | [ "Base" | "Symbol" ] ] [ 25 | "Pattern" | [ ] ] [ 25 | "Utility" | [
    "Base" | "Overture" ] ] [ 26 | "Nipper" | [ "Base" | "Overture" | "Symbol" ] ] [ 27 | "Section" | [ "Base" |
    "Symbol" ] ] [ 28 | "Rel" | [ "Base" | "Overture" | "Symbol" ] ] [ 29 | "Clause" | [ "Base" | "Overture" | "Symbol"
    ] ] =}
|-- DIR-RANK  /dir-rank 33  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 31 | "Bracket"
    | [ "Base" | "Token" ] ] [ 31 | "Pattern" | [ "Base" | "Token" ] ] [ 32 | "TokenTree" | [ "Base" | "Token" ] ] [ 33
    | "Parse" | [ "Base" | "Overture" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 37  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 35 | "Message" | [ "Base" | "Overture" | "Para" | "Symbol" | "TTree" ] ] [ 36 | "Attr" | [ "Base" |
    "Overture" | "Para" | "TTree" ] ] [ 36 | "Slot" | [ "Base" | "Overture" | "TTree" | "Token" ] ] [ 37 | "AttrEd" | [
    "Base" | "TTree" | "Token" ] ] [ 37 | "Parse" | [ "Base" | "Overture" | "Para" ] ] =}
|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 22 | "Para" | [ ] ] [
    23 | "Symbol" | [ ] ] [ 30 | "Token" | [ ] ] [ 34 | "TTree" | [ ] ] [ 36 | "Message" | [ ] ] [ 38 | "Attr" | [ ] ]
    =}

|-- DIR-RANK  /dir-rank 42  /dir "Koshucode.Baala.Data.Type.Type"  /base-rank {= /rank /base /import-dir [ 42 |
    "TypeCalc" | [ "Type" ] ] =}
|-- DIR-RANK  /dir-rank 44  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 41 |
    "ShareSide" | [ "Base" | "Syntax" ] ] [ 42 | "Head" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 43 | "Rel" | [
    "Base" | "Overture" | "Syntax" ] ] [ 44 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 45  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 20 | "Message" | [
    "Base" ] ] [ 24 | "Decimal" | [ ] ] [ 26 | "Time" | [ ] ] [ 40 | "Judge" | [ "Base" | "Overture" | "Syntax" ] ] [ 41
    | "Interp" | [ "Base" | "Syntax" ] ] [ 41 | "Type" | [ "Base" | "Overture" | "Syntax" ] ] [ 45 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 50  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 40 | "Lexmap" |
    [ "Base" | "Overture" | "Syntax" ] ] [ 49 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [ 49 | "Message" | [ "Base" |
    "Overture" | "Syntax" ] ] [ 50 | "Construct" | [ "Base" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 50  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 40 | "Message"
    | [ "Base" | "Overture" | "Syntax" ] ] [ 47 | "Singleton" | [ "Base" | "Overture" | "Type" ] ] [ 47 | "Tree" | [
    "Base" | "Syntax" | "Type" ] ] [ 48 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 48 | "Simple" | [ "Base" |
    "Overture" | "Type" ] ] [ 49 | "Utility" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 50 | "BaalaC" | [ "Base"
    | "Syntax" | "Type" ] ] [ 50 | "Decode" | [ "Base" | "Syntax" | "Type" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 53  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 47 | "Message" |
    [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 48 | "Cox" | [ "Base" | "Overture" | "Syntax" ] ] [ 49 | "Cop" | [
    "Base" | "Overture" | "Syntax" ] ] [ 52 | "Build" | [ "Base" | "Content" | "Overture" | "Syntax" ] ] [ 53 | "Run" |
    [ "Base" | "Content" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 54  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 46 | "Type" | [
    "Decimal" | "Rel" | "Time" | "Type" ] ] [ 48 | "Message" | [ ] ] [ 51 | "Content" | [ ] ] [ 54 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 57  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Overture" | "Syntax" ] ] [ 56 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ] ] [ 57
    | "Construct" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 57 | "Run" | [ "Base" | "Data" | "Overture" |
    "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 56 | "Message" |
    [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 56 | "Result" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 57 |
    "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 59 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" ] ] [ 60 | "Rop" | [ "Base" | "Lexmap" | "Syntax" ] ] [ 60 | "Specialize" | [ "Base" | "Data" | "Lexmap" |
    "Overture" | "Relkit" | "Syntax" ] ] [ 61 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" ] ] [ 61 | "Global" | [ "Base" | "Data" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 64  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Overture" | "Syntax" ] ] [ 56 | "RelTable" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 59 |
    "Dataset" | [ "Base" | "Data" | "Relkit" | "Syntax" ] ] [ 63 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Overture"
    | "Relmap" | "Syntax" ] ] [ 64 | "Run" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax"
    ] ] =}

|-- DIR-RANK  /dir-rank 68  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 20 | "Message"
    | [ "Base" | "Overture" ] ] [ 56 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ] ] [ 66 |
    "Resource" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ] ] [ 67 |
    "Concrete" | [ "Data" | "Relmap" | "Syntax" ] ] [ 67 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" |
    "Relmap" | "Syntax" ] ] [ 67 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
    ] [ 68 | "Read" | [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 51 | "Lexmap" | [ ] ] [
    57 | "Message" | [ ] ] [ 58 | "Relkit" | [ ] ] [ 62 | "Relmap" | [ ] ] [ 65 | "Assert" | [ ] ] [ 69 | "Resource" | [
    ] ] =}
|-- DIR-RANK  /dir-rank 72  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 58 | "Message" | [
    "Base" ] ] [ 59 | "Term" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 71 | "Define" | [ "Core" | "Syntax" ] ] [
    71 | "Get" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 72  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 71 | "Csv" | [ "Base"
    | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 71 | "Html" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ]
    [ 71 | "Json" | [ "Core" | "Data" | "Syntax" ] ] [ 71 | "Judge" | [ "Base" | "Core" | "Data" | "Overture" ] ] [ 72 |
    "Koshu" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 74  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    56 | "Coxhand" | [ "Data" | "Overture" | "Syntax" ] ] [ 57 | "Logic" | [ "Base" | "Data" ] ] [ 72 | "Message" | [ ]
    ] [ 73 | "Arith" | [ "Base" | "Data" | "Overture" ] ] [ 73 | "List" | [ "Base" | "Data" | "Overture" ] ] [ 73 |
    "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 73 | "Order" | [ "Base" | "Data" ] ] [ 73 | "Text" | [ "Base" | "Data" |
    "Overture" | "Syntax" ] ] [ 73 | "Time" | [ "Data" | "Overture" ] ] [ 73 | "Type" | [ "Base" | "Data" ] ] [ 74 |
    "Bundle" | [ "Base" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 75  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 3 | "Overture" | [ "Text" ]
    ] [ 11 | "Subtext" | [ "Operator" ] ] [ 19 | "Base" | [ "Abort" | "IO" | "List" | "Prelude" | "Syntax" | "Text" ] ]
    [ 39 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ] ] [ 55 | "Data" | [ "Church" | "Content" |
    "Type" ] ] [ 70 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 73 | "Writer" | [ ] ] [ 75
    | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 75  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 71 | "KoshuRdf"
    | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" ] ] [ 75 | "KoshuChange" | [ "Base" ] ] [ 75 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 75 |
    "KoshuMain" | [ "Base" | "Core" | "Data" | "Overture" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 76  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 74 |
    "Tropashko" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 75 | "Restrict" | [ "Base" | "Base" | "Core" |
    "Data" | "Overture" | "Syntax" ] ] [ 76 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 20 | "PoScale" | [
    "Base" | "Overture" ] ] [ 71 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 74 | "Elem" | [ "Base" |
    "Base" | "Core" | "Data" | "Syntax" ] ] [ 74 | "Meta" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 74 |
    "Order" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 74 | "Resource" | [ "Base" | "Base" | "Core" | "Data"
    | "Syntax" ] ] [ 74 | "Source" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 74 | "Subtext" | [ "Base"
    | "Base" | "Core" | "Data" | "Overture" | "Subtext" | "Syntax" ] ] [ 74 | "Term" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 75 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 75 |
    "Peripheral" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 75 | "TermGadget" | [ "Base" | "Core" | "Data" ]
    ] [ 77 | "Lattice" | [ ] ] [ 78 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ] ] [ 78 |
    "Control" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Overture" ] ] [ 79 | "Bundle" | [ "Base" | "Core" |
    "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 82  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 72 | "Message" | [ ] ] [ 74 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 75 | "Accessor" | [
    "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 75 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ]
    [ 75 | "Filter" | [ "Base" | "Core" | "Data" ] ] [ 75 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Overture"
    | "Syntax" ] ] [ 75 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 81 | "Empty" | [ "Base" |
    "Base" | "Core" | "Data" | "Flat" | "Overture" ] ] [ 82 | "Bundle" | [ "Core" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 83  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 72 | "Message" | [
    "Base" | "Data" | "Syntax" ] ] [ 74 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 81 | "Confl" | [
    "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ] ] [ 82 | "Deriv" | [ "Base" | "Base" | "Core" |
    "Data" | "Flat" | "Overture" | "Syntax" ] ] [ 83 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 84  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 73 | "Base" | [ ] ] [ 80
    | "Flat" | [ "Lattice" ] ] [ 83 | "Cox" | [ ] ] [ 84 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 85  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 56 | "RDF" | [ "Data" ] ] [ 71 | "Element" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax"
    ] ] [ 71 | "Input" | [ "Base" | "Core" ] ] [ 74 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 74 | "Run"
    | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 85 | "Global" | [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" |
    "Nest" ] ] =}

*** 43 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                  /base-rank
  --------- ------------------------------------- ----------------------------------------------------------------------------------------------------------------
  0         "Koshucode.Baala.Overture.Name"       /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "String"         [ ]
                                                  
  1         "Koshucode.Baala.Overture.Text"       /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Category"       [ ]
                                                  0     "Unicode"        [ ]
                                                  1     "Utility"        [ ]
                                                  
  2         "Koshucode.Baala.Overture"            /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Type"           [ ]
                                                  1     "Fn"             [ ]
                                                  2     "Text"           [ ]
                                                  
  4         "Koshucode.Baala.Base.Prelude"        /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Case"           [ ]
                                                  0     "Class"          [ ]
                                                  0     "Import"         [ ]
                                                  1     "Queue"          [ ]
                                                  4     "Pair"           [ "Overture" ]
                                                  
  6         "Koshucode.Baala.Subtext.Operator"    /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  3     "Basic"          [ ]
                                                  4     "Combine"        [ ]
                                                  5     "Repeat"         [ ]
                                                  6     "Char"           [ "Overture" ]
                                                  
  7         "Koshucode.Baala.Base.List"           /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Set"            [ ]
                                                  0     "Split"          [ ]
                                                  1     "Dots"           [ ]
                                                  4     "Assoc"          [ "Overture" ]
                                                  4     "List"           [ "Overture" ]
                                                  6     "Snip"           [ "Overture" | "Prelude" ]
                                                  7     "Order"          [ "Overture" | "Prelude" ]
                                                  
  9         "Koshucode.Baala.Base.Text"           /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Comment"        [ ]
                                                  0     "LineBreak"      [ ]
                                                  4     "MixText"        [ "Overture" ]
                                                  5     "MixClass"       [ ]
                                                  5     "MixEncode"      [ ]
                                                  9     "PPrint"         [ "List" | "Prelude" ]
                                                  9     "Suffix"         [ "List" | "Overture" ]
                                                  9     "TextTable"      [ "List" | "Overture" ]
                                                  
  10        "Koshucode.Baala.Base.IO"             /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "SimpleOption"   [ ]
                                                  6     "BzFile"         [ "Prelude" ]
                                                  6     "Exit"           [ "Prelude" ]
                                                  6     "Http"           [ "Overture" | "Prelude" ]
                                                  9     "IOPoint"        [ "List" | "Prelude" ]
                                                  10    "CodePt"         [ "Overture" | "Prelude" ]
                                                  
  10        "Koshucode.Baala.Subtext"             /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "MinMax"         [ ]
                                                  2     "Expr"           [ ]
                                                  7     "Operator"       [ ]
                                                  8     "Bundle"         [ "Operator" ]
                                                  9     "Para"           [ "Operator" ]
                                                  10    "Match"          [ "Operator" ]
                                                  
  15        "Koshucode.Baala.Base.Abort"          /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  12    "Reason"         [ "IO" ]
                                                  13    "Abortable"      [ "IO" | "Overture" ]
                                                  13    "Report"         [ "IO" | "List" | "Overture" | "Prelude" | "Text" ]
                                                  15    "Message"        [ "Abort" ]
                                                  
  17        "Koshucode.Baala.Base.Syntax"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  12    "Line"           [ "IO" | "Prelude" ]
                                                  13    "Clause"         [ "IO" | "List" | "Prelude" ]
                                                  13    "Scan"           [ "IO" | "Overture" | "Prelude" ]
                                                  15    "Message"        [ "Abort" | "IO" | "Overture" ]
                                                  16    "Tree"           [ "Abort" | "IO" | "List" | "Overture" | "Prelude" ]
                                                  17    "Infix"          [ "Overture" | "Prelude" ]
                                                  
  18        "Koshucode.Baala.Base"                /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  5     "Prelude"        [ ]
                                                  8     "List"           [ ]
                                                  10    "Text"           [ ]
                                                  11    "IO"             [ ]
                                                  14    "Abort"          [ ]
                                                  16    "Message"        [ ]
                                                  18    "Syntax"         [ ]
                                                  
  21        "Koshucode.Baala.Syntax.Para"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "Para"           [ "Base" | "Overture" ]
                                                  21    "Get"            [ "Base" ]
                                                  21    "ParaSpec"       [ "Base" | "Overture" ]
                                                  
  22        "Koshucode.Baala.Syntax.Symbol"       /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  4     "AngleText"      [ "Overture" ]
                                                  4     "Term"           [ "Overture" ]
                                                  20    "Message"        [ "Base" ]
                                                  21    "Next"           [ "Base" | "Overture" ]
                                                  22    "Short"          [ "Base" | "Overture" ]
                                                  
  23        "Koshucode.Baala.Data.Type.Decimal"   /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "Decimal"        [ "Base" | "Overture" ]
                                                  20    "Rational"       [ "Base" | "Overture" ]
                                                  21    "Fraction"       [ "Base" | "Overture" ]
                                                  22    "Coder"          [ "Base" | "Overture" ]
                                                  23    "BinaryAb"       [ "Base" ]
                                                  
  25        "Koshucode.Baala.Data.Type.Time"      /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  21    "Date"           [ "Base" | "Overture" ]
                                                  22    "Clock"          [ "Base" | "Overture" ]
                                                  23    "ClockCalc"      [ "Base" | "Overture" ]
                                                  24    "Time"           [ "Base" | "Overture" ]
                                                  25    "TimeCalc"       [ "Base" | "Overture" ]
                                                  
  29        "Koshucode.Baala.Syntax.Token"        /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "Message"        [ "Base" | "Overture" ]
                                                  24    "Token"          [ "Base" | "Symbol" ]
                                                  25    "Pattern"        [ ]
                                                  25    "Utility"        [ "Base" | "Overture" ]
                                                  26    "Nipper"         [ "Base" | "Overture" | "Symbol" ]
                                                  27    "Section"        [ "Base" | "Symbol" ]
                                                  28    "Rel"            [ "Base" | "Overture" | "Symbol" ]
                                                  29    "Clause"         [ "Base" | "Overture" | "Symbol" ]
                                                  
  33        "Koshucode.Baala.Syntax.TTree"        /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  31    "Bracket"        [ "Base" | "Token" ]
                                                  31    "Pattern"        [ "Base" | "Token" ]
                                                  32    "TokenTree"      [ "Base" | "Token" ]
                                                  33    "Parse"          [ "Base" | "Overture" | "Token" ]
                                                  
  37        "Koshucode.Baala.Syntax.Attr"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "AttrName"       [ ]
                                                  35    "Message"        [ "Base" | "Overture" | "Para" | "Symbol" | "TTree" ]
                                                  36    "Attr"           [ "Base" | "Overture" | "Para" | "TTree" ]
                                                  36    "Slot"           [ "Base" | "Overture" | "TTree" | "Token" ]
                                                  37    "AttrEd"         [ "Base" | "TTree" | "Token" ]
                                                  37    "Parse"          [ "Base" | "Overture" | "Para" ]
                                                  
  38        "Koshucode.Baala.Syntax"              /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  22    "Para"           [ ]
                                                  23    "Symbol"         [ ]
                                                  30    "Token"          [ ]
                                                  34    "TTree"          [ ]
                                                  36    "Message"        [ ]
                                                  38    "Attr"           [ ]
                                                  
  42        "Koshucode.Baala.Data.Type.Type"      /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  42    "TypeCalc"       [ "Type" ]
                                                  
  44        "Koshucode.Baala.Data.Type.Rel"       /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  41    "ShareSide"      [ "Base" | "Syntax" ]
                                                  42    "Head"           [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                  43    "Rel"            [ "Base" | "Overture" | "Syntax" ]
                                                  44    "Mono"           [ "Base" ]
                                                  
  45        "Koshucode.Baala.Data.Type"           /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "Message"        [ "Base" ]
                                                  24    "Decimal"        [ ]
                                                  26    "Time"           [ ]
                                                  40    "Judge"          [ "Base" | "Overture" | "Syntax" ]
                                                  41    "Interp"         [ "Base" | "Syntax" ]
                                                  41    "Type"           [ "Base" | "Overture" | "Syntax" ]
                                                  45    "Rel"            [ ]
                                                  
  50        "Koshucode.Baala.Core.Lexmap"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  40    "Lexmap"         [ "Base" | "Overture" | "Syntax" ]
                                                  49    "LexmapTrees"    [ "Base" | "Syntax" ]
                                                  49    "Message"        [ "Base" | "Overture" | "Syntax" ]
                                                  50    "Construct"      [ "Base" | "Overture" | "Syntax" ]
                                                  
  50        "Koshucode.Baala.Data.Content"        /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  40    "Message"        [ "Base" | "Overture" | "Syntax" ]
                                                  47    "Singleton"      [ "Base" | "Overture" | "Type" ]
                                                  47    "Tree"           [ "Base" | "Syntax" | "Type" ]
                                                  48    "Complex"        [ "Base" | "Syntax" | "Type" ]
                                                  48    "Simple"         [ "Base" | "Overture" | "Type" ]
                                                  49    "Utility"        [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                  50    "BaalaC"         [ "Base" | "Syntax" | "Type" ]
                                                  50    "Decode"         [ "Base" | "Syntax" | "Type" ]
                                                  
  53        "Koshucode.Baala.Data.Church"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  47    "Message"        [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                  48    "Cox"            [ "Base" | "Overture" | "Syntax" ]
                                                  49    "Cop"            [ "Base" | "Overture" | "Syntax" ]
                                                  52    "Build"          [ "Base" | "Content" | "Overture" | "Syntax" ]
                                                  53    "Run"            [ "Base" | "Content" | "Overture" | "Syntax" | "Type" ]
                                                  
  54        "Koshucode.Baala.Data"                /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  46    "Type"           [ "Decimal" | "Rel" | "Time" | "Type" ]
                                                  48    "Message"        [ ]
                                                  51    "Content"        [ ]
                                                  54    "Church"         [ ]
                                                  
  57        "Koshucode.Baala.Core.Relkit"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  40    "Message"        [ "Base" | "Overture" | "Syntax" ]
                                                  56    "Relkit"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ]
                                                  57    "Construct"      [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  57    "Run"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  
  61        "Koshucode.Baala.Core.Relmap"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  56    "Message"        [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  56    "Result"         [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  57    "Option"         [ "Base" | "Data" | "Syntax" ]
                                                  59    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  60    "Rop"            [ "Base" | "Lexmap" | "Syntax" ]
                                                  60    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  61    "Construct"      [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  61    "Global"         [ "Base" | "Data" | "Overture" ]
                                                  
  64        "Koshucode.Baala.Core.Assert"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  40    "Message"        [ "Base" | "Overture" | "Syntax" ]
                                                  56    "RelTable"       [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  59    "Dataset"        [ "Base" | "Data" | "Relkit" | "Syntax" ]
                                                  63    "Assert"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
                                                  64    "Run"            [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  68        "Koshucode.Baala.Core.Resource"       /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "Message"        [ "Base" | "Overture" ]
                                                  56    "Clause"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ]
                                                  66    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ]
                                                  67    "Concrete"       [ "Data" | "Relmap" | "Syntax" ]
                                                  67    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  67    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
                                                  68    "Read"           [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ]
                                                  
  69        "Koshucode.Baala.Core"                /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  51    "Lexmap"         [ ]
                                                  57    "Message"        [ ]
                                                  58    "Relkit"         [ ]
                                                  62    "Relmap"         [ ]
                                                  65    "Assert"         [ ]
                                                  69    "Resource"       [ ]
                                                  
  72        "Koshucode.Baala.Rop.Base"            /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  58    "Message"        [ "Base" ]
                                                  59    "Term"           [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  71    "Define"         [ "Core" | "Syntax" ]
                                                  71    "Get"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Rop"            [ "Base" | "Core" ]
                                                  
  72        "Koshucode.Baala.Writer"              /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  71    "Csv"            [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  71    "Html"           [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  71    "Json"           [ "Core" | "Data" | "Syntax" ]
                                                  71    "Judge"          [ "Base" | "Core" | "Data" | "Overture" ]
                                                  72    "Koshu"          [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  
  74        "Koshucode.Baala.Cop"                 /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Replace"        [ ]
                                                  56    "Coxhand"        [ "Data" | "Overture" | "Syntax" ]
                                                  57    "Logic"          [ "Base" | "Data" ]
                                                  72    "Message"        [ ]
                                                  73    "Arith"          [ "Base" | "Data" | "Overture" ]
                                                  73    "List"           [ "Base" | "Data" | "Overture" ]
                                                  73    "Misc"           [ "Base" | "Data" | "Syntax" ]
                                                  73    "Order"          [ "Base" | "Data" ]
                                                  73    "Text"           [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  73    "Time"           [ "Data" | "Overture" ]
                                                  73    "Type"           [ "Base" | "Data" ]
                                                  74    "Bundle"         [ "Base" | "Data" ]
                                                  
  75        "Koshucode.Baala"                     /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  3     "Overture"       [ "Text" ]
                                                  11    "Subtext"        [ "Operator" ]
                                                  19    "Base"           [ "Abort" | "IO" | "List" | "Prelude" | "Syntax" | "Text" ]
                                                  39    "Syntax"         [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                                  55    "Data"           [ "Church" | "Content" | "Type" ]
                                                  70    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  73    "Writer"         [ ]
                                                  75    "Cop"            [ ]
                                                  
  75        "Koshucode.Baala.Toolkit.Main"        /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  71    "KoshuRdf"       [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "KoshuSyntax"    [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  75    "KoshuChange"    [ "Base" ]
                                                  75    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                  75    "KoshuMain"      [ "Base" | "Core" | "Data" | "Overture" | "Writer" ]
                                                  
  76        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  74    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  75    "Restrict"       [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  76    "Rop"            [ "Base" | "Core" ]
                                                  
  79        "Koshucode.Baala.Rop.Flat"            /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "PoScale"        [ "Base" | "Overture" ]
                                                  71    "Message"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  74    "Elem"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  74    "Meta"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  74    "Order"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  74    "Resource"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  74    "Source"         [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  74    "Subtext"        [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Subtext" | "Syntax" ]
                                                  74    "Term"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  75    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  75    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  75    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                  77    "Lattice"        [ ]
                                                  78    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  78    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Overture" ]
                                                  79    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  82        "Koshucode.Baala.Rop.Cox"             /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "GeoDatumJp"     [ ]
                                                  72    "Message"        [ ]
                                                  74    "Get"            [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  75    "Accessor"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  75    "Calc"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  75    "Filter"         [ "Base" | "Core" | "Data" ]
                                                  75    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  75    "Range"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  81    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" ]
                                                  82    "Bundle"         [ "Core" | "Data" ]
                                                  
  83        "Koshucode.Baala.Rop.Nest"            /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  72    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  74    "Flow"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  81    "Confl"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ]
                                                  82    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ]
                                                  83    "Rop"            [ "Base" | "Core" | "Data" ]
                                                  
  84        "Koshucode.Baala.Rop"                 /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  73    "Base"           [ ]
                                                  80    "Flat"           [ "Lattice" ]
                                                  83    "Cox"            [ ]
                                                  84    "Nest"           [ ]
                                                  
  85        "Koshucode.Baala.Toolkit.Library"     /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Version"        [ ]
                                                  56    "RDF"            [ "Data" ]
                                                  71    "Element"        [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  71    "Input"          [ "Base" | "Core" ]
                                                  74    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                  74    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                  85    "Global"         [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      43 judges on DIR-RANK
**      43 judges in total
**

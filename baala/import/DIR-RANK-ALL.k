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
    | [ ] ] [ 0 | "Integer" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 1 | "Utility" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Overture"  /base-rank {= /rank /base /import-dir [ 0 | "Type" | [ ] ] [
    1 | "Fn" | [ ] ] [ 2 | "Text" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Case" | [ ]
    ] [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Queue" | [ ] ] [ 4 | "Pair" | [ "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Base.MixText"  /base-rank {= /rank /base /import-dir [ 0 | "LineBreak"
    | [ ] ] [ 4 | "MixText" | [ "Overture" ] ] [ 5 | "MixClass" | [ ] ] [ 5 | "MixEncode" | [ ] ] [ 6 | "Deriv" | [ ] ]
    =}

|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Subtext.Operator"  /base-rank {= /rank /base /import-dir [ 3 | "Basic"
    | [ ] ] [ 4 | "Combine" | [ ] ] [ 5 | "Repeat" | [ ] ] [ 6 | "Char" | [ "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Split" | [ ] ]
    [ 4 | "Assoc" | [ "Overture" ] ] [ 4 | "Dots" | [ "Overture" ] ] [ 4 | "List" | [ "Overture" ] ] [ 4 | "Set" | [
    "Overture" ] ] [ 6 | "Snip" | [ "Overture" | "Prelude" ] ] [ 7 | "Order" | [ "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 9  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 4 | "Comment" | [
    "Overture" ] ] [ 6 | "PPrint" | [ "Overture" | "Prelude" ] ] [ 9 | "Suffix" | [ "List" | "Overture" ] ] [ 9 |
    "TextTable" | [ "List" | "Overture" ] ] =}
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
    "Prelude" ] ] [ 15 | "Message" | [ "Abort" | "IO" ] ] [ 16 | "Tree" | [ "Abort" | "IO" | "List" | "Overture" |
    "Prelude" ] ] [ 17 | "Infix" | [ "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 5 | "Prelude" | [ ] ] [
    7 | "MixText" | [ ] ] [ 8 | "List" | [ ] ] [ 10 | "Text" | [ ] ] [ 11 | "IO" | [ ] ] [ 14 | "Abort" | [ ] ] [ 16 |
    "Message" | [ ] ] [ 18 | "Syntax" | [ ] ] =}
|-- DIR-RANK  /dir-rank 21  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 20 | "Para" | [
    "Base" | "Overture" ] ] [ 21 | "Get" | [ "Base" ] ] [ 21 | "ParaSpec" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 22  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 4 |
    "AngleText" | [ "Overture" ] ] [ 4 | "Term" | [ "Overture" ] ] [ 20 | "Message" | [ "Base" ] ] [ 21 | "Next" | [
    "Base" | "Overture" ] ] [ 22 | "Short" | [ "Base" | "Overture" ] ] =}

|-- DIR-RANK  /dir-rank 23  /dir "Koshucode.Baala.Data.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 4 |
    "Rational" | [ "Overture" ] ] [ 20 | "Decimal" | [ "Base" | "Overture" ] ] [ 21 | "Fraction" | [ "Overture" ] ] [ 22
    | "Coder" | [ "Base" | "Overture" ] ] [ 23 | "BinaryAb" | [ "Base" ] ] =}
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
    [ ] ] [ 35 | "Message" | [ "Base" | "Para" | "Symbol" | "TTree" ] ] [ 36 | "Attr" | [ "Base" | "Overture" | "Para" |
    "TTree" ] ] [ 36 | "Slot" | [ "Base" | "Overture" | "TTree" | "Token" ] ] [ 37 | "AttrEd" | [ "Base" | "TTree" |
    "Token" ] ] [ 37 | "Parse" | [ "Base" | "Overture" | "Para" ] ] =}

|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 22 | "Para" | [ ] ] [
    23 | "Symbol" | [ ] ] [ 30 | "Token" | [ ] ] [ 34 | "TTree" | [ ] ] [ 36 | "Message" | [ ] ] [ 38 | "Attr" | [ ] ]
    =}
|-- DIR-RANK  /dir-rank 43  /dir "Koshucode.Baala.Data.Type.Type"  /base-rank {= /rank /base /import-dir [ 43 |
    "TypeCalc" | [ "Type" ] ] =}
|-- DIR-RANK  /dir-rank 45  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 41 |
    "TermPicker" | [ "Base" | "Overture" | "Syntax" ] ] [ 43 | "Head" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [
    44 | "Rel" | [ "Base" | "Overture" | "Syntax" ] ] [ 45 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 46  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 20 | "Message" | [
    "Base" ] ] [ 24 | "Decimal" | [ ] ] [ 26 | "Time" | [ ] ] [ 40 | "JudgeClass" | [ "Base" | "Syntax" ] ] [ 41 |
    "Judge" | [ "Base" | "Overture" | "Syntax" ] ] [ 42 | "Interp" | [ "Base" | "Syntax" ] ] [ 42 | "Type" | [ "Base" |
    "Overture" | "Syntax" ] ] [ 46 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 51  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 40 | "Lexmap" |
    [ "Base" | "Overture" | "Syntax" ] ] [ 50 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [ 50 | "Message" | [ "Base" |
    "Syntax" ] ] [ 51 | "Construct" | [ "Base" | "Overture" | "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 51  /dir "Koshucode.Baala.Data.Class"  /base-rank {= /rank /base /import-dir [ 20 | "Message" |
    [ "Base" ] ] [ 48 | "Singleton" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 49 | "Simple" | [ "Base" |
    "Overture" | "Type" ] ] [ 50 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 51 | "Content" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 54  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 40 | "Message"
    | [ "Base" | "Overture" | "Syntax" ] ] [ 41 | "Term" | [ "Base" | "Syntax" ] ] [ 48 | "Tree" | [ "Base" | "Syntax" |
    "Type" ] ] [ 53 | "Decode" | [ "Base" | "Class" | "Syntax" | "Type" ] ] [ 53 | "Utility" | [ "Base" | "Class" |
    "Overture" | "Syntax" | "Type" ] ] [ 54 | "BaalaC" | [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 57  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 48 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 49 | "Cox" | [ "Base" | "Overture" | "Syntax" ] ] [ 50 | "Cop" | [ "Base" |
    "Overture" | "Syntax" ] ] [ 56 | "Build" | [ "Base" | "Class" | "Content" | "Overture" | "Syntax" ] ] [ 57 | "Run" |
    [ "Base" | "Class" | "Content" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 58  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 47 | "Type" | [
    "Decimal" | "Rel" | "Time" | "Type" ] ] [ 49 | "Message" | [ ] ] [ 52 | "Class" | [ ] ] [ 55 | "Content" | [ ] ] [
    58 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Overture" | "Syntax" ] ] [ 60 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ] ] [ 61
    | "Construct" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 61 | "Run" | [ "Base" | "Data" | "Overture" |
    "Syntax" ] ] =}

|-- DIR-RANK  /dir-rank 65  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 60 | "Message" |
    [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 60 | "Result" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 61 |
    "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 63 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" ] ] [ 64 | "Rop" | [ "Base" | "Lexmap" | "Syntax" ] ] [ 64 | "Specialize" | [ "Base" | "Data" | "Lexmap" |
    "Overture" | "Relkit" | "Syntax" ] ] [ 65 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" ] ] [ 65 | "Global" | [ "Base" | "Data" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 68  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Overture" | "Syntax" ] ] [ 60 | "RelTable" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 63 |
    "Dataset" | [ "Base" | "Data" | "Relkit" | "Syntax" ] ] [ 67 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Overture"
    | "Relmap" | "Syntax" ] ] [ 68 | "Run" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax"
    ] ] =}
|-- DIR-RANK  /dir-rank 72  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 20 | "Message"
    | [ "Base" | "Overture" ] ] [ 60 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ] ] [ 70 |
    "Resource" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ] ] [ 71 |
    "Concrete" | [ "Data" | "Relmap" | "Syntax" ] ] [ 71 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" |
    "Relmap" | "Syntax" ] ] [ 71 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
    ] [ 72 | "Read" | [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 73  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 52 | "Lexmap" | [ ] ] [
    61 | "Message" | [ ] ] [ 62 | "Relkit" | [ ] ] [ 66 | "Relmap" | [ ] ] [ 69 | "Assert" | [ ] ] [ 73 | "Resource" | [
    ] ] =}
|-- DIR-RANK  /dir-rank 76  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 62 | "Message" | [
    "Base" ] ] [ 63 | "Term" | [ "Base" | "Syntax" ] ] [ 75 | "Define" | [ "Core" | "Syntax" ] ] [ 75 | "Get" | [ "Base"
    | "Core" | "Data" | "Syntax" ] ] [ 76 | "Rop" | [ "Base" | "Core" ] ] =}

|-- DIR-RANK  /dir-rank 76  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 75 | "Csv" | [ "Base"
    | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 75 | "Html" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ]
    [ 75 | "Json" | [ "Core" | "Data" | "Syntax" ] ] [ 75 | "Judge" | [ "Base" | "Core" | "Data" | "Overture" ] ] [ 76 |
    "Koshu" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    60 | "Coxhand" | [ "Data" | "Overture" | "Syntax" ] ] [ 61 | "Logic" | [ "Data" | "Overture" ] ] [ 76 | "Message" |
    [ ] ] [ 77 | "Arith" | [ "Base" | "Data" | "Overture" ] ] [ 77 | "List" | [ "Base" | "Data" | "Overture" ] ] [ 77 |
    "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 77 | "Order" | [ "Data" | "Overture" ] ] [ 77 | "Text" | [ "Base" |
    "Data" | "Overture" | "Syntax" ] ] [ 77 | "Time" | [ "Data" | "Overture" ] ] [ 77 | "Type" | [ "Base" | "Data" ] ] [
    78 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 3 | "Overture" | [ "Text" ]
    ] [ 11 | "Subtext" | [ "Operator" ] ] [ 19 | "Base" | [ "Abort" | "IO" | "List" | "MixText" | "Prelude" | "Syntax" |
    "Text" ] ] [ 39 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ] ] [ 59 | "Data" | [ "Church" |
    "Class" | "Content" | "Type" ] ] [ 74 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 77 |
    "Writer" | [ ] ] [ 79 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 75 | "KoshuRdf"
    | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 75 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" ] ] [ 79 | "KoshuChange" | [ "Base" ] ] [ 79 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 79 |
    "KoshuMain" | [ "Base" | "Core" | "Data" | "Overture" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 78 |
    "Tropashko" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 79 | "Restrict" | [ "Base" | "Base" | "Core" |
    "Data" | "Overture" | "Syntax" ] ] [ 80 | "Rop" | [ "Base" | "Core" ] ] =}

|-- DIR-RANK  /dir-rank 83  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 20 | "PoScale" | [
    "Base" | "Overture" ] ] [ 75 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 78 | "Elem" | [ "Base" |
    "Base" | "Core" | "Data" | "Syntax" ] ] [ 78 | "Meta" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 78 |
    "Order" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 78 | "Resource" | [ "Base" | "Base" | "Core" | "Data"
    | "Syntax" ] ] [ 78 | "Source" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 78 | "Subtext" | [ "Base"
    | "Base" | "Core" | "Data" | "Overture" | "Subtext" | "Syntax" ] ] [ 78 | "Term" | [ "Base" | "Base" | "Core" |
    "Data" | "Syntax" ] ] [ 79 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 79 |
    "Peripheral" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 79 | "TermGadget" | [ "Base" | "Core" | "Data" ]
    ] [ 81 | "Lattice" | [ ] ] [ 82 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ] ] [ 82 |
    "Control" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Overture" ] ] [ 83 | "Bundle" | [ "Base" | "Core" |
    "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 86  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 76 | "Message" | [ ] ] [ 78 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 79 | "Accessor" | [
    "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 79 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ]
    [ 79 | "Filter" | [ "Base" | "Core" | "Data" ] ] [ 79 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Overture"
    | "Syntax" ] ] [ 79 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 85 | "Empty" | [ "Base" |
    "Base" | "Core" | "Data" | "Flat" | "Overture" ] ] [ 86 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 87  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 76 | "Message" | [
    "Base" | "Data" | "Syntax" ] ] [ 78 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 85 | "Confl" | [
    "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ] ] [ 86 | "Deriv" | [ "Base" | "Base" | "Core" |
    "Data" | "Flat" | "Overture" | "Syntax" ] ] [ 87 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 88  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 77 | "Base" | [ ] ] [ 84
    | "Flat" | [ "Lattice" ] ] [ 87 | "Cox" | [ ] ] [ 88 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 89  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 60 | "RDF" | [ "Data" ] ] [ 75 | "Element" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax"
    ] ] [ 75 | "Input" | [ "Base" | "Core" ] ] [ 78 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 78 | "Run"
    | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 89 | "Global" | [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" |
    "Nest" ] ] =}

*** 45 judges

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
                                                  0     "Integer"        [ ]
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
                                                  
  6         "Koshucode.Baala.Base.MixText"        /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "LineBreak"      [ ]
                                                  4     "MixText"        [ "Overture" ]
                                                  5     "MixClass"       [ ]
                                                  5     "MixEncode"      [ ]
                                                  6     "Deriv"          [ ]
                                                  
  6         "Koshucode.Baala.Subtext.Operator"    /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  3     "Basic"          [ ]
                                                  4     "Combine"        [ ]
                                                  5     "Repeat"         [ ]
                                                  6     "Char"           [ "Overture" ]
                                                  
  7         "Koshucode.Baala.Base.List"           /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Split"          [ ]
                                                  4     "Assoc"          [ "Overture" ]
                                                  4     "Dots"           [ "Overture" ]
                                                  4     "List"           [ "Overture" ]
                                                  4     "Set"            [ "Overture" ]
                                                  6     "Snip"           [ "Overture" | "Prelude" ]
                                                  7     "Order"          [ "Overture" | "Prelude" ]
                                                  
  9         "Koshucode.Baala.Base.Text"           /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  4     "Comment"        [ "Overture" ]
                                                  6     "PPrint"         [ "Overture" | "Prelude" ]
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
                                                  15    "Message"        [ "Abort" | "IO" ]
                                                  16    "Tree"           [ "Abort" | "IO" | "List" | "Overture" | "Prelude" ]
                                                  17    "Infix"          [ "Overture" | "Prelude" ]
                                                  
  18        "Koshucode.Baala.Base"                /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  5     "Prelude"        [ ]
                                                  7     "MixText"        [ ]
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
                                                  4     "Rational"       [ "Overture" ]
                                                  20    "Decimal"        [ "Base" | "Overture" ]
                                                  21    "Fraction"       [ "Overture" ]
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
                                                  35    "Message"        [ "Base" | "Para" | "Symbol" | "TTree" ]
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
                                                  
  43        "Koshucode.Baala.Data.Type.Type"      /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  43    "TypeCalc"       [ "Type" ]
                                                  
  45        "Koshucode.Baala.Data.Type.Rel"       /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  41    "TermPicker"     [ "Base" | "Overture" | "Syntax" ]
                                                  43    "Head"           [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                  44    "Rel"            [ "Base" | "Overture" | "Syntax" ]
                                                  45    "Mono"           [ "Base" ]
                                                  
  46        "Koshucode.Baala.Data.Type"           /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "Message"        [ "Base" ]
                                                  24    "Decimal"        [ ]
                                                  26    "Time"           [ ]
                                                  40    "JudgeClass"     [ "Base" | "Syntax" ]
                                                  41    "Judge"          [ "Base" | "Overture" | "Syntax" ]
                                                  42    "Interp"         [ "Base" | "Syntax" ]
                                                  42    "Type"           [ "Base" | "Overture" | "Syntax" ]
                                                  46    "Rel"            [ ]
                                                  
  51        "Koshucode.Baala.Core.Lexmap"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  40    "Lexmap"         [ "Base" | "Overture" | "Syntax" ]
                                                  50    "LexmapTrees"    [ "Base" | "Syntax" ]
                                                  50    "Message"        [ "Base" | "Syntax" ]
                                                  51    "Construct"      [ "Base" | "Overture" | "Syntax" ]
                                                  
  51        "Koshucode.Baala.Data.Class"          /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "Message"        [ "Base" ]
                                                  48    "Singleton"      [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                  49    "Simple"         [ "Base" | "Overture" | "Type" ]
                                                  50    "Complex"        [ "Base" | "Syntax" | "Type" ]
                                                  51    "Content"        [ "Base" ]
                                                  
  54        "Koshucode.Baala.Data.Content"        /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  40    "Message"        [ "Base" | "Overture" | "Syntax" ]
                                                  41    "Term"           [ "Base" | "Syntax" ]
                                                  48    "Tree"           [ "Base" | "Syntax" | "Type" ]
                                                  53    "Decode"         [ "Base" | "Class" | "Syntax" | "Type" ]
                                                  53    "Utility"        [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                  54    "BaalaC"         [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                  
  57        "Koshucode.Baala.Data.Church"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  48    "Message"        [ "Base" | "Syntax" | "Type" ]
                                                  49    "Cox"            [ "Base" | "Overture" | "Syntax" ]
                                                  50    "Cop"            [ "Base" | "Overture" | "Syntax" ]
                                                  56    "Build"          [ "Base" | "Class" | "Content" | "Overture" | "Syntax" ]
                                                  57    "Run"            [ "Base" | "Class" | "Content" | "Overture" | "Syntax" | "Type" ]
                                                  
  58        "Koshucode.Baala.Data"                /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  47    "Type"           [ "Decimal" | "Rel" | "Time" | "Type" ]
                                                  49    "Message"        [ ]
                                                  52    "Class"          [ ]
                                                  55    "Content"        [ ]
                                                  58    "Church"         [ ]
                                                  
  61        "Koshucode.Baala.Core.Relkit"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  40    "Message"        [ "Base" | "Overture" | "Syntax" ]
                                                  60    "Relkit"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ]
                                                  61    "Construct"      [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  61    "Run"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  
  65        "Koshucode.Baala.Core.Relmap"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  60    "Message"        [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  60    "Result"         [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  61    "Option"         [ "Base" | "Data" | "Syntax" ]
                                                  63    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  64    "Rop"            [ "Base" | "Lexmap" | "Syntax" ]
                                                  64    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  65    "Construct"      [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  65    "Global"         [ "Base" | "Data" | "Overture" ]
                                                  
  68        "Koshucode.Baala.Core.Assert"         /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  40    "Message"        [ "Base" | "Overture" | "Syntax" ]
                                                  60    "RelTable"       [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  63    "Dataset"        [ "Base" | "Data" | "Relkit" | "Syntax" ]
                                                  67    "Assert"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
                                                  68    "Run"            [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  72        "Koshucode.Baala.Core.Resource"       /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "Message"        [ "Base" | "Overture" ]
                                                  60    "Clause"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ]
                                                  70    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ]
                                                  71    "Concrete"       [ "Data" | "Relmap" | "Syntax" ]
                                                  71    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  71    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
                                                  72    "Read"           [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ]
                                                  
  73        "Koshucode.Baala.Core"                /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  52    "Lexmap"         [ ]
                                                  61    "Message"        [ ]
                                                  62    "Relkit"         [ ]
                                                  66    "Relmap"         [ ]
                                                  69    "Assert"         [ ]
                                                  73    "Resource"       [ ]
                                                  
  76        "Koshucode.Baala.Rop.Base"            /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  62    "Message"        [ "Base" ]
                                                  63    "Term"           [ "Base" | "Syntax" ]
                                                  75    "Define"         [ "Core" | "Syntax" ]
                                                  75    "Get"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  76    "Rop"            [ "Base" | "Core" ]
                                                  
  76        "Koshucode.Baala.Writer"              /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  75    "Csv"            [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  75    "Html"           [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  75    "Json"           [ "Core" | "Data" | "Syntax" ]
                                                  75    "Judge"          [ "Base" | "Core" | "Data" | "Overture" ]
                                                  76    "Koshu"          [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  
  78        "Koshucode.Baala.Cop"                 /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Replace"        [ ]
                                                  60    "Coxhand"        [ "Data" | "Overture" | "Syntax" ]
                                                  61    "Logic"          [ "Data" | "Overture" ]
                                                  76    "Message"        [ ]
                                                  77    "Arith"          [ "Base" | "Data" | "Overture" ]
                                                  77    "List"           [ "Base" | "Data" | "Overture" ]
                                                  77    "Misc"           [ "Base" | "Data" | "Syntax" ]
                                                  77    "Order"          [ "Data" | "Overture" ]
                                                  77    "Text"           [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  77    "Time"           [ "Data" | "Overture" ]
                                                  77    "Type"           [ "Base" | "Data" ]
                                                  78    "Bundle"         [ "Base" | "Data" ]
                                                  
  79        "Koshucode.Baala"                     /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  3     "Overture"       [ "Text" ]
                                                  11    "Subtext"        [ "Operator" ]
                                                  19    "Base"           [ "Abort" | "IO" | "List" | "MixText" | "Prelude" | "Syntax" | "Text" ]
                                                  39    "Syntax"         [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                                  59    "Data"           [ "Church" | "Class" | "Content" | "Type" ]
                                                  74    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  77    "Writer"         [ ]
                                                  79    "Cop"            [ ]
                                                  
  79        "Koshucode.Baala.Toolkit.Main"        /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  75    "KoshuRdf"       [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  75    "KoshuSyntax"    [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  79    "KoshuChange"    [ "Base" ]
                                                  79    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                  79    "KoshuMain"      [ "Base" | "Core" | "Data" | "Overture" | "Writer" ]
                                                  
  80        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  78    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Restrict"       [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  80    "Rop"            [ "Base" | "Core" ]
                                                  
  83        "Koshucode.Baala.Rop.Flat"            /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  20    "PoScale"        [ "Base" | "Overture" ]
                                                  75    "Message"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  78    "Elem"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  78    "Meta"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  78    "Order"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  78    "Resource"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  78    "Source"         [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  78    "Subtext"        [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Subtext" | "Syntax" ]
                                                  78    "Term"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  79    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                  81    "Lattice"        [ ]
                                                  82    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  82    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Overture" ]
                                                  83    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  86        "Koshucode.Baala.Rop.Cox"             /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "GeoDatumJp"     [ ]
                                                  76    "Message"        [ ]
                                                  78    "Get"            [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Accessor"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Calc"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Filter"         [ "Base" | "Core" | "Data" ]
                                                  79    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  79    "Range"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  85    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" ]
                                                  86    "Bundle"         [ "Core" | "Data" ]
                                                  
  87        "Koshucode.Baala.Rop.Nest"            /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  76    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  78    "Flow"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  85    "Confl"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ]
                                                  86    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ]
                                                  87    "Rop"            [ "Base" | "Core" | "Data" ]
                                                  
  88        "Koshucode.Baala.Rop"                 /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  77    "Base"           [ ]
                                                  84    "Flat"           [ "Lattice" ]
                                                  87    "Cox"            [ ]
                                                  88    "Nest"           [ ]
                                                  
  89        "Koshucode.Baala.Toolkit.Library"     /rank /base            /import-dir
                                                  ----- ---------------- -----------------------------------------------------------------------------------------
                                                  0     "Version"        [ ]
                                                  60    "RDF"            [ "Data" ]
                                                  75    "Element"        [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  75    "Input"          [ "Base" | "Core" ]
                                                  78    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                  78    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                  89    "Global"         [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      45 judges on DIR-RANK
**      45 judges in total
**

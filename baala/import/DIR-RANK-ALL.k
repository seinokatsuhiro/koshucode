** -*- koshu -*-
**
**  INPUT
**    dir-rank.k
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

|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.Overture.Name"  /base-rank {= /rank /base /import-dir [ 0 | "String" |
    [ ] ] =}
|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Overture.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Category"
    | [ ] ] [ 0 | "Integer" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 1 | "Utility" | [ ] ] =}
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Overture"  /base-rank {= /rank /base /import-dir [ 0 | "Cache" | [ ] ]
    [ 0 | "Type" | [ ] ] [ 1 | "Fn" | [ ] ] [ 1 | "Shorthand" | [ ] ] [ 2 | "Text" | [ ] ] =}
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
    "Overture" ] ] [ 4 | "TextTable" | [ "Overture" ] ] [ 6 | "PPrint" | [ "Overture" | "Prelude" ] ] [ 9 | "Suffix" | [
    "List" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Subtext"  /base-rank {= /rank /base /import-dir [ 0 | "MinMax" | [ ] ]
    [ 2 | "Expr" | [ ] ] [ 7 | "Operator" | [ ] ] [ 8 | "Bundle" | [ "Operator" ] ] [ 9 | "Para" | [ "Operator" ] ] [ 10
    | "Match" | [ "Operator" ] ] =}
|-- DIR-RANK  /dir-rank 11  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" |
    [ ] ] [ 6 | "BzFile" | [ "Prelude" ] ] [ 6 | "Exit" | [ "Prelude" ] ] [ 6 | "Http" | [ "Overture" | "Prelude" ] ] [
    9 | "IOPoint" | [ "List" | "Prelude" ] ] [ 11 | "CodePos" | [ "Prelude" | "Text" ] ] =}

|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 13 | "Reason" | [
    "IO" | "Overture" ] ] [ 14 | "Report" | [ "IO" | "List" | "Overture" | "Prelude" | "Text" ] ] [ 16 | "Message" | [
    "Abort" ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Base.Code"  /base-rank {= /rank /base /import-dir [ 13 | "Line" | [
    "IO" | "Prelude" ] ] [ 14 | "Clause" | [ "IO" | "List" | "Prelude" ] ] [ 14 | "Scan" | [ "IO" | "Overture" |
    "Prelude" ] ] [ 16 | "Message" | [ "Abort" | "IO" ] ] [ 17 | "Tree" | [ "Abort" | "IO" | "List" | "Overture" |
    "Prelude" ] ] [ 18 | "Infix" | [ "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 19  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 5 | "Prelude" | [ ] ] [
    7 | "MixText" | [ ] ] [ 8 | "List" | [ ] ] [ 10 | "Text" | [ ] ] [ 12 | "IO" | [ ] ] [ 15 | "Abort" | [ ] ] [ 17 |
    "Message" | [ ] ] [ 19 | "Code" | [ ] ] =}
|-- DIR-RANK  /dir-rank 22  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 21 | "Para" | [
    "Base" | "Overture" ] ] [ 22 | "Get" | [ "Base" ] ] [ 22 | "ParaSpec" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 23  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 4 |
    "AngleText" | [ "Overture" ] ] [ 4 | "Term" | [ "Overture" ] ] [ 21 | "Message" | [ "Base" ] ] [ 22 | "Next" | [
    "Base" | "Overture" ] ] [ 23 | "Short" | [ "Base" | "Overture" ] ] =}

|-- DIR-RANK  /dir-rank 24  /dir "Koshucode.Baala.Data.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 4 |
    "Rational" | [ "Overture" ] ] [ 21 | "Decimal" | [ "Base" | "Overture" ] ] [ 22 | "Fraction" | [ "Overture" ] ] [ 23
    | "Coder" | [ "Base" | "Overture" ] ] [ 24 | "BinaryAb" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 26  /dir "Koshucode.Baala.Data.Type.Time"  /base-rank {= /rank /base /import-dir [ 22 | "Date" |
    [ "Base" | "Overture" ] ] [ 23 | "Clock" | [ "Base" | "Overture" ] ] [ 24 | "ClockCalc" | [ "Base" | "Overture" ] ]
    [ 25 | "Time" | [ "Base" | "Overture" ] ] [ 26 | "TimeCalc" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 30  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 21 | "Message"
    | [ "Base" ] ] [ 25 | "Token" | [ "Base" | "Symbol" ] ] [ 26 | "Pattern" | [ ] ] [ 26 | "Utility" | [ "Base" |
    "Overture" | "Symbol" ] ] [ 27 | "Nipper" | [ "Base" | "Overture" | "Symbol" ] ] [ 28 | "Section" | [ "Base" |
    "Symbol" ] ] [ 29 | "Rel" | [ "Base" | "Overture" | "Symbol" ] ] [ 30 | "Clause" | [ "Base" | "Overture" | "Symbol"
    ] ] =}
|-- DIR-RANK  /dir-rank 34  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 32 | "Bracket"
    | [ "Base" | "Token" ] ] [ 32 | "Pattern" | [ "Base" | "Token" ] ] [ 33 | "TokenTree" | [ "Base" | "Token" ] ] [ 34
    | "Parse" | [ "Base" | "Overture" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 37  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 25 | "Message" | [ "Base" | "Para" | "Symbol" ] ] [ 36 | "Attr" | [ "Base" | "Overture" | "Para" | "TTree" ]
    ] [ 36 | "Slot" | [ "Base" | "Overture" | "TTree" | "Token" ] ] [ 37 | "AttrEd" | [ "Base" | "Symbol" | "TTree" |
    "Token" ] ] [ 37 | "Parse" | [ "Base" | "Overture" | "Para" ] ] =}

|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 23 | "Para" | [ ] ] [
    24 | "Symbol" | [ ] ] [ 26 | "Message" | [ ] ] [ 31 | "Token" | [ ] ] [ 35 | "TTree" | [ ] ] [ 38 | "Attr" | [ ] ]
    =}
|-- DIR-RANK  /dir-rank 42  /dir "Koshucode.Baala.Data.Type.Judge"  /base-rank {= /rank /base /import-dir [ 40 |
    "JudgeClass" | [ "Base" | "Syntax" ] ] [ 41 | "Judge" | [ "Base" | "Overture" | "Syntax" ] ] [ 42 | "Interp" | [
    "Base" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 47  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 44 |
    "TermPicker" | [ "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 45 | "Head" | [ "Base" | "Judge" | "Overture" |
    "Syntax" | "Type" ] ] [ 46 | "Rel" | [ "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 47 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 48  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 21 | "Message" | [
    "Base" ] ] [ 25 | "Decimal" | [ ] ] [ 27 | "Time" | [ ] ] [ 43 | "Judge" | [ ] ] [ 44 | "Type" | [ "Base" | "Judge"
    | "Overture" | "Syntax" ] ] [ 48 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 53  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 21 | "Message" |
    [ "Base" ] ] [ 40 | "Lexmap" | [ "Base" | "Overture" | "Syntax" ] ] [ 52 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [
    53 | "Construct" | [ "Base" | "Overture" | "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 54  /dir "Koshucode.Baala.Data.Class"  /base-rank {= /rank /base /import-dir [ 21 | "Message" |
    [ "Base" ] ] [ 50 | "Singleton" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 51 | "Simple" | [ "Base" |
    "Overture" | "Syntax" | "Type" ] ] [ 52 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 53 | "Content" | [ "Base" ]
    ] [ 53 | "Map" | [ "Overture" ] ] [ 54 | "Encode" | [ "Base" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 56  /dir "Koshucode.Baala.Data.Decode"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Syntax" ] ] [ 41 | "Term" | [ "Base" | "Overture" | "Syntax" ] ] [ 50 | "Type" | [ "Base" | "Syntax" |
    "Type" ] ] [ 51 | "Numeric" | [ "Base" | "Syntax" | "Type" ] ] [ 56 | "Content" | [ "Base" | "Class" | "Syntax" |
    "Type" ] ] =}
|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 50 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 51 | "Cox" | [ "Base" | "Overture" | "Syntax" ] ] [ 52 | "Cop" | [ "Base" |
    "Overture" | "Syntax" ] ] [ 58 | "Build" | [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ] ] [ 59 | "Run" |
    [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 60  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 49 | "Type" | [
    "Decimal" | "Judge" | "Rel" | "Time" | "Type" ] ] [ 51 | "Message" | [ ] ] [ 55 | "Class" | [ ] ] [ 57 | "Decode" |
    [ ] ] [ 58 | "Content" | [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ] ] [ 60 | "Church" | [ ] ]
    =}
|-- DIR-RANK  /dir-rank 62  /dir "Koshucode.Baala.Data.Type.Type"  /base-rank {= /rank /base /import-dir [ 56 | "Genus"
    | [ "Class" | "Overture" ] ] [ 62 | "Predefined where" | [ "Class" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 63  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Syntax" ] ] [ 62 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ] ] [ 63 |
    "Construct" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 63 | "Run" | [ "Base" | "Data" | "Overture" | "Syntax"
    ] ] =}
|-- DIR-RANK  /dir-rank 67  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 62 | "Message" |
    [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 62 | "Result" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 63 |
    "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 65 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" ] ] [ 66 | "Rop" | [ "Base" | "Lexmap" | "Syntax" ] ] [ 66 | "Specialize" | [ "Base" | "Data" | "Lexmap" |
    "Overture" | "Relkit" | "Syntax" ] ] [ 67 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" ] ] [ 67 | "Global" | [ "Base" | "Data" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Syntax" ] ] [ 62 | "Dataset" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 62 | "RelTable" | [
    "Base" | "Data" | "Overture" | "Syntax" ] ] [ 69 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" |
    "Syntax" ] ] [ 70 | "Run" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 74  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 21 | "Message"
    | [ "Base" ] ] [ 62 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ] ] [ 72 | "Resource" | [
    "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ] ] [ 73 | "Concrete" | [ "Data"
    | "Relmap" | "Syntax" ] ] [ 73 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 73
    | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ] ] [ 74 | "Read" | [ "Base" |
    "Data" | "Overture" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 75  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 54 | "Lexmap" | [ ] ] [
    63 | "Message" | [ ] ] [ 64 | "Relkit" | [ ] ] [ 68 | "Relmap" | [ ] ] [ 71 | "Assert" | [ ] ] [ 75 | "Resource" | [
    ] ] =}

|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 64 | "Message" | [
    "Base" ] ] [ 77 | "Define" | [ "Core" | "Overture" | "Syntax" ] ] [ 77 | "Get" | [ "Base" | "Core" | "Data" |
    "Overture" | "Syntax" ] ] [ 78 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 77 | "Csv" | [ "Base"
    | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 77 | "Html" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ]
    [ 77 | "Json" | [ "Core" | "Data" | "Syntax" ] ] [ 77 | "Judge" | [ "Base" | "Core" | "Data" | "Overture" ] ] [ 78 |
    "Koshu" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    62 | "Coxhand" | [ "Data" | "Overture" | "Syntax" ] ] [ 63 | "Logic" | [ "Data" | "Overture" ] ] [ 78 | "Message" |
    [ ] ] [ 79 | "Arith" | [ "Base" | "Data" | "Overture" ] ] [ 79 | "List" | [ "Base" | "Data" | "Overture" ] ] [ 79 |
    "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 79 | "Order" | [ "Data" | "Overture" ] ] [ 79 | "Text" | [ "Base" |
    "Data" | "Overture" | "Syntax" ] ] [ 79 | "Time" | [ "Data" | "Overture" ] ] [ 79 | "Type" | [ "Base" | "Data" ] ] [
    80 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 3 | "Overture" | [ "Text" ]
    ] [ 11 | "Subtext" | [ "Operator" ] ] [ 20 | "Base" | [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" |
    "Text" ] ] [ 39 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ] ] [ 61 | "Data" | [ "Church" |
    "Class" | "Decode" | "Type" ] ] [ 62 | "DataPlus" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 76 | "Core" | [
    "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 79 | "Writer" | [ ] ] [ 81 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 63 | "KoshuRdf"
    | [ "Base" | "Data" | "Syntax" ] ] [ 77 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [
    81 | "KoshuChange" | [ "Base" ] ] [ 81 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 81 | "KoshuMain" | [
    "Base" | "Core" | "Data" | "Overture" | "Writer" ] ] =}

|-- DIR-RANK  /dir-rank 82  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 80 |
    "Tropashko" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 81 | "Restrict" | [ "Base" | "Base" | "Core" |
    "Data" | "Overture" | "Syntax" ] ] [ 82 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 85  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 21 | "PoScale" | [
    "Base" | "Overture" ] ] [ 77 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 80 | "Elem" | [ "Base" |
    "Base" | "Core" | "Data" | "Syntax" ] ] [ 80 | "Meta" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 80 |
    "Order" | [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 80 | "Resource" | [ "Base" | "Base" |
    "Core" | "Data" | "Syntax" ] ] [ 80 | "Source" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 80 |
    "Subtext" | [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Subtext" | "Syntax" ] ] [ 80 | "Term" | [ "Base" |
    "Base" | "Core" | "Data" | "Syntax" ] ] [ 81 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Overture" |
    "Syntax" ] ] [ 81 | "Peripheral" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 81 | "TermGadget" | [ "Base"
    | "Core" | "Data" | "Syntax" ] ] [ 83 | "Lattice" | [ ] ] [ 84 | "Check" | [ "Base" | "Base" | "Core" | "Data" |
    "Lattice" | "Syntax" ] ] [ 84 | "Control" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Overture" ] ] [ 85 |
    "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 88  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 78 | "Message" | [ ] ] [ 80 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 81 | "Accessor" | [
    "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 81 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Overture" |
    "Syntax" ] ] [ 81 | "Filter" | [ "Base" | "Core" | "Data" ] ] [ 81 | "Gadget" | [ "Base" | "Base" | "Core" | "Data"
    | "Overture" | "Syntax" ] ] [ 81 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 87 | "Empty" | [
    "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" ] ] [ 88 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 89  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 78 | "Message" | [
    "Base" | "Data" | "Syntax" ] ] [ 80 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 87 | "Confl" | [
    "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ] ] [ 88 | "Deriv" | [ "Base" | "Base" | "Core" |
    "Data" | "Flat" | "Overture" | "Syntax" ] ] [ 89 | "Rop" | [ "Base" | "Core" | "Data" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 90  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 79 | "Base" | [ ] ] [ 86
    | "Flat" | [ "Lattice" ] ] [ 89 | "Cox" | [ ] ] [ 90 | "Nest" | [ ] ] =}

|-- DIR-RANK  /dir-rank 91  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 62 | "RDF" | [ "Data" ] ] [ 77 | "Element" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax"
    ] ] [ 77 | "Input" | [ "Base" | "Core" | "Data" ] ] [ 80 | "Change" | [ "Base" | "Data" | "Writer" ] ] [ 80 | "Run"
    | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 91 | "Global" | [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" |
    "Nest" ] ] =}

*** 46 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                  /base-rank
  --------- ------------------------------------- --------------------------------------------------------------------------------------------------------------------
  0         "Koshucode.Baala.Overture.Name"       /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "String"             [ ]
                                                  
  1         "Koshucode.Baala.Overture.Text"       /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "Category"           [ ]
                                                  0     "Integer"            [ ]
                                                  0     "Unicode"            [ ]
                                                  1     "Utility"            [ ]
                                                  
  2         "Koshucode.Baala.Overture"            /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "Cache"              [ ]
                                                  0     "Type"               [ ]
                                                  1     "Fn"                 [ ]
                                                  1     "Shorthand"          [ ]
                                                  2     "Text"               [ ]
                                                  
  4         "Koshucode.Baala.Base.Prelude"        /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "Case"               [ ]
                                                  0     "Class"              [ ]
                                                  0     "Import"             [ ]
                                                  1     "Queue"              [ ]
                                                  4     "Pair"               [ "Overture" ]
                                                  
  6         "Koshucode.Baala.Base.MixText"        /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "LineBreak"          [ ]
                                                  4     "MixText"            [ "Overture" ]
                                                  5     "MixClass"           [ ]
                                                  5     "MixEncode"          [ ]
                                                  6     "Deriv"              [ ]
                                                  
  6         "Koshucode.Baala.Subtext.Operator"    /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  3     "Basic"              [ ]
                                                  4     "Combine"            [ ]
                                                  5     "Repeat"             [ ]
                                                  6     "Char"               [ "Overture" ]
                                                  
  7         "Koshucode.Baala.Base.List"           /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "Split"              [ ]
                                                  4     "Assoc"              [ "Overture" ]
                                                  4     "Dots"               [ "Overture" ]
                                                  4     "List"               [ "Overture" ]
                                                  4     "Set"                [ "Overture" ]
                                                  6     "Snip"               [ "Overture" | "Prelude" ]
                                                  7     "Order"              [ "Overture" | "Prelude" ]
                                                  
  9         "Koshucode.Baala.Base.Text"           /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  4     "Comment"            [ "Overture" ]
                                                  4     "TextTable"          [ "Overture" ]
                                                  6     "PPrint"             [ "Overture" | "Prelude" ]
                                                  9     "Suffix"             [ "List" | "Overture" ]
                                                  
  10        "Koshucode.Baala.Subtext"             /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "MinMax"             [ ]
                                                  2     "Expr"               [ ]
                                                  7     "Operator"           [ ]
                                                  8     "Bundle"             [ "Operator" ]
                                                  9     "Para"               [ "Operator" ]
                                                  10    "Match"              [ "Operator" ]
                                                  
  11        "Koshucode.Baala.Base.IO"             /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "SimpleOption"       [ ]
                                                  6     "BzFile"             [ "Prelude" ]
                                                  6     "Exit"               [ "Prelude" ]
                                                  6     "Http"               [ "Overture" | "Prelude" ]
                                                  9     "IOPoint"            [ "List" | "Prelude" ]
                                                  11    "CodePos"            [ "Prelude" | "Text" ]
                                                  
  16        "Koshucode.Baala.Base.Abort"          /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  13    "Reason"             [ "IO" | "Overture" ]
                                                  14    "Report"             [ "IO" | "List" | "Overture" | "Prelude" | "Text" ]
                                                  16    "Message"            [ "Abort" ]
                                                  
  18        "Koshucode.Baala.Base.Code"           /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  13    "Line"               [ "IO" | "Prelude" ]
                                                  14    "Clause"             [ "IO" | "List" | "Prelude" ]
                                                  14    "Scan"               [ "IO" | "Overture" | "Prelude" ]
                                                  16    "Message"            [ "Abort" | "IO" ]
                                                  17    "Tree"               [ "Abort" | "IO" | "List" | "Overture" | "Prelude" ]
                                                  18    "Infix"              [ "Overture" | "Prelude" ]
                                                  
  19        "Koshucode.Baala.Base"                /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  5     "Prelude"            [ ]
                                                  7     "MixText"            [ ]
                                                  8     "List"               [ ]
                                                  10    "Text"               [ ]
                                                  12    "IO"                 [ ]
                                                  15    "Abort"              [ ]
                                                  17    "Message"            [ ]
                                                  19    "Code"               [ ]
                                                  
  22        "Koshucode.Baala.Syntax.Para"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Para"               [ "Base" | "Overture" ]
                                                  22    "Get"                [ "Base" ]
                                                  22    "ParaSpec"           [ "Base" | "Overture" ]
                                                  
  23        "Koshucode.Baala.Syntax.Symbol"       /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  4     "AngleText"          [ "Overture" ]
                                                  4     "Term"               [ "Overture" ]
                                                  21    "Message"            [ "Base" ]
                                                  22    "Next"               [ "Base" | "Overture" ]
                                                  23    "Short"              [ "Base" | "Overture" ]
                                                  
  24        "Koshucode.Baala.Data.Type.Decimal"   /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  4     "Rational"           [ "Overture" ]
                                                  21    "Decimal"            [ "Base" | "Overture" ]
                                                  22    "Fraction"           [ "Overture" ]
                                                  23    "Coder"              [ "Base" | "Overture" ]
                                                  24    "BinaryAb"           [ "Base" ]
                                                  
  26        "Koshucode.Baala.Data.Type.Time"      /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  22    "Date"               [ "Base" | "Overture" ]
                                                  23    "Clock"              [ "Base" | "Overture" ]
                                                  24    "ClockCalc"          [ "Base" | "Overture" ]
                                                  25    "Time"               [ "Base" | "Overture" ]
                                                  26    "TimeCalc"           [ "Base" | "Overture" ]
                                                  
  30        "Koshucode.Baala.Syntax.Token"        /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  25    "Token"              [ "Base" | "Symbol" ]
                                                  26    "Pattern"            [ ]
                                                  26    "Utility"            [ "Base" | "Overture" | "Symbol" ]
                                                  27    "Nipper"             [ "Base" | "Overture" | "Symbol" ]
                                                  28    "Section"            [ "Base" | "Symbol" ]
                                                  29    "Rel"                [ "Base" | "Overture" | "Symbol" ]
                                                  30    "Clause"             [ "Base" | "Overture" | "Symbol" ]
                                                  
  34        "Koshucode.Baala.Syntax.TTree"        /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  32    "Bracket"            [ "Base" | "Token" ]
                                                  32    "Pattern"            [ "Base" | "Token" ]
                                                  33    "TokenTree"          [ "Base" | "Token" ]
                                                  34    "Parse"              [ "Base" | "Overture" | "Token" ]
                                                  
  37        "Koshucode.Baala.Syntax.Attr"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "AttrName"           [ ]
                                                  25    "Message"            [ "Base" | "Para" | "Symbol" ]
                                                  36    "Attr"               [ "Base" | "Overture" | "Para" | "TTree" ]
                                                  36    "Slot"               [ "Base" | "Overture" | "TTree" | "Token" ]
                                                  37    "AttrEd"             [ "Base" | "Symbol" | "TTree" | "Token" ]
                                                  37    "Parse"              [ "Base" | "Overture" | "Para" ]
                                                  
  38        "Koshucode.Baala.Syntax"              /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  23    "Para"               [ ]
                                                  24    "Symbol"             [ ]
                                                  26    "Message"            [ ]
                                                  31    "Token"              [ ]
                                                  35    "TTree"              [ ]
                                                  38    "Attr"               [ ]
                                                  
  42        "Koshucode.Baala.Data.Type.Judge"     /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  40    "JudgeClass"         [ "Base" | "Syntax" ]
                                                  41    "Judge"              [ "Base" | "Overture" | "Syntax" ]
                                                  42    "Interp"             [ "Base" | "Syntax" ]
                                                  
  47        "Koshucode.Baala.Data.Type.Rel"       /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  44    "TermPicker"         [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                  45    "Head"               [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ]
                                                  46    "Rel"                [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                  47    "Mono"               [ "Base" ]
                                                  
  48        "Koshucode.Baala.Data.Type"           /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  25    "Decimal"            [ ]
                                                  27    "Time"               [ ]
                                                  43    "Judge"              [ ]
                                                  44    "Type"               [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                  48    "Rel"                [ ]
                                                  
  53        "Koshucode.Baala.Core.Lexmap"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  40    "Lexmap"             [ "Base" | "Overture" | "Syntax" ]
                                                  52    "LexmapTrees"        [ "Base" | "Syntax" ]
                                                  53    "Construct"          [ "Base" | "Overture" | "Syntax" ]
                                                  
  54        "Koshucode.Baala.Data.Class"          /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  50    "Singleton"          [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                  51    "Simple"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                  52    "Complex"            [ "Base" | "Syntax" | "Type" ]
                                                  53    "Content"            [ "Base" ]
                                                  53    "Map"                [ "Overture" ]
                                                  54    "Encode"             [ "Base" | "Syntax" | "Type" ]
                                                  
  56        "Koshucode.Baala.Data.Decode"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  40    "Message"            [ "Base" | "Syntax" ]
                                                  41    "Term"               [ "Base" | "Overture" | "Syntax" ]
                                                  50    "Type"               [ "Base" | "Syntax" | "Type" ]
                                                  51    "Numeric"            [ "Base" | "Syntax" | "Type" ]
                                                  56    "Content"            [ "Base" | "Class" | "Syntax" | "Type" ]
                                                  
  59        "Koshucode.Baala.Data.Church"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  50    "Message"            [ "Base" | "Syntax" | "Type" ]
                                                  51    "Cox"                [ "Base" | "Overture" | "Syntax" ]
                                                  52    "Cop"                [ "Base" | "Overture" | "Syntax" ]
                                                  58    "Build"              [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ]
                                                  59    "Run"                [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                  
  60        "Koshucode.Baala.Data"                /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  49    "Type"               [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ]
                                                  51    "Message"            [ ]
                                                  55    "Class"              [ ]
                                                  57    "Decode"             [ ]
                                                  58    "Content"            [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                  60    "Church"             [ ]
                                                  
  62        "Koshucode.Baala.Data.Type.Type"      /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  56    "Genus"              [ "Class" | "Overture" ]
                                                  62    "Predefined where"   [ "Class" | "Data" ]
                                                  
  63        "Koshucode.Baala.Core.Relkit"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  40    "Message"            [ "Base" | "Syntax" ]
                                                  62    "Relkit"             [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ]
                                                  63    "Construct"          [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  63    "Run"                [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  
  67        "Koshucode.Baala.Core.Relmap"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  62    "Message"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  62    "Result"             [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  63    "Option"             [ "Base" | "Data" | "Syntax" ]
                                                  65    "Relmap"             [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  66    "Rop"                [ "Base" | "Lexmap" | "Syntax" ]
                                                  66    "Specialize"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  67    "Construct"          [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  67    "Global"             [ "Base" | "Data" | "Overture" ]
                                                  
  70        "Koshucode.Baala.Core.Assert"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  40    "Message"            [ "Base" | "Syntax" ]
                                                  62    "Dataset"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  62    "RelTable"           [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  69    "Assert"             [ "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
                                                  70    "Run"                [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  74        "Koshucode.Baala.Core.Resource"       /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  62    "Clause"             [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ]
                                                  72    "Resource"           [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ]
                                                  73    "Concrete"           [ "Data" | "Relmap" | "Syntax" ]
                                                  73    "Include"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  73    "Run"                [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
                                                  74    "Read"               [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ]
                                                  
  75        "Koshucode.Baala.Core"                /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  54    "Lexmap"             [ ]
                                                  63    "Message"            [ ]
                                                  64    "Relkit"             [ ]
                                                  68    "Relmap"             [ ]
                                                  71    "Assert"             [ ]
                                                  75    "Resource"           [ ]
                                                  
  78        "Koshucode.Baala.Rop.Base"            /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  64    "Message"            [ "Base" ]
                                                  77    "Define"             [ "Core" | "Overture" | "Syntax" ]
                                                  77    "Get"                [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  78    "Rop"                [ "Base" | "Core" ]
                                                  
  78        "Koshucode.Baala.Writer"              /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  77    "Csv"                [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  77    "Html"               [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  77    "Json"               [ "Core" | "Data" | "Syntax" ]
                                                  77    "Judge"              [ "Base" | "Core" | "Data" | "Overture" ]
                                                  78    "Koshu"              [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  
  80        "Koshucode.Baala.Cop"                 /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "Replace"            [ ]
                                                  62    "Coxhand"            [ "Data" | "Overture" | "Syntax" ]
                                                  63    "Logic"              [ "Data" | "Overture" ]
                                                  78    "Message"            [ ]
                                                  79    "Arith"              [ "Base" | "Data" | "Overture" ]
                                                  79    "List"               [ "Base" | "Data" | "Overture" ]
                                                  79    "Misc"               [ "Base" | "Data" | "Syntax" ]
                                                  79    "Order"              [ "Data" | "Overture" ]
                                                  79    "Text"               [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  79    "Time"               [ "Data" | "Overture" ]
                                                  79    "Type"               [ "Base" | "Data" ]
                                                  80    "Bundle"             [ "Base" | "Data" ]
                                                  
  81        "Koshucode.Baala"                     /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  3     "Overture"           [ "Text" ]
                                                  11    "Subtext"            [ "Operator" ]
                                                  20    "Base"               [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ]
                                                  39    "Syntax"             [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                                  61    "Data"               [ "Church" | "Class" | "Decode" | "Type" ]
                                                  62    "DataPlus"           [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  76    "Core"               [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  79    "Writer"             [ ]
                                                  81    "Cop"                [ ]
                                                  
  81        "Koshucode.Baala.Toolkit.Main"        /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  63    "KoshuRdf"           [ "Base" | "Data" | "Syntax" ]
                                                  77    "KoshuSyntax"        [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  81    "KoshuChange"        [ "Base" ]
                                                  81    "KoshuFilter"        [ "Base" | "Core" | "Data" ]
                                                  81    "KoshuMain"          [ "Base" | "Core" | "Data" | "Overture" | "Writer" ]
                                                  
  82        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  80    "Tropashko"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  81    "Restrict"           [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  82    "Rop"                [ "Base" | "Core" ]
                                                  
  85        "Koshucode.Baala.Rop.Flat"            /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "PoScale"            [ "Base" | "Overture" ]
                                                  77    "Message"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  80    "Elem"               [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  80    "Meta"               [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  80    "Order"              [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  80    "Resource"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  80    "Source"             [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  80    "Subtext"            [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Subtext" | "Syntax" ]
                                                  80    "Term"               [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  81    "Gadget"             [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  81    "Peripheral"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  81    "TermGadget"         [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  83    "Lattice"            [ ]
                                                  84    "Check"              [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  84    "Control"            [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Overture" ]
                                                  85    "Bundle"             [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  88        "Koshucode.Baala.Rop.Cox"             /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "GeoDatumJp"         [ ]
                                                  78    "Message"            [ ]
                                                  80    "Get"                [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  81    "Accessor"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  81    "Calc"               [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  81    "Filter"             [ "Base" | "Core" | "Data" ]
                                                  81    "Gadget"             [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  81    "Range"              [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  87    "Empty"              [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" ]
                                                  88    "Bundle"             [ "Core" | "Data" ]
                                                  
  89        "Koshucode.Baala.Rop.Nest"            /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  78    "Message"            [ "Base" | "Data" | "Syntax" ]
                                                  80    "Flow"               [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  87    "Confl"              [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ]
                                                  88    "Deriv"              [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ]
                                                  89    "Rop"                [ "Base" | "Core" | "Data" | "Overture" ]
                                                  
  90        "Koshucode.Baala.Rop"                 /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  79    "Base"               [ ]
                                                  86    "Flat"               [ "Lattice" ]
                                                  89    "Cox"                [ ]
                                                  90    "Nest"               [ ]
                                                  
  91        "Koshucode.Baala.Toolkit.Library"     /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "Version"            [ ]
                                                  62    "RDF"                [ "Data" ]
                                                  77    "Element"            [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  77    "Input"              [ "Base" | "Core" | "Data" ]
                                                  80    "Change"             [ "Base" | "Data" | "Writer" ]
                                                  80    "Run"                [ "Base" | "Core" | "Data" | "Writer" ]
                                                  91    "Global"             [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      46 judges on DIR-RANK
**      46 judges in total
**

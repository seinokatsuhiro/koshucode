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
|-- DIR-RANK  /dir-rank 0  /dir "Koshucode.Baala.System"  /base-rank {= /rank /base /import-dir [ 0 | "CliParser" | [ ]
    ] [ 0 | "Exit" | [ ] ] =}
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
    [ ] ] [ 6 | "BzFile" | [ "Prelude" ] ] [ 6 | "Encoding" | [ "Prelude" ] ] [ 6 | "Http" | [ "Overture" | "Prelude" ]
    ] [ 9 | "IOPoint" | [ "List" | "Prelude" ] ] [ 11 | "CodePos" | [ "Overture" | "Prelude" | "Text" ] ] =}
|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 13 | "Reason" | [
    "IO" | "Overture" ] ] [ 14 | "Report" | [ "IO" | "List" | "Overture" | "Text" ] ] [ 16 | "Message" | [ "Abort" ] ]
    =}
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
|-- DIR-RANK  /dir-rank 29  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 21 | "Message"
    | [ "Base" ] ] [ 25 | "Token" | [ "Base" | "Symbol" ] ] [ 26 | "Nipper" | [ "Base" | "Overture" | "Symbol" ] ] [ 26
    | "Pattern" | [ ] ] [ 26 | "Utility" | [ "Base" | "Overture" | "Symbol" ] ] [ 27 | "Section" | [ "Base" | "Symbol" ]
    ] [ 28 | "Rel" | [ "Base" | "Overture" | "Symbol" ] ] [ 29 | "Clause" | [ "Base" | "Overture" | "Symbol" ] ] =}
|-- DIR-RANK  /dir-rank 33  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 31 | "Bracket"
    | [ "Base" | "Token" ] ] [ 32 | "Pattern" | [ "Base" | "Token" ] ] [ 32 | "TokenTree" | [ "Base" | "Token" ] ] [ 33
    | "Parse" | [ "Base" | "Overture" | "Token" ] ] =}

|-- DIR-RANK  /dir-rank 36  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 25 | "Message" | [ "Base" | "Para" | "Symbol" ] ] [ 35 | "Attr" | [ "Base" | "Overture" | "Para" | "TTree" ]
    ] [ 35 | "Slot" | [ "Base" | "Overture" | "TTree" | "Token" ] ] [ 36 | "AttrEd" | [ "Base" | "Symbol" | "TTree" |
    "Token" ] ] [ 36 | "Parse" | [ "Base" | "Overture" | "Para" ] ] =}
|-- DIR-RANK  /dir-rank 37  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 23 | "Para" | [ ] ] [
    24 | "Symbol" | [ ] ] [ 26 | "Message" | [ ] ] [ 30 | "Token" | [ ] ] [ 33 | "Pattern" | [ ] ] [ 34 | "TTree" | [ ]
    ] [ 37 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 41  /dir "Koshucode.Baala.Data.Type.Judge"  /base-rank {= /rank /base /import-dir [ 39 |
    "JudgeClass" | [ "Base" | "Syntax" ] ] [ 40 | "Judge" | [ "Base" | "Overture" | "Syntax" ] ] [ 41 | "Interp" | [
    "Base" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 46  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 43 |
    "TermPicker" | [ "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 44 | "Head" | [ "Base" | "Judge" | "Overture" |
    "Syntax" | "Type" ] ] [ 45 | "Rel" | [ "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 46 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 47  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 21 | "Message" | [
    "Base" ] ] [ 25 | "Decimal" | [ ] ] [ 27 | "Time" | [ ] ] [ 42 | "Judge" | [ ] ] [ 43 | "Type" | [ "Base" | "Judge"
    | "Overture" | "Syntax" ] ] [ 47 | "Rel" | [ ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 52  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 21 | "Message" |
    [ "Base" ] ] [ 39 | "Lexmap" | [ "Base" | "Overture" | "Syntax" ] ] [ 51 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [
    52 | "Construct" | [ "Base" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 53  /dir "Koshucode.Baala.Data.Class"  /base-rank {= /rank /base /import-dir [ 21 | "Message" |
    [ "Base" ] ] [ 49 | "Singleton" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 50 | "Simple" | [ "Base" |
    "Overture" | "Syntax" | "Type" ] ] [ 51 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 52 | "Content" | [ "Base" ]
    ] [ 52 | "Map" | [ "Overture" ] ] [ 53 | "Encode" | [ "Base" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 55  /dir "Koshucode.Baala.Data.Decode"  /base-rank {= /rank /base /import-dir [ 39 | "Message" |
    [ "Base" | "Syntax" ] ] [ 40 | "Term" | [ "Base" | "Overture" | "Syntax" ] ] [ 49 | "Type" | [ "Base" | "Syntax" |
    "Type" ] ] [ 50 | "Numeric" | [ "Base" | "Syntax" | "Type" ] ] [ 55 | "Content" | [ "Base" | "Class" | "Syntax" |
    "Type" ] ] =}
|-- DIR-RANK  /dir-rank 58  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 49 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 50 | "Cox" | [ "Base" | "Overture" | "Syntax" ] ] [ 51 | "Cop" | [ "Base" |
    "Overture" | "Syntax" ] ] [ 57 | "Build" | [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ] ] [ 58 | "Run" |
    [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 48 | "Type" | [
    "Decimal" | "Judge" | "Rel" | "Time" | "Type" ] ] [ 50 | "Message" | [ ] ] [ 54 | "Class" | [ ] ] [ 56 | "Decode" |
    [ ] ] [ 57 | "Content" | [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ] ] [ 59 | "Church" | [ ] ]
    =}

|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Data.Type.Type"  /base-rank {= /rank /base /import-dir [ 55 | "Genus"
    | [ "Class" | "Overture" ] ] [ 61 | "Predefined where" | [ "Class" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 62  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 39 | "Message" |
    [ "Base" | "Syntax" ] ] [ 61 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ] ] [ 62 |
    "Construct" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 62 | "Run" | [ "Base" | "Data" | "Overture" | "Syntax"
    ] ] =}
|-- DIR-RANK  /dir-rank 66  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 61 | "Message" |
    [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 61 | "Result" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 62 |
    "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 64 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" ] ] [ 65 | "Rop" | [ "Base" | "Lexmap" | "Syntax" ] ] [ 65 | "Specialize" | [ "Base" | "Data" | "Lexmap" |
    "Overture" | "Relkit" | "Syntax" ] ] [ 66 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" ] ] [ 66 | "Global" | [ "Base" | "Data" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 39 | "Message" |
    [ "Base" | "Syntax" ] ] [ 61 | "Dataset" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 61 | "RelTable" | [
    "Base" | "Data" | "Overture" | "Syntax" ] ] [ 68 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" |
    "Syntax" ] ] [ 69 | "Run" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 73  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 21 | "Message"
    | [ "Base" ] ] [ 61 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ] ] [ 71 | "Resource" | [
    "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ] ] [ 72 | "Concrete" | [ "Data"
    | "Relmap" | "Syntax" ] ] [ 72 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 72
    | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ] ] [ 73 | "Read" | [ "Base" |
    "Data" | "Overture" | "Relmap" | "Syntax" ] ] =}

|-- DIR-RANK  /dir-rank 74  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 53 | "Lexmap" | [ ] ] [
    62 | "Message" | [ ] ] [ 63 | "Relkit" | [ ] ] [ 67 | "Relmap" | [ ] ] [ 70 | "Assert" | [ ] ] [ 74 | "Resource" | [
    ] ] =}
|-- DIR-RANK  /dir-rank 77  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 63 | "Message" | [
    "Base" ] ] [ 76 | "Define" | [ "Core" | "Overture" | "Syntax" ] ] [ 76 | "Get" | [ "Base" | "Core" | "Data" |
    "Overture" | "Syntax" ] ] [ 77 | "Rop" | [ "Core" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 77  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 76 | "Csv" | [ "Base"
    | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 76 | "Html" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ]
    [ 76 | "Json" | [ "Core" | "Data" | "Syntax" ] ] [ 76 | "Judge" | [ "Base" | "Core" | "Data" | "Overture" ] ] [ 77 |
    "Koshu" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    61 | "Coxhand" | [ "Data" | "Overture" | "Syntax" ] ] [ 62 | "Logic" | [ "Data" | "Overture" ] ] [ 77 | "Message" |
    [ ] ] [ 78 | "Arith" | [ "Base" | "Data" | "Overture" ] ] [ 78 | "List" | [ "Base" | "Data" | "Overture" ] ] [ 78 |
    "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 78 | "Order" | [ "Data" | "Overture" ] ] [ 78 | "Text" | [ "Base" |
    "Data" | "Overture" | "Syntax" ] ] [ 78 | "Time" | [ "Data" | "Overture" ] ] [ 78 | "Type" | [ "Base" | "Data" ] ] [
    79 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 1 | "System" | [ ] ] [ 3 |
    "Overture" | [ "Text" ] ] [ 11 | "Subtext" | [ "Operator" ] ] [ 20 | "Base" | [ "Abort" | "Code" | "IO" | "List" |
    "MixText" | "Prelude" | "Text" ] ] [ 38 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ] ] [ 60 |
    "Data" | [ "Church" | "Class" | "Decode" | "Type" ] ] [ 61 | "DataPlus" | [ "Base" | "Data" | "Overture" | "Syntax"
    ] ] [ 75 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 78 | "Writer" | [ ] ] [ 80 |
    "Cop" | [ ] ] =}

|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 62 | "KoshuRdf"
    | [ "Base" | "Data" | "Syntax" ] ] [ 76 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [
    80 | "KoshuChange" | [ "Base" ] ] [ 80 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 80 | "KoshuMain" | [
    "Base" | "Core" | "Data" | "Overture" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 79 |
    "Tropashko" | [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 80 | "Restrict" | [ "Base" | "Core"
    | "Data" | "Overture" | "Syntax" ] ] [ 81 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 84  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 21 | "PoScale" | [
    "Base" | "Overture" ] ] [ 76 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 79 | "Elem" | [ "Base" |
    "Base" | "Core" | "Data" | "Syntax" ] ] [ 79 | "Meta" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 79 |
    "Order" | [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 79 | "Resource" | [ "Base" | "Base" |
    "Core" | "Data" | "Syntax" ] ] [ 79 | "Source" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 79 |
    "Subtext" | [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Subtext" | "Syntax" ] ] [ 79 | "Term" | [ "Base" |
    "Base" | "Core" | "Data" | "Syntax" ] ] [ 80 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Overture" |
    "Syntax" ] ] [ 80 | "Peripheral" | [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ] ] [ 80 |
    "TermGadget" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 82 | "Lattice" | [ ] ] [ 83 | "Check" | [ "Base" | "Base"
    | "Core" | "Data" | "Lattice" | "Overture" | "Syntax" ] ] [ 83 | "Control" | [ "Base" | "Base" | "Core" | "Data" |
    "Lattice" | "Overture" ] ] [ 84 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 87  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 77 | "Message" | [ ] ] [ 79 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 80 | "Accessor" | [
    "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 80 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Overture" |
    "Syntax" ] ] [ 80 | "Filter" | [ "Base" | "Core" | "Data" ] ] [ 80 | "Gadget" | [ "Base" | "Base" | "Core" | "Data"
    | "Overture" | "Syntax" ] ] [ 80 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 86 | "Empty" | [
    "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" ] ] [ 87 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 88  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 77 | "Message" | [
    "Base" | "Data" | "Syntax" ] ] [ 79 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 86 | "Confl" | [
    "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ] ] [ 87 | "Deriv" | [ "Base" | "Core" | "Data" |
    "Flat" | "Overture" | "Syntax" ] ] [ 88 | "Rop" | [ "Base" | "Core" | "Data" | "Overture" ] ] =}

|-- DIR-RANK  /dir-rank 89  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 78 | "Base" | [ ] ] [ 85
    | "Flat" | [ "Lattice" ] ] [ 88 | "Cox" | [ ] ] [ 89 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 90  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 61 | "RDF" | [ "Data" ] ] [ 76 | "Element" | [ "Base" | "Core" | "Data" | "Overture" | "Syntax"
    ] ] [ 76 | "Input" | [ "Base" | "Core" | "Data" ] ] [ 79 | "Change" | [ "Base" | "Data" | "Writer" ] ] [ 79 | "Run"
    | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 90 | "Global" | [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" |
    "Nest" ] ] =}

*** 47 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                  /base-rank
  --------- ------------------------------------- --------------------------------------------------------------------------------------------------------------------
  0         "Koshucode.Baala.Overture.Name"       /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "String"             [ ]
                                                  
  0         "Koshucode.Baala.System"              /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "CliParser"          [ ]
                                                  0     "Exit"               [ ]
                                                  
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
                                                  6     "Encoding"           [ "Prelude" ]
                                                  6     "Http"               [ "Overture" | "Prelude" ]
                                                  9     "IOPoint"            [ "List" | "Prelude" ]
                                                  11    "CodePos"            [ "Overture" | "Prelude" | "Text" ]
                                                  
  16        "Koshucode.Baala.Base.Abort"          /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  13    "Reason"             [ "IO" | "Overture" ]
                                                  14    "Report"             [ "IO" | "List" | "Overture" | "Text" ]
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
                                                  
  29        "Koshucode.Baala.Syntax.Token"        /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  25    "Token"              [ "Base" | "Symbol" ]
                                                  26    "Nipper"             [ "Base" | "Overture" | "Symbol" ]
                                                  26    "Pattern"            [ ]
                                                  26    "Utility"            [ "Base" | "Overture" | "Symbol" ]
                                                  27    "Section"            [ "Base" | "Symbol" ]
                                                  28    "Rel"                [ "Base" | "Overture" | "Symbol" ]
                                                  29    "Clause"             [ "Base" | "Overture" | "Symbol" ]
                                                  
  33        "Koshucode.Baala.Syntax.TTree"        /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  31    "Bracket"            [ "Base" | "Token" ]
                                                  32    "Pattern"            [ "Base" | "Token" ]
                                                  32    "TokenTree"          [ "Base" | "Token" ]
                                                  33    "Parse"              [ "Base" | "Overture" | "Token" ]
                                                  
  36        "Koshucode.Baala.Syntax.Attr"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "AttrName"           [ ]
                                                  25    "Message"            [ "Base" | "Para" | "Symbol" ]
                                                  35    "Attr"               [ "Base" | "Overture" | "Para" | "TTree" ]
                                                  35    "Slot"               [ "Base" | "Overture" | "TTree" | "Token" ]
                                                  36    "AttrEd"             [ "Base" | "Symbol" | "TTree" | "Token" ]
                                                  36    "Parse"              [ "Base" | "Overture" | "Para" ]
                                                  
  37        "Koshucode.Baala.Syntax"              /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  23    "Para"               [ ]
                                                  24    "Symbol"             [ ]
                                                  26    "Message"            [ ]
                                                  30    "Token"              [ ]
                                                  33    "Pattern"            [ ]
                                                  34    "TTree"              [ ]
                                                  37    "Attr"               [ ]
                                                  
  41        "Koshucode.Baala.Data.Type.Judge"     /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  39    "JudgeClass"         [ "Base" | "Syntax" ]
                                                  40    "Judge"              [ "Base" | "Overture" | "Syntax" ]
                                                  41    "Interp"             [ "Base" | "Syntax" ]
                                                  
  46        "Koshucode.Baala.Data.Type.Rel"       /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  43    "TermPicker"         [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                  44    "Head"               [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ]
                                                  45    "Rel"                [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                  46    "Mono"               [ "Base" ]
                                                  
  47        "Koshucode.Baala.Data.Type"           /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  25    "Decimal"            [ ]
                                                  27    "Time"               [ ]
                                                  42    "Judge"              [ ]
                                                  43    "Type"               [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                  47    "Rel"                [ ]
                                                  
  52        "Koshucode.Baala.Core.Lexmap"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  39    "Lexmap"             [ "Base" | "Overture" | "Syntax" ]
                                                  51    "LexmapTrees"        [ "Base" | "Syntax" ]
                                                  52    "Construct"          [ "Base" | "Overture" | "Syntax" ]
                                                  
  53        "Koshucode.Baala.Data.Class"          /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  49    "Singleton"          [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                  50    "Simple"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                  51    "Complex"            [ "Base" | "Syntax" | "Type" ]
                                                  52    "Content"            [ "Base" ]
                                                  52    "Map"                [ "Overture" ]
                                                  53    "Encode"             [ "Base" | "Syntax" | "Type" ]
                                                  
  55        "Koshucode.Baala.Data.Decode"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  39    "Message"            [ "Base" | "Syntax" ]
                                                  40    "Term"               [ "Base" | "Overture" | "Syntax" ]
                                                  49    "Type"               [ "Base" | "Syntax" | "Type" ]
                                                  50    "Numeric"            [ "Base" | "Syntax" | "Type" ]
                                                  55    "Content"            [ "Base" | "Class" | "Syntax" | "Type" ]
                                                  
  58        "Koshucode.Baala.Data.Church"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  49    "Message"            [ "Base" | "Syntax" | "Type" ]
                                                  50    "Cox"                [ "Base" | "Overture" | "Syntax" ]
                                                  51    "Cop"                [ "Base" | "Overture" | "Syntax" ]
                                                  57    "Build"              [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ]
                                                  58    "Run"                [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                  
  59        "Koshucode.Baala.Data"                /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  48    "Type"               [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ]
                                                  50    "Message"            [ ]
                                                  54    "Class"              [ ]
                                                  56    "Decode"             [ ]
                                                  57    "Content"            [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                  59    "Church"             [ ]
                                                  
  61        "Koshucode.Baala.Data.Type.Type"      /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  55    "Genus"              [ "Class" | "Overture" ]
                                                  61    "Predefined where"   [ "Class" | "Data" ]
                                                  
  62        "Koshucode.Baala.Core.Relkit"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  39    "Message"            [ "Base" | "Syntax" ]
                                                  61    "Relkit"             [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ]
                                                  62    "Construct"          [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  62    "Run"                [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  
  66        "Koshucode.Baala.Core.Relmap"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  61    "Message"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  61    "Result"             [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  62    "Option"             [ "Base" | "Data" | "Syntax" ]
                                                  64    "Relmap"             [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  65    "Rop"                [ "Base" | "Lexmap" | "Syntax" ]
                                                  65    "Specialize"         [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  66    "Construct"          [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Syntax" ]
                                                  66    "Global"             [ "Base" | "Data" | "Overture" ]
                                                  
  69        "Koshucode.Baala.Core.Assert"         /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  39    "Message"            [ "Base" | "Syntax" ]
                                                  61    "Dataset"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  61    "RelTable"           [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  68    "Assert"             [ "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
                                                  69    "Run"                [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  73        "Koshucode.Baala.Core.Resource"       /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "Message"            [ "Base" ]
                                                  61    "Clause"             [ "Base" | "Data" | "Lexmap" | "Overture" | "Syntax" ]
                                                  71    "Resource"           [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" ]
                                                  72    "Concrete"           [ "Data" | "Relmap" | "Syntax" ]
                                                  72    "Include"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  72    "Run"                [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" ]
                                                  73    "Read"               [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ]
                                                  
  74        "Koshucode.Baala.Core"                /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  53    "Lexmap"             [ ]
                                                  62    "Message"            [ ]
                                                  63    "Relkit"             [ ]
                                                  67    "Relmap"             [ ]
                                                  70    "Assert"             [ ]
                                                  74    "Resource"           [ ]
                                                  
  77        "Koshucode.Baala.Rop.Base"            /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  63    "Message"            [ "Base" ]
                                                  76    "Define"             [ "Core" | "Overture" | "Syntax" ]
                                                  76    "Get"                [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  77    "Rop"                [ "Core" | "Overture" ]
                                                  
  77        "Koshucode.Baala.Writer"              /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  76    "Csv"                [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  76    "Html"               [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  76    "Json"               [ "Core" | "Data" | "Syntax" ]
                                                  76    "Judge"              [ "Base" | "Core" | "Data" | "Overture" ]
                                                  77    "Koshu"              [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  
  79        "Koshucode.Baala.Cop"                 /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "Replace"            [ ]
                                                  61    "Coxhand"            [ "Data" | "Overture" | "Syntax" ]
                                                  62    "Logic"              [ "Data" | "Overture" ]
                                                  77    "Message"            [ ]
                                                  78    "Arith"              [ "Base" | "Data" | "Overture" ]
                                                  78    "List"               [ "Base" | "Data" | "Overture" ]
                                                  78    "Misc"               [ "Base" | "Data" | "Syntax" ]
                                                  78    "Order"              [ "Data" | "Overture" ]
                                                  78    "Text"               [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  78    "Time"               [ "Data" | "Overture" ]
                                                  78    "Type"               [ "Base" | "Data" ]
                                                  79    "Bundle"             [ "Base" | "Data" ]
                                                  
  80        "Koshucode.Baala"                     /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  1     "System"             [ ]
                                                  3     "Overture"           [ "Text" ]
                                                  11    "Subtext"            [ "Operator" ]
                                                  20    "Base"               [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ]
                                                  38    "Syntax"             [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                                  60    "Data"               [ "Church" | "Class" | "Decode" | "Type" ]
                                                  61    "DataPlus"           [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                  75    "Core"               [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  78    "Writer"             [ ]
                                                  80    "Cop"                [ ]
                                                  
  80        "Koshucode.Baala.Toolkit.Main"        /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  62    "KoshuRdf"           [ "Base" | "Data" | "Syntax" ]
                                                  76    "KoshuSyntax"        [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  80    "KoshuChange"        [ "Base" ]
                                                  80    "KoshuFilter"        [ "Base" | "Core" | "Data" ]
                                                  80    "KoshuMain"          [ "Base" | "Core" | "Data" | "Overture" | "Writer" ]
                                                  
  81        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  79    "Tropashko"          [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  80    "Restrict"           [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  81    "Rop"                [ "Base" | "Core" ]
                                                  
  84        "Koshucode.Baala.Rop.Flat"            /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  21    "PoScale"            [ "Base" | "Overture" ]
                                                  76    "Message"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Elem"               [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Meta"               [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Order"              [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  79    "Resource"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  79    "Source"             [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  79    "Subtext"            [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Subtext" | "Syntax" ]
                                                  79    "Term"               [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  80    "Gadget"             [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  80    "Peripheral"         [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  80    "TermGadget"         [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  82    "Lattice"            [ ]
                                                  83    "Check"              [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Overture" | "Syntax" ]
                                                  83    "Control"            [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Overture" ]
                                                  84    "Bundle"             [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  87        "Koshucode.Baala.Rop.Cox"             /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "GeoDatumJp"         [ ]
                                                  77    "Message"            [ ]
                                                  79    "Get"                [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  80    "Accessor"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  80    "Calc"               [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  80    "Filter"             [ "Base" | "Core" | "Data" ]
                                                  80    "Gadget"             [ "Base" | "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  80    "Range"              [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  86    "Empty"              [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" ]
                                                  87    "Bundle"             [ "Core" | "Data" ]
                                                  
  88        "Koshucode.Baala.Rop.Nest"            /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  77    "Message"            [ "Base" | "Data" | "Syntax" ]
                                                  79    "Flow"               [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  86    "Confl"              [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ]
                                                  87    "Deriv"              [ "Base" | "Core" | "Data" | "Flat" | "Overture" | "Syntax" ]
                                                  88    "Rop"                [ "Base" | "Core" | "Data" | "Overture" ]
                                                  
  89        "Koshucode.Baala.Rop"                 /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  78    "Base"               [ ]
                                                  85    "Flat"               [ "Lattice" ]
                                                  88    "Cox"                [ ]
                                                  89    "Nest"               [ ]
                                                  
  90        "Koshucode.Baala.Toolkit.Library"     /rank /base                /import-dir
                                                  ----- -------------------- -----------------------------------------------------------------------------------------
                                                  0     "Version"            [ ]
                                                  61    "RDF"                [ "Data" ]
                                                  76    "Element"            [ "Base" | "Core" | "Data" | "Overture" | "Syntax" ]
                                                  76    "Input"              [ "Base" | "Core" | "Data" ]
                                                  79    "Change"             [ "Base" | "Data" | "Writer" ]
                                                  79    "Run"                [ "Base" | "Core" | "Data" | "Writer" ]
                                                  90    "Global"             [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      47 judges on DIR-RANK
**      47 judges in total
**

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
**    ../rop/data/IMPORT.k
**    ../subtext/data/IMPORT.k
**    ../syntax/data/IMPORT.k
**    ../toolkit/data/IMPORT.k
**    ../type/data/IMPORT.k
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
    [ 0 | "Index" | [ ] ] [ 0 | "Type" | [ ] ] [ 0 | "Value" | [ ] ] [ 1 | "Fn" | [ ] ] [ 1 | "Infix" | [ ] ] [ 1 |
    "Misc" | [ ] ] [ 1 | "Shorthand" | [ ] ] [ 2 | "Text" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Case" | [ ]
    ] [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Queue" | [ ] ] [ 4 | "Pair" | [ "Overture" ] ] =}

|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Split" | [ ] ]
    [ 4 | "Assoc" | [ "Overture" ] ] [ 4 | "List" | [ "Overture" ] ] [ 4 | "Select" | [ "Overture" ] ] [ 4 | "Set" | [
    "Overture" ] ] [ 5 | "Picker" | [ "Overture" ] ] [ 6 | "Order" | [ "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Base.MixText"  /base-rank {= /rank /base /import-dir [ 0 | "LineBreak"
    | [ ] ] [ 4 | "MixText" | [ "Overture" ] ] [ 5 | "MixClass" | [ ] ] [ 5 | "MixEncode" | [ "Overture" ] ] [ 6 |
    "Deriv" | [ ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Subtext.Operator"  /base-rank {= /rank /base /import-dir [ 3 | "Basic"
    | [ ] ] [ 4 | "Combine" | [ ] ] [ 5 | "Repeat" | [ ] ] [ 6 | "Char" | [ "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 8  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 4 | "Comment" | [
    "Overture" ] ] [ 4 | "Suffix" | [ "Overture" ] ] [ 4 | "TextTable" | [ "Overture" ] ] [ 6 | "PPrint" | [ "Overture"
    | "Prelude" ] ] [ 8 | "Dots" | [ "List" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Subtext"  /base-rank {= /rank /base /import-dir [ 0 | "MinMax" | [ ] ]
    [ 2 | "Expr" | [ ] ] [ 7 | "Operator" | [ ] ] [ 8 | "Bundle" | [ "Operator" ] ] [ 9 | "Para" | [ "Operator" ] ] [ 10
    | "Match" | [ "Operator" ] ] =}

|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 10 | "CodePos" |
    [ "Overture" | "Prelude" | "Text" ] ] [ 11 | "Reason" | [ "List" | "Overture" ] ] [ 12 | "Report" | [ "List" |
    "Overture" | "Text" ] ] [ 14 | "Message" | [ "Abort" ] ] =}
|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 6 | "Encoding" | [
    "Prelude" ] ] [ 6 | "Http" | [ "Overture" | "Prelude" ] ] [ 8 | "IOPoint" | [ "List" | "Overture" | "Prelude" ] ] [
    14 | "BzFile" | [ "Abort" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Base.Code"  /base-rank {= /rank /base /import-dir [ 14 | "Message" | [
    "Abort" ] ] [ 15 | "Bracket" | [ "Abort" | "List" | "Overture" ] ] [ 16 | "Line" | [ "Abort" | "IO" | "Prelude" ] ]
    [ 16 | "Tree" | [ "Abort" | "Overture" | "Prelude" ] ] [ 17 | "Clause" | [ "Abort" | "List" | "Prelude" ] ] [ 17 |
    "Infix" | [ "Overture" | "Prelude" ] ] [ 17 | "Scan" | [ "Abort" | "IO" | "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 5 | "Prelude" | [ ] ] [
    7 | "List" | [ ] ] [ 7 | "MixText" | [ ] ] [ 9 | "Text" | [ ] ] [ 13 | "Abort" | [ ] ] [ 15 | "IO" | [ ] ] [ 15 |
    "Message" | [ ] ] [ 18 | "Code" | [ ] ] =}
|-- DIR-RANK  /dir-rank 22  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 20 | "Para" | [
    "Base" | "Overture" ] ] [ 21 | "Get" | [ "Base" ] ] [ 21 | "ParaSpec" | [ "Base" | "Overture" ] ] [ 22 | "Parse" | [
    "Base" | "Overture" ] ] =}

|-- DIR-RANK  /dir-rank 23  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 4 |
    "AngleText" | [ "Overture" ] ] [ 4 | "Term" | [ "Overture" ] ] [ 20 | "Message" | [ "Base" ] ] [ 21 | "Next" | [
    "Base" | "Overture" ] ] [ 22 | "Symbol" | [ "Base" | "Overture" ] ] [ 23 | "Short" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 23  /dir "Koshucode.Baala.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 4 | "Rational"
    | [ "Overture" ] ] [ 20 | "Decimal" | [ "Base" | "Overture" ] ] [ 21 | "Fraction" | [ "Overture" ] ] [ 22 | "Coder"
    | [ "Base" | "Overture" ] ] [ 23 | "BinaryAb" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 26  /dir "Koshucode.Baala.Type.Time"  /base-rank {= /rank /base /import-dir [ 21 | "Parts" | [
    "Base" ] ] [ 22 | "Date" | [ "Base" | "Overture" ] ] [ 23 | "Clock" | [ "Base" | "Overture" ] ] [ 24 | "ClockCalc" |
    [ "Base" | "Overture" ] ] [ 25 | "Time" | [ "Base" | "Overture" ] ] [ 26 | "TimeCalc" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 29  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 20 | "Message"
    | [ "Base" ] ] [ 25 | "Token" | [ "Base" | "Symbol" ] ] [ 26 | "Clip" | [ "Base" | "Overture" | "Symbol" ] ] [ 26 |
    "Pattern" | [ ] ] [ 26 | "Utility" | [ "Base" | "Overture" | "Symbol" ] ] [ 27 | "Section" | [ "Base" | "Symbol" ] ]
    [ 28 | "Rel" | [ "Base" | "Overture" | "Symbol" ] ] [ 29 | "Clause" | [ "Base" | "Overture" | "Symbol" ] ] =}
|-- DIR-RANK  /dir-rank 33  /dir "Koshucode.Baala.Syntax.Tree"  /base-rank {= /rank /base /import-dir [ 31 | "Bracket" |
    [ "Base" | "Token" ] ] [ 32 | "Parse" | [ "Base" | "Overture" | "Token" ] ] [ 32 | "Pattern" | [ "Base" | "Token" ]
    ] [ 33 | "Split" | [ "Base" | "Overture" | "Token" ] ] =}

|-- DIR-RANK  /dir-rank 35  /dir "Koshucode.Baala.Syntax.Subtree"  /base-rank {= /rank /base /import-dir [ 4 | "Filter"
    | [ "Overture" ] ] [ 25 | "Subtree" | [ "Base" | "Overture" | "Symbol" ] ] [ 26 | "DirTree" | [ "Base" | "Overture"
    ] ] [ 26 | "Xml" | [ "Base" | "Overture" | "Symbol" ] ] [ 35 | "Decode" | [ "Base" | "Overture" | "Symbol" | "Token"
    | "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 36  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 25 | "Message" | [ "Base" | "Para" | "Symbol" ] ] [ 35 | "Attr" | [ "Base" | "Overture" | "Para" | "Tree" ]
    ] [ 35 | "Slot" | [ "Base" | "Overture" | "Token" | "Tree" ] ] [ 36 | "AttrEd" | [ "Base" | "Symbol" | "Token" |
    "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 37  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 23 | "Para" | [ ] ] [
    24 | "Symbol" | [ ] ] [ 26 | "Message" | [ ] ] [ 30 | "Token" | [ ] ] [ 33 | "Pattern" | [ ] ] [ 34 | "Tree" | [ ] ]
    [ 36 | "Subtree" | [ ] ] [ 37 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 41  /dir "Koshucode.Baala.Type.Judge"  /base-rank {= /rank /base /import-dir [ 39 | "JudgeClass"
    | [ "Base" | "Syntax" ] ] [ 40 | "Judge" | [ "Base" | "Overture" | "Syntax" ] ] [ 41 | "Interp" | [ "Base" |
    "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 46  /dir "Koshucode.Baala.Type.Rel"  /base-rank {= /rank /base /import-dir [ 43 | "Order" | [
    "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 43 | "TermPicker" | [ "Base" | "Judge" | "Overture" | "Syntax" ] ] [
    44 | "Head" | [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ] ] [ 45 | "Rel" | [ "Base" | "Judge" | "Overture"
    | "Syntax" ] ] [ 46 | "Mono" | [ "Base" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 47  /dir "Koshucode.Baala.Type"  /base-rank {= /rank /base /import-dir [ 20 | "Message" | [
    "Base" | "Overture" ] ] [ 24 | "Decimal" | [ ] ] [ 27 | "Time" | [ ] ] [ 42 | "Judge" | [ ] ] [ 43 | "Type" | [
    "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 47 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 52  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 20 | "Message" |
    [ "Base" ] ] [ 39 | "Lexmap" | [ "Base" | "Overture" | "Syntax" ] ] [ 51 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [
    52 | "Construct" | [ "Base" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 53  /dir "Koshucode.Baala.Data.Class"  /base-rank {= /rank /base /import-dir [ 21 | "Message" |
    [ "Base" ] ] [ 49 | "Edge" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 50 | "Simple" | [ "Base" | "Overture" |
    "Syntax" | "Type" ] ] [ 51 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 52 | "Content" | [ "Base" | "Overture" |
    "Type" ] ] [ 52 | "Map" | [ "Overture" ] ] [ 53 | "Encode" | [ "Base" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 56  /dir "Koshucode.Baala.Data.Decode"  /base-rank {= /rank /base /import-dir [ 39 | "Message" |
    [ "Base" | "Syntax" ] ] [ 40 | "Term" | [ "Base" | "Overture" | "Syntax" ] ] [ 49 | "Type" | [ "Base" | "Overture" |
    "Syntax" | "Type" ] ] [ 50 | "Numeric" | [ "Base" | "Syntax" | "Type" ] ] [ 55 | "Content" | [ "Base" | "Class" |
    "Overture" | "Syntax" | "Type" ] ] [ 56 | "Dataset" | [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 49 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 50 | "Cox" | [ "Base" | "Overture" | "Syntax" ] ] [ 51 | "Cop" | [ "Base" |
    "Overture" | "Syntax" ] ] [ 58 | "Build" | [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ] ] [ 59 | "Run" |
    [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 60  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 50 | "Message" | [ ] ]
    [ 54 | "Class" | [ ] ] [ 57 | "Decode" | [ ] ] [ 58 | "Content" | [ "Base" | "Class" | "Decode" | "Overture" |
    "Syntax" | "Type" ] ] [ 60 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 62  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 39 | "Message" |
    [ "Base" | "Syntax" ] ] [ 54 | "Relkit" | [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ] ] [ 55 |
    "Construct" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 62 | "Run" | [ "Base" | "Data" | "Overture" | "Syntax"
    | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 62  /dir "Koshucode.Baala.Type.Type"  /base-rank {= /rank /base /import-dir [ 55 | "Genus" | [
    "Class" | "Overture" ] ] [ 62 | "Predefined where" | [ "Class" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 66  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 49 | "Message" |
    [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 49 | "Result" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 62 |
    "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 64 | "Relmap" | [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax"
    | "Type" ] ] [ 65 | "Rop" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 65 | "Specialize" | [ "Base" | "Lexmap" |
    "Overture" | "Relkit" | "Syntax" | "Type" ] ] [ 66 | "Construct" | [ "Base" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" | "Type" ] ] [ 66 | "Global" | [ "Base" | "Data" | "Overture" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 39 | "Message" |
    [ "Base" | "Syntax" ] ] [ 62 | "RelTable" | [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 68 | "Assert" |
    [ "Base" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ] ] [ 69 | "Run" | [ "Base" | "Data" | "Lexmap" |
    "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 73  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 20 | "Message"
    | [ "Base" ] ] [ 54 | "Clause" | [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ] ] [ 71 | "Resource" | [
    "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ] ] [ 72 | "Concrete" |
    [ "Base" | "Data" | "Relmap" | "Syntax" | "Type" ] ] [ 72 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" |
    "Relmap" | "Syntax" | "Type" ] ] [ 72 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" |
    "Syntax" | "Type" ] ] [ 73 | "Read" | [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 74  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 51 | "Message" | [ ] ]
    [ 53 | "Lexmap" | [ ] ] [ 63 | "Relkit" | [ ] ] [ 67 | "Relmap" | [ ] ] [ 70 | "Assert" | [ ] ] [ 74 | "Resource" |
    [ ] ] =}
|-- DIR-RANK  /dir-rank 77  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 76 | "Csv" | [ "Base"
    | "Core" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 76 | "Html" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 76 | "Json" | [ "Core" | "Data" | "Syntax" | "Type" ] ] [ 76 | "Judge" | [ "Base" | "Core" |
    "Overture" | "Syntax" | "Type" ] ] [ 77 | "Koshu" | [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    62 | "Coxhand" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 63 | "Logic" | [ "Data" | "Overture" ] ] [ 77 |
    "Arith" | [ "Base" | "Data" | "Overture" | "Type" ] ] [ 77 | "List" | [ "Base" | "Data" | "Overture" | "Type" ] ] [
    77 | "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 77 | "Order" | [ "Data" | "Overture" ] ] [ 77 | "Text" | [ "Base" |
    "Data" | "Overture" | "Syntax" | "Type" ] ] [ 77 | "Time" | [ "Data" | "Overture" | "Type" ] ] [ 77 | "Type" | [
    "Data" | "Overture" ] ] [ 78 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 76 | "Message" | [
    "Core" ] ] [ 77 | "Define" | [ "Core" ] ] [ 78 | "Rops" | [ "Core" ] ] =}

|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 1 | "System" | [ ] ] [ 3 |
    "Overture" | [ "Text" ] ] [ 11 | "Subtext" | [ "Operator" ] ] [ 19 | "Base" | [ "Abort" | "Code" | "IO" | "List" |
    "MixText" | "Prelude" | "Text" ] ] [ 38 | "Syntax" | [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" | "Tree" ] ]
    [ 48 | "Type" | [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ] ] [ 61 | "Data" | [ "Church" | "Class" | "Decode"
    | "Type" ] ] [ 62 | "DataPlus" | [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 75 | "Core" | [ "Assert" |
    "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 78 | "Writer" | [ ] ] [ 79 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Rop.Base.Get"  /base-rank {= /rank /base /import-dir [ 77 | "Get" | [
    "Core" ] ] [ 78 | "Rel" | [ "Core" ] ] [ 79 | "Cox" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 63 | "KoshuRdf"
    | [ "Base" | "Data" | "Syntax" | "Type" ] ] [ 76 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 80 | "KoshuChange" | [ "Base" ] ] [ 80 | "KoshuFilter" | [ "Base" | "Core" | "Data" | "Type"
    ] ] [ 80 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Overture" | "Type" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala.Rop.Cox.Type"  /base-rank {= /rank /base /import-dir [ 81 | "Clock" |
    [ "Base" | "Core" ] ] [ 81 | "Dec" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 83  /dir "Koshucode.Baala.Rop.Flat.Applied"  /base-rank {= /rank /base /import-dir [ 63 |
    "PoScale" | [ ] ] [ 81 | "Gadget" | [ "Base" | "Core" ] ] [ 81 | "Subtext" | [ "Base" | "Core" | "Subtext" ] ] [ 82
    | "Peripheral" | [ "Base" | "Core" ] ] [ 83 | "Rops" | [ "Core" ] ] =}

|-- DIR-RANK  /dir-rank 83  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 81 |
    "Tropashko" | [ "Base" | "Core" ] ] [ 82 | "Restrict" | [ "Base" | "Core" ] ] [ 83 | "Rop" | [ "Base" | "Core" ] ]
    =}
|-- DIR-RANK  /dir-rank 86  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 81 | "Elem" | [
    "Base" | "Core" ] ] [ 81 | "Meta" | [ "Base" | "Core" ] ] [ 81 | "Order" | [ "Base" | "Core" ] ] [ 81 | "Resource" |
    [ "Base" | "Core" ] ] [ 81 | "Source" | [ "Base" | "Core" ] ] [ 81 | "Term" | [ "Base" | "Core" ] ] [ 82 |
    "TermGadget" | [ "Base" | "Core" ] ] [ 84 | "Applied" | [ ] ] [ 84 | "Lattice" | [ ] ] [ 85 | "Check" | [ "Base" |
    "Core" | "Lattice" ] ] [ 85 | "Control" | [ "Base" | "Core" | "Lattice" ] ] [ 86 | "Rops" | [ "Applied" | "Base" |
    "Core" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 89  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 81 | "Calc" | [ "Base" | "Core" ] ] [ 81 | "Filter" | [ "Base" | "Core" ] ] [ 81 | "Gadget" | [ "Base" |
    "Core" ] ] [ 81 | "Range" | [ "Base" | "Core" ] ] [ 88 | "Empty" | [ "Base" | "Core" | "Flat" ] ] [ 89 | "Rops" | [
    "Core" ] ] =}
|-- DIR-RANK  /dir-rank 90  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 81 | "Flow" | [
    "Base" | "Core" ] ] [ 88 | "Confl" | [ "Base" | "Core" | "Flat" ] ] [ 89 | "Deriv" | [ "Base" | "Core" | "Flat" ] ]
    [ 90 | "Rops" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 91  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 80 | "Base" | [ ] ] [ 87
    | "Flat" | [ "Applied" | "Lattice" ] ] [ 90 | "Cox" | [ ] ] [ 91 | "Nest" | [ ] ] =}
*** 50

|-- DIR-RANK  /dir-rank 92  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 62 | "RDF" | [ "Data" | "Type" ] ] [ 76 | "Element" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 76 | "Input" | [ "Base" | "Core" | "Data" ] ] [ 79 | "Change" | [ "Base" | "Data" | "Type" |
    "Writer" ] ] [ 79 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 92 | "Global" | [ "Base" | "Cop" | "Core" |
    "Cox" | "Data" | "Flat" | "Nest" ] ] =}

*** 51 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                 /base-rank
  --------- ------------------------------------ -----------------------------------------------------------------------------------------------------------------------------
  0         "Koshucode.Baala.Overture.Name"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "String"             [ ]
                                                 
  0         "Koshucode.Baala.System"             /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "CliParser"          [ ]
                                                 0     "Exit"               [ ]
                                                 
  1         "Koshucode.Baala.Overture.Text"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Category"           [ ]
                                                 0     "Integer"            [ ]
                                                 0     "Unicode"            [ ]
                                                 1     "Utility"            [ ]
                                                 
  2         "Koshucode.Baala.Overture"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Cache"              [ ]
                                                 0     "Index"              [ ]
                                                 0     "Type"               [ ]
                                                 0     "Value"              [ ]
                                                 1     "Fn"                 [ ]
                                                 1     "Infix"              [ ]
                                                 1     "Misc"               [ ]
                                                 1     "Shorthand"          [ ]
                                                 2     "Text"               [ ]
                                                 
  4         "Koshucode.Baala.Base.Prelude"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Case"               [ ]
                                                 0     "Class"              [ ]
                                                 0     "Import"             [ ]
                                                 1     "Queue"              [ ]
                                                 4     "Pair"               [ "Overture" ]
                                                 
  6         "Koshucode.Baala.Base.List"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Split"              [ ]
                                                 4     "Assoc"              [ "Overture" ]
                                                 4     "List"               [ "Overture" ]
                                                 4     "Select"             [ "Overture" ]
                                                 4     "Set"                [ "Overture" ]
                                                 5     "Picker"             [ "Overture" ]
                                                 6     "Order"              [ "Overture" | "Prelude" ]
                                                 
  6         "Koshucode.Baala.Base.MixText"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "LineBreak"          [ ]
                                                 4     "MixText"            [ "Overture" ]
                                                 5     "MixClass"           [ ]
                                                 5     "MixEncode"          [ "Overture" ]
                                                 6     "Deriv"              [ ]
                                                 
  6         "Koshucode.Baala.Subtext.Operator"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 3     "Basic"              [ ]
                                                 4     "Combine"            [ ]
                                                 5     "Repeat"             [ ]
                                                 6     "Char"               [ "Overture" ]
                                                 
  8         "Koshucode.Baala.Base.Text"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 4     "Comment"            [ "Overture" ]
                                                 4     "Suffix"             [ "Overture" ]
                                                 4     "TextTable"          [ "Overture" ]
                                                 6     "PPrint"             [ "Overture" | "Prelude" ]
                                                 8     "Dots"               [ "List" | "Overture" ]
                                                 
  10        "Koshucode.Baala.Subtext"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "MinMax"             [ ]
                                                 2     "Expr"               [ ]
                                                 7     "Operator"           [ ]
                                                 8     "Bundle"             [ "Operator" ]
                                                 9     "Para"               [ "Operator" ]
                                                 10    "Match"              [ "Operator" ]
                                                 
  14        "Koshucode.Baala.Base.Abort"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 10    "CodePos"            [ "Overture" | "Prelude" | "Text" ]
                                                 11    "Reason"             [ "List" | "Overture" ]
                                                 12    "Report"             [ "List" | "Overture" | "Text" ]
                                                 14    "Message"            [ "Abort" ]
                                                 
  14        "Koshucode.Baala.Base.IO"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 6     "Encoding"           [ "Prelude" ]
                                                 6     "Http"               [ "Overture" | "Prelude" ]
                                                 8     "IOPoint"            [ "List" | "Overture" | "Prelude" ]
                                                 14    "BzFile"             [ "Abort" | "Prelude" ]
                                                 
  17        "Koshucode.Baala.Base.Code"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 14    "Message"            [ "Abort" ]
                                                 15    "Bracket"            [ "Abort" | "List" | "Overture" ]
                                                 16    "Line"               [ "Abort" | "IO" | "Prelude" ]
                                                 16    "Tree"               [ "Abort" | "Overture" | "Prelude" ]
                                                 17    "Clause"             [ "Abort" | "List" | "Prelude" ]
                                                 17    "Infix"              [ "Overture" | "Prelude" ]
                                                 17    "Scan"               [ "Abort" | "IO" | "Overture" | "Prelude" ]
                                                 
  18        "Koshucode.Baala.Base"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 5     "Prelude"            [ ]
                                                 7     "List"               [ ]
                                                 7     "MixText"            [ ]
                                                 9     "Text"               [ ]
                                                 13    "Abort"              [ ]
                                                 15    "IO"                 [ ]
                                                 15    "Message"            [ ]
                                                 18    "Code"               [ ]
                                                 
  22        "Koshucode.Baala.Syntax.Para"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 20    "Para"               [ "Base" | "Overture" ]
                                                 21    "Get"                [ "Base" ]
                                                 21    "ParaSpec"           [ "Base" | "Overture" ]
                                                 22    "Parse"              [ "Base" | "Overture" ]
                                                 
  23        "Koshucode.Baala.Syntax.Symbol"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 4     "AngleText"          [ "Overture" ]
                                                 4     "Term"               [ "Overture" ]
                                                 20    "Message"            [ "Base" ]
                                                 21    "Next"               [ "Base" | "Overture" ]
                                                 22    "Symbol"             [ "Base" | "Overture" ]
                                                 23    "Short"              [ "Base" | "Overture" ]
                                                 
  23        "Koshucode.Baala.Type.Decimal"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 4     "Rational"           [ "Overture" ]
                                                 20    "Decimal"            [ "Base" | "Overture" ]
                                                 21    "Fraction"           [ "Overture" ]
                                                 22    "Coder"              [ "Base" | "Overture" ]
                                                 23    "BinaryAb"           [ "Base" ]
                                                 
  26        "Koshucode.Baala.Type.Time"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Parts"              [ "Base" ]
                                                 22    "Date"               [ "Base" | "Overture" ]
                                                 23    "Clock"              [ "Base" | "Overture" ]
                                                 24    "ClockCalc"          [ "Base" | "Overture" ]
                                                 25    "Time"               [ "Base" | "Overture" ]
                                                 26    "TimeCalc"           [ "Base" | "Overture" ]
                                                 
  29        "Koshucode.Baala.Syntax.Token"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 20    "Message"            [ "Base" ]
                                                 25    "Token"              [ "Base" | "Symbol" ]
                                                 26    "Clip"               [ "Base" | "Overture" | "Symbol" ]
                                                 26    "Pattern"            [ ]
                                                 26    "Utility"            [ "Base" | "Overture" | "Symbol" ]
                                                 27    "Section"            [ "Base" | "Symbol" ]
                                                 28    "Rel"                [ "Base" | "Overture" | "Symbol" ]
                                                 29    "Clause"             [ "Base" | "Overture" | "Symbol" ]
                                                 
  33        "Koshucode.Baala.Syntax.Tree"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 31    "Bracket"            [ "Base" | "Token" ]
                                                 32    "Parse"              [ "Base" | "Overture" | "Token" ]
                                                 32    "Pattern"            [ "Base" | "Token" ]
                                                 33    "Split"              [ "Base" | "Overture" | "Token" ]
                                                 
  35        "Koshucode.Baala.Syntax.Subtree"     /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 4     "Filter"             [ "Overture" ]
                                                 25    "Subtree"            [ "Base" | "Overture" | "Symbol" ]
                                                 26    "DirTree"            [ "Base" | "Overture" ]
                                                 26    "Xml"                [ "Base" | "Overture" | "Symbol" ]
                                                 35    "Decode"             [ "Base" | "Overture" | "Symbol" | "Token" | "Tree" ]
                                                 
  36        "Koshucode.Baala.Syntax.Attr"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "AttrName"           [ ]
                                                 25    "Message"            [ "Base" | "Para" | "Symbol" ]
                                                 35    "Attr"               [ "Base" | "Overture" | "Para" | "Tree" ]
                                                 35    "Slot"               [ "Base" | "Overture" | "Token" | "Tree" ]
                                                 36    "AttrEd"             [ "Base" | "Symbol" | "Token" | "Tree" ]
                                                 
  37        "Koshucode.Baala.Syntax"             /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 23    "Para"               [ ]
                                                 24    "Symbol"             [ ]
                                                 26    "Message"            [ ]
                                                 30    "Token"              [ ]
                                                 33    "Pattern"            [ ]
                                                 34    "Tree"               [ ]
                                                 36    "Subtree"            [ ]
                                                 37    "Attr"               [ ]
                                                 
  41        "Koshucode.Baala.Type.Judge"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 39    "JudgeClass"         [ "Base" | "Syntax" ]
                                                 40    "Judge"              [ "Base" | "Overture" | "Syntax" ]
                                                 41    "Interp"             [ "Base" | "Syntax" ]
                                                 
  46        "Koshucode.Baala.Type.Rel"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 43    "Order"              [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 43    "TermPicker"         [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 44    "Head"               [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ]
                                                 45    "Rel"                [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 46    "Mono"               [ "Base" ]
                                                 
  47        "Koshucode.Baala.Type"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 20    "Message"            [ "Base" | "Overture" ]
                                                 24    "Decimal"            [ ]
                                                 27    "Time"               [ ]
                                                 42    "Judge"              [ ]
                                                 43    "Type"               [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 47    "Rel"                [ ]
                                                 
  52        "Koshucode.Baala.Core.Lexmap"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 20    "Message"            [ "Base" ]
                                                 39    "Lexmap"             [ "Base" | "Overture" | "Syntax" ]
                                                 51    "LexmapTrees"        [ "Base" | "Syntax" ]
                                                 52    "Construct"          [ "Base" | "Overture" | "Syntax" ]
                                                 
  53        "Koshucode.Baala.Data.Class"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Message"            [ "Base" ]
                                                 49    "Edge"               [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 50    "Simple"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 51    "Complex"            [ "Base" | "Syntax" | "Type" ]
                                                 52    "Content"            [ "Base" | "Overture" | "Type" ]
                                                 52    "Map"                [ "Overture" ]
                                                 53    "Encode"             [ "Base" | "Syntax" | "Type" ]
                                                 
  56        "Koshucode.Baala.Data.Decode"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 39    "Message"            [ "Base" | "Syntax" ]
                                                 40    "Term"               [ "Base" | "Overture" | "Syntax" ]
                                                 49    "Type"               [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 50    "Numeric"            [ "Base" | "Syntax" | "Type" ]
                                                 55    "Content"            [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                 56    "Dataset"            [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                 
  59        "Koshucode.Baala.Data.Church"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 49    "Message"            [ "Base" | "Syntax" | "Type" ]
                                                 50    "Cox"                [ "Base" | "Overture" | "Syntax" ]
                                                 51    "Cop"                [ "Base" | "Overture" | "Syntax" ]
                                                 58    "Build"              [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ]
                                                 59    "Run"                [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                 
  60        "Koshucode.Baala.Data"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 50    "Message"            [ ]
                                                 54    "Class"              [ ]
                                                 57    "Decode"             [ ]
                                                 58    "Content"            [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                 60    "Church"             [ ]
                                                 
  62        "Koshucode.Baala.Core.Relkit"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 39    "Message"            [ "Base" | "Syntax" ]
                                                 54    "Relkit"             [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ]
                                                 55    "Construct"          [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 62    "Run"                [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 
  62        "Koshucode.Baala.Type.Type"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 55    "Genus"              [ "Class" | "Overture" ]
                                                 62    "Predefined where"   [ "Class" | "Data" ]
                                                 
  66        "Koshucode.Baala.Core.Relmap"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 49    "Message"            [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 49    "Result"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 62    "Option"             [ "Base" | "Data" | "Syntax" ]
                                                 64    "Relmap"             [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 65    "Rop"                [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                 65    "Specialize"         [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 66    "Construct"          [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 66    "Global"             [ "Base" | "Data" | "Overture" | "Type" ]
                                                 
  69        "Koshucode.Baala.Core.Assert"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 39    "Message"            [ "Base" | "Syntax" ]
                                                 62    "RelTable"           [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 68    "Assert"             [ "Base" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 69    "Run"                [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ]
                                                 
  73        "Koshucode.Baala.Core.Resource"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 20    "Message"            [ "Base" ]
                                                 54    "Clause"             [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ]
                                                 71    "Resource"           [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ]
                                                 72    "Concrete"           [ "Base" | "Data" | "Relmap" | "Syntax" | "Type" ]
                                                 72    "Include"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" | "Type" ]
                                                 72    "Run"                [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 73    "Read"               [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ]
                                                 
  74        "Koshucode.Baala.Core"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 51    "Message"            [ ]
                                                 53    "Lexmap"             [ ]
                                                 63    "Relkit"             [ ]
                                                 67    "Relmap"             [ ]
                                                 70    "Assert"             [ ]
                                                 74    "Resource"           [ ]
                                                 
  77        "Koshucode.Baala.Writer"             /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 76    "Csv"                [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 76    "Html"               [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 76    "Json"               [ "Core" | "Data" | "Syntax" | "Type" ]
                                                 76    "Judge"              [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ]
                                                 77    "Koshu"              [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ]
                                                 
  78        "Koshucode.Baala.Cop"                /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Replace"            [ ]
                                                 62    "Coxhand"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                 63    "Logic"              [ "Data" | "Overture" ]
                                                 77    "Arith"              [ "Base" | "Data" | "Overture" | "Type" ]
                                                 77    "List"               [ "Base" | "Data" | "Overture" | "Type" ]
                                                 77    "Misc"               [ "Base" | "Data" | "Syntax" ]
                                                 77    "Order"              [ "Data" | "Overture" ]
                                                 77    "Text"               [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 77    "Time"               [ "Data" | "Overture" | "Type" ]
                                                 77    "Type"               [ "Data" | "Overture" ]
                                                 78    "Bundle"             [ "Base" | "Data" ]
                                                 
  78        "Koshucode.Baala.Rop.Base"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 76    "Message"            [ "Core" ]
                                                 77    "Define"             [ "Core" ]
                                                 78    "Rops"               [ "Core" ]
                                                 
  79        "Koshucode.Baala"                    /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 1     "System"             [ ]
                                                 3     "Overture"           [ "Text" ]
                                                 11    "Subtext"            [ "Operator" ]
                                                 19    "Base"               [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ]
                                                 38    "Syntax"             [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" | "Tree" ]
                                                 48    "Type"               [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ]
                                                 61    "Data"               [ "Church" | "Class" | "Decode" | "Type" ]
                                                 62    "DataPlus"           [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 75    "Core"               [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                 78    "Writer"             [ ]
                                                 79    "Cop"                [ ]
                                                 
  79        "Koshucode.Baala.Rop.Base.Get"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 77    "Get"                [ "Core" ]
                                                 78    "Rel"                [ "Core" ]
                                                 79    "Cox"                [ "Core" ]
                                                 
  80        "Koshucode.Baala.Toolkit.Main"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 63    "KoshuRdf"           [ "Base" | "Data" | "Syntax" | "Type" ]
                                                 76    "KoshuSyntax"        [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 80    "KoshuChange"        [ "Base" ]
                                                 80    "KoshuFilter"        [ "Base" | "Core" | "Data" | "Type" ]
                                                 80    "KoshuMain"          [ "Base" | "Core" | "Data" | "Overture" | "Type" | "Writer" ]
                                                 
  81        "Koshucode.Baala.Rop.Cox.Type"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 81    "Clock"              [ "Base" | "Core" ]
                                                 81    "Dec"                [ "Base" | "Core" ]
                                                 
  83        "Koshucode.Baala.Rop.Flat.Applied"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 63    "PoScale"            [ ]
                                                 81    "Gadget"             [ "Base" | "Core" ]
                                                 81    "Subtext"            [ "Base" | "Core" | "Subtext" ]
                                                 82    "Peripheral"         [ "Base" | "Core" ]
                                                 83    "Rops"               [ "Core" ]
                                                 
  83        "Koshucode.Baala.Rop.Flat.Lattice"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 81    "Tropashko"          [ "Base" | "Core" ]
                                                 82    "Restrict"           [ "Base" | "Core" ]
                                                 83    "Rop"                [ "Base" | "Core" ]
                                                 
  86        "Koshucode.Baala.Rop.Flat"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 81    "Elem"               [ "Base" | "Core" ]
                                                 81    "Meta"               [ "Base" | "Core" ]
                                                 81    "Order"              [ "Base" | "Core" ]
                                                 81    "Resource"           [ "Base" | "Core" ]
                                                 81    "Source"             [ "Base" | "Core" ]
                                                 81    "Term"               [ "Base" | "Core" ]
                                                 82    "TermGadget"         [ "Base" | "Core" ]
                                                 84    "Applied"            [ ]
                                                 84    "Lattice"            [ ]
                                                 85    "Check"              [ "Base" | "Core" | "Lattice" ]
                                                 85    "Control"            [ "Base" | "Core" | "Lattice" ]
                                                 86    "Rops"               [ "Applied" | "Base" | "Core" | "Lattice" ]
                                                 
  89        "Koshucode.Baala.Rop.Cox"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "GeoDatumJp"         [ ]
                                                 81    "Calc"               [ "Base" | "Core" ]
                                                 81    "Filter"             [ "Base" | "Core" ]
                                                 81    "Gadget"             [ "Base" | "Core" ]
                                                 81    "Range"              [ "Base" | "Core" ]
                                                 88    "Empty"              [ "Base" | "Core" | "Flat" ]
                                                 89    "Rops"               [ "Core" ]
                                                 
  90        "Koshucode.Baala.Rop.Nest"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 81    "Flow"               [ "Base" | "Core" ]
                                                 88    "Confl"              [ "Base" | "Core" | "Flat" ]
                                                 89    "Deriv"              [ "Base" | "Core" | "Flat" ]
                                                 90    "Rops"               [ "Base" | "Core" ]
                                                 
  91        "Koshucode.Baala.Rop"                /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 80    "Base"               [ ]
                                                 87    "Flat"               [ "Applied" | "Lattice" ]
                                                 90    "Cox"                [ ]
                                                 91    "Nest"               [ ]
                                                 
  92        "Koshucode.Baala.Toolkit.Library"    /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Version"            [ ]
                                                 62    "RDF"                [ "Data" | "Type" ]
                                                 76    "Element"            [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 76    "Input"              [ "Base" | "Core" | "Data" ]
                                                 79    "Change"             [ "Base" | "Data" | "Type" | "Writer" ]
                                                 79    "Run"                [ "Base" | "Core" | "Data" | "Writer" ]
                                                 92    "Global"             [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                 

=== rel

**
**  SUMMARY
**      51 judges on DIR-RANK
**      51 judges in total
**

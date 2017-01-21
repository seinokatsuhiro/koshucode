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
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Overture.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Category"
    | [ ] ] [ 0 | "Unicode" | [ ] ] [ 2 | "Textual" | [ ] ] [ 3 | "Integer" | [ ] ] [ 3 | "Utility" | [ ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Overture"  /base-rank {= /rank /base /import-dir [ 0 | "Cache" | [ ] ]
    [ 0 | "Index" | [ ] ] [ 0 | "Some" | [ ] ] [ 0 | "Type" | [ ] ] [ 0 | "Value" | [ ] ] [ 1 | "Fn" | [ ] ] [ 1 |
    "Infix" | [ ] ] [ 1 | "Misc" | [ ] ] [ 1 | "Shorthand" | [ ] ] [ 4 | "Text" | [ ] ] =}
|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Subtext.Operator"  /base-rank {= /rank /base /import-dir [ 3 | "Basic"
    | [ ] ] [ 4 | "Combine" | [ ] ] [ 5 | "Repeat" | [ ] ] [ 6 | "Char" | [ "Overture" ] ] =}

|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Case" | [ ]
    ] [ 0 | "Class" | [ ] ] [ 6 | "Import" | [ "Overture" ] ] [ 6 | "Pair" | [ "Overture" ] ] [ 7 | "Queue" | [ ] ] =}
|-- DIR-RANK  /dir-rank 8  /dir "Koshucode.Baala.Base.MixText"  /base-rank {= /rank /base /import-dir [ 0 | "LineBreak"
    | [ ] ] [ 6 | "MixText" | [ "Overture" ] ] [ 7 | "MixClass" | [ ] ] [ 7 | "MixEncode" | [ "Overture" ] ] [ 8 |
    "Deriv" | [ ] ] =}
|-- DIR-RANK  /dir-rank 9  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Split" | [ ] ]
    [ 6 | "Assoc" | [ "Overture" ] ] [ 6 | "List" | [ "Overture" ] ] [ 6 | "Select" | [ "Overture" ] ] [ 6 | "Set" | [
    "Overture" ] ] [ 7 | "Picker" | [ "Overture" ] ] [ 9 | "Order" | [ "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 11  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 6 | "Comment" | [
    "Overture" ] ] [ 6 | "Suffix" | [ "Overture" ] ] [ 6 | "TextTable" | [ "Overture" ] ] [ 9 | "PPrint" | [ "Overture"
    | "Prelude" ] ] [ 11 | "Dots" | [ "List" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 15  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 9 | "CodePos" | [
    "Overture" | "Prelude" ] ] [ 11 | "Reason" | [ "List" | "Overture" ] ] [ 13 | "Report" | [ "List" | "Overture" |
    "Text" ] ] [ 15 | "Message" | [ "Abort" ] ] =}

|-- DIR-RANK  /dir-rank 15  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 9 | "Encoding" | [
    "Prelude" ] ] [ 9 | "Http" | [ "Overture" | "Prelude" ] ] [ 11 | "IOPoint" | [ "List" | "Overture" | "Prelude" ] ] [
    15 | "BzFile" | [ "Abort" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Base.Code"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [
    "Abort" ] ] [ 16 | "Bracket" | [ "Abort" | "List" | "Overture" ] ] [ 17 | "Line" | [ "Abort" | "IO" | "Overture" ] ]
    [ 17 | "Tree" | [ "Abort" | "Overture" ] ] [ 18 | "Clause" | [ "Abort" | "List" | "Prelude" ] ] [ 18 | "Infix" | [
    "Overture" | "Prelude" ] ] [ 18 | "Scan" | [ "Abort" | "IO" | "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 19  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 8 | "Prelude" | [ ] ] [
    9 | "MixText" | [ ] ] [ 10 | "List" | [ ] ] [ 12 | "Text" | [ ] ] [ 14 | "Abort" | [ ] ] [ 16 | "IO" | [ ] ] [ 16 |
    "Message" | [ ] ] [ 19 | "Code" | [ ] ] =}
|-- DIR-RANK  /dir-rank 23  /dir "Koshucode.Baala.Subtext.Sieve"  /base-rank {= /rank /base /import-dir [ 21 | "Token" |
    [ "Base" | "Overture" ] ] [ 22 | "Tree" | [ "Base" | "Operator" | "Overture" ] ] [ 23 | "Sivmap" | [ "Operator" |
    "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 23  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 21 | "Para" | [
    "Base" | "Overture" ] ] [ 22 | "Get" | [ "Base" ] ] [ 22 | "ParaSpec" | [ "Base" | "Overture" ] ] [ 23 | "Parse" | [
    "Base" | "Overture" ] ] =}

|-- DIR-RANK  /dir-rank 24  /dir "Koshucode.Baala.Subtext"  /base-rank {= /rank /base /import-dir [ 0 | "MinMax" | [ ] ]
    [ 2 | "Expr" | [ ] ] [ 7 | "Operator" | [ ] ] [ 8 | "Bundle" | [ "Operator" ] ] [ 9 | "Para" | [ "Operator" |
    "Overture" ] ] [ 10 | "Match" | [ "Operator" ] ] [ 24 | "Sieve" | [ ] ] =}
|-- DIR-RANK  /dir-rank 24  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 6 | "Term" | [
    "Overture" ] ] [ 21 | "AngleText" | [ "Base" | "Overture" ] ] [ 21 | "Message" | [ "Base" ] ] [ 22 | "Next" | [
    "Base" | "Overture" ] ] [ 23 | "Symbol" | [ "Base" | "Overture" ] ] [ 24 | "Short" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 24  /dir "Koshucode.Baala.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 6 | "Rational"
    | [ "Overture" ] ] [ 21 | "Decimal" | [ "Base" | "Overture" ] ] [ 22 | "Fraction" | [ "Overture" ] ] [ 23 | "Coder"
    | [ "Base" | "Overture" ] ] [ 24 | "BinaryAb" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 27  /dir "Koshucode.Baala.Type.Time"  /base-rank {= /rank /base /import-dir [ 22 | "Parts" | [
    "Base" ] ] [ 23 | "Date" | [ "Base" | "Overture" ] ] [ 24 | "Clock" | [ "Base" | "Overture" ] ] [ 25 | "ClockCalc" |
    [ "Base" | "Overture" ] ] [ 26 | "Time" | [ "Base" | "Overture" ] ] [ 27 | "TimeCalc" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 30  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 21 | "Message"
    | [ "Base" | "Overture" ] ] [ 26 | "Token" | [ "Base" | "Overture" | "Symbol" ] ] [ 27 | "Clip" | [ "Base" |
    "Overture" | "Symbol" ] ] [ 27 | "Pattern" | [ ] ] [ 27 | "Utility" | [ "Base" | "Overture" | "Symbol" ] ] [ 28 |
    "Section" | [ "Base" | "Overture" ] ] [ 29 | "Rel" | [ "Base" | "Overture" | "Symbol" ] ] [ 30 | "Clause" | [ "Base"
    | "Overture" | "Symbol" ] ] =}

|-- DIR-RANK  /dir-rank 34  /dir "Koshucode.Baala.Syntax.Tree"  /base-rank {= /rank /base /import-dir [ 32 | "Bracket" |
    [ "Base" | "Overture" | "Token" ] ] [ 33 | "Parse" | [ "Base" | "Overture" | "Symbol" | "Token" ] ] [ 33 | "Pattern"
    | [ "Base" | "Token" ] ] [ 34 | "Split" | [ "Base" | "Overture" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 36  /dir "Koshucode.Baala.Syntax.Subtree"  /base-rank {= /rank /base /import-dir [ 26 |
    "Subtree" | [ "Base" | "Overture" | "Subtext" | "Symbol" ] ] [ 27 | "DirTree" | [ "Base" | "Overture" ] ] [ 27 |
    "Xml" | [ "Base" | "Overture" | "Subtext" | "Symbol" ] ] [ 36 | "Decode" | [ "Base" | "Overture" | "Subtext" |
    "Symbol" | "Token" | "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 37  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 26 | "Message" | [ "Base" | "Para" | "Symbol" ] ] [ 36 | "Attr" | [ "Base" | "Overture" | "Para" | "Tree" ]
    ] [ 36 | "Slot" | [ "Base" | "Overture" | "Token" | "Tree" ] ] [ 37 | "AttrEd" | [ "Base" | "Overture" | "Symbol" |
    "Token" | "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 24 | "Para" | [ ] ] [
    25 | "Symbol" | [ ] ] [ 27 | "Message" | [ ] ] [ 31 | "Token" | [ ] ] [ 34 | "Pattern" | [ ] ] [ 35 | "Tree" | [ ] ]
    [ 37 | "Subtree" | [ ] ] [ 38 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 42  /dir "Koshucode.Baala.Type.Judge"  /base-rank {= /rank /base /import-dir [ 40 | "JudgeClass"
    | [ "Base" | "Syntax" ] ] [ 41 | "Judge" | [ "Base" | "Overture" | "Syntax" ] ] [ 42 | "Interp" | [ "Base" |
    "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 47  /dir "Koshucode.Baala.Type.Rel"  /base-rank {= /rank /base /import-dir [ 44 | "Order" | [
    "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 44 | "TermPicker" | [ "Base" | "Judge" | "Overture" | "Syntax" ] ] [
    45 | "Head" | [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ] ] [ 46 | "Rel" | [ "Base" | "Judge" | "Overture"
    | "Syntax" ] ] [ 47 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 48  /dir "Koshucode.Baala.Type"  /base-rank {= /rank /base /import-dir [ 21 | "Message" | [
    "Base" | "Overture" ] ] [ 25 | "Decimal" | [ ] ] [ 28 | "Time" | [ ] ] [ 43 | "Judge" | [ ] ] [ 44 | "Type" | [
    "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 48 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 53  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 21 | "Message" |
    [ "Base" ] ] [ 40 | "Lexmap" | [ "Base" | "Overture" | "Syntax" ] ] [ 52 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [
    53 | "Construct" | [ "Base" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 54  /dir "Koshucode.Baala.Data.Class"  /base-rank {= /rank /base /import-dir [ 22 | "Message" |
    [ "Base" ] ] [ 50 | "Edge" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 51 | "Simple" | [ "Base" | "Overture" |
    "Syntax" | "Type" ] ] [ 52 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 53 | "Content" | [ "Base" | "Overture" |
    "Type" ] ] [ 53 | "Map" | [ "Overture" ] ] [ 54 | "Encode" | [ "Base" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 57  /dir "Koshucode.Baala.Data.Decode"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Syntax" ] ] [ 41 | "Term" | [ "Base" | "Overture" | "Syntax" ] ] [ 50 | "Type" | [ "Base" | "Overture" |
    "Syntax" | "Type" ] ] [ 51 | "Numeric" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 56 | "Content" | [ "Base" |
    "Class" | "Overture" | "Syntax" | "Type" ] ] [ 57 | "Dataset" | [ "Base" | "Class" | "Overture" | "Syntax" | "Type"
    ] ] =}

|-- DIR-RANK  /dir-rank 60  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 50 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 51 | "Cox" | [ "Base" | "Overture" | "Syntax" ] ] [ 52 | "Cop" | [ "Base" |
    "Overture" | "Syntax" ] ] [ 59 | "Build" | [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ] ] [ 60 | "Run" |
    [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 51 | "Message" | [ ] ]
    [ 55 | "Class" | [ ] ] [ 58 | "Decode" | [ ] ] [ 59 | "Content" | [ "Base" | "Class" | "Decode" | "Overture" |
    "Syntax" | "Type" ] ] [ 61 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 63  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Syntax" ] ] [ 55 | "Relkit" | [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ] ] [ 56 |
    "Construct" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 63 | "Run" | [ "Base" | "Data" | "Overture" | "Syntax"
    | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 63  /dir "Koshucode.Baala.Type.Type"  /base-rank {= /rank /base /import-dir [ 56 | "Genus" | [
    "Class" | "Overture" ] ] [ 63 | "Predefined where" | [ "Class" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 67  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 50 | "Message" |
    [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 50 | "Result" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 63 |
    "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 65 | "Relmap" | [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax"
    | "Type" ] ] [ 66 | "Rop" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 66 | "Specialize" | [ "Base" | "Lexmap" |
    "Overture" | "Relkit" | "Syntax" | "Type" ] ] [ 67 | "Construct" | [ "Base" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" | "Type" ] ] [ 67 | "Global" | [ "Base" | "Data" | "Overture" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 40 | "Message" |
    [ "Base" | "Syntax" ] ] [ 63 | "RelTable" | [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 69 | "Assert" |
    [ "Base" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ] ] [ 70 | "Run" | [ "Base" | "Data" | "Lexmap" |
    "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 74  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 21 | "Message"
    | [ "Base" ] ] [ 55 | "Clause" | [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ] ] [ 72 | "Resource" | [
    "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ] ] [ 73 | "Concrete" |
    [ "Base" | "Data" | "Relmap" | "Syntax" | "Type" ] ] [ 73 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" |
    "Overture" | "Relmap" | "Syntax" | "Type" ] ] [ 73 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" |
    "Relmap" | "Syntax" | "Type" ] ] [ 74 | "Read" | [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 75  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 52 | "Message" | [ ] ]
    [ 54 | "Lexmap" | [ ] ] [ 64 | "Relkit" | [ ] ] [ 68 | "Relmap" | [ ] ] [ 71 | "Assert" | [ ] ] [ 75 | "Resource" |
    [ ] ] =}
|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 77 | "Csv" | [ "Base"
    | "Core" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 77 | "Html" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 77 | "Json" | [ "Core" | "Data" | "Syntax" | "Type" ] ] [ 77 | "Judge" | [ "Base" | "Core" |
    "Overture" | "Syntax" | "Type" ] ] [ 78 | "Koshu" | [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    63 | "Coxhand" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 64 | "Logic" | [ "Data" | "Overture" ] ] [ 78 |
    "Arith" | [ "Base" | "Data" | "Overture" | "Type" ] ] [ 78 | "List" | [ "Base" | "Data" | "Overture" | "Type" ] ] [
    78 | "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 78 | "Order" | [ "Data" | "Overture" ] ] [ 78 | "Text" | [ "Base" |
    "Data" | "Overture" | "Subtext" | "Syntax" | "Type" ] ] [ 78 | "Time" | [ "Data" | "Overture" | "Type" ] ] [ 78 |
    "Type" | [ "Data" | "Overture" ] ] [ 79 | "Bundle" | [ "Base" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 77 | "Message" | [
    "Core" ] ] [ 78 | "Define" | [ "Core" ] ] [ 79 | "Rops" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 1 | "System" | [ ] ] [ 5 |
    "Overture" | [ "Text" ] ] [ 20 | "Base" | [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ] ] [
    25 | "Subtext" | [ "Operator" | "Sieve" ] ] [ 39 | "Syntax" | [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" |
    "Tree" ] ] [ 49 | "Type" | [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ] ] [ 62 | "Data" | [ "Church" | "Class"
    | "Decode" | "Type" ] ] [ 63 | "DataPlus" | [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 76 | "Core" | [
    "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 79 | "Writer" | [ ] ] [ 80 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Rop.Base.Get"  /base-rank {= /rank /base /import-dir [ 78 | "Get" | [
    "Core" ] ] [ 79 | "Rel" | [ "Core" ] ] [ 80 | "Cox" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 64 | "KoshuRdf"
    | [ "Base" | "Data" | "Syntax" | "Type" ] ] [ 77 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 81 | "KoshuChange" | [ "Base" ] ] [ 81 | "KoshuFilter" | [ "Base" | "Core" | "Data" | "Type"
    ] ] [ 81 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Overture" | "Type" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 82  /dir "Koshucode.Baala.Rop.Cox.Type"  /base-rank {= /rank /base /import-dir [ 82 | "Clock" |
    [ "Base" | "Core" ] ] [ 82 | "Dec" | [ "Base" | "Core" ] ] =}

|-- DIR-RANK  /dir-rank 84  /dir "Koshucode.Baala.Rop.Flat.Applied"  /base-rank {= /rank /base /import-dir [ 64 |
    "PoScale" | [ ] ] [ 82 | "Gadget" | [ "Base" | "Core" ] ] [ 82 | "Subtext" | [ "Base" | "Core" | "Subtext" ] ] [ 83
    | "Peripheral" | [ "Base" | "Core" ] ] [ 84 | "Rops" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 84  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 82 |
    "Tropashko" | [ "Base" | "Core" ] ] [ 83 | "Restrict" | [ "Base" | "Core" ] ] [ 84 | "Rop" | [ "Base" | "Core" ] ]
    =}
|-- DIR-RANK  /dir-rank 87  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 82 | "Elem" | [
    "Base" | "Core" ] ] [ 82 | "Meta" | [ "Base" | "Core" ] ] [ 82 | "Order" | [ "Base" | "Core" ] ] [ 82 | "Resource" |
    [ "Base" | "Core" ] ] [ 82 | "Source" | [ "Base" | "Core" ] ] [ 82 | "Term" | [ "Base" | "Core" ] ] [ 83 |
    "TermGadget" | [ "Base" | "Core" ] ] [ 85 | "Applied" | [ ] ] [ 85 | "Lattice" | [ ] ] [ 86 | "Check" | [ "Base" |
    "Core" | "Lattice" ] ] [ 86 | "Control" | [ "Base" | "Core" | "Lattice" ] ] [ 87 | "Rops" | [ "Applied" | "Base" |
    "Core" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 90  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 82 | "Calc" | [ "Base" | "Core" ] ] [ 82 | "Filter" | [ "Base" | "Core" ] ] [ 82 | "Gadget" | [ "Base" |
    "Core" ] ] [ 82 | "Range" | [ "Base" | "Core" ] ] [ 89 | "Empty" | [ "Base" | "Core" | "Flat" ] ] [ 90 | "Rops" | [
    "Core" ] ] =}
|-- DIR-RANK  /dir-rank 91  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 82 | "Flow" | [
    "Base" | "Core" ] ] [ 89 | "Confl" | [ "Base" | "Core" | "Flat" ] ] [ 90 | "Deriv" | [ "Base" | "Core" | "Flat" ] ]
    [ 91 | "Rops" | [ "Base" | "Core" ] ] =}
*** 50

|-- DIR-RANK  /dir-rank 92  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 81 | "Base" | [ ] ] [ 88
    | "Flat" | [ "Applied" | "Lattice" ] ] [ 91 | "Cox" | [ ] ] [ 92 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 93  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 63 | "RDF" | [ "Data" | "Type" ] ] [ 77 | "Element" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 77 | "Input" | [ "Base" | "Core" | "Data" ] ] [ 80 | "Change" | [ "Base" | "Data" | "Type" |
    "Writer" ] ] [ 80 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 93 | "Global" | [ "Base" | "Cop" | "Core" |
    "Cox" | "Data" | "Flat" | "Nest" ] ] =}

*** 52 judges

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
                                                 
  3         "Koshucode.Baala.Overture.Text"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Category"           [ ]
                                                 0     "Unicode"            [ ]
                                                 2     "Textual"            [ ]
                                                 3     "Integer"            [ ]
                                                 3     "Utility"            [ ]
                                                 
  4         "Koshucode.Baala.Overture"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Cache"              [ ]
                                                 0     "Index"              [ ]
                                                 0     "Some"               [ ]
                                                 0     "Type"               [ ]
                                                 0     "Value"              [ ]
                                                 1     "Fn"                 [ ]
                                                 1     "Infix"              [ ]
                                                 1     "Misc"               [ ]
                                                 1     "Shorthand"          [ ]
                                                 4     "Text"               [ ]
                                                 
  6         "Koshucode.Baala.Subtext.Operator"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 3     "Basic"              [ ]
                                                 4     "Combine"            [ ]
                                                 5     "Repeat"             [ ]
                                                 6     "Char"               [ "Overture" ]
                                                 
  7         "Koshucode.Baala.Base.Prelude"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Case"               [ ]
                                                 0     "Class"              [ ]
                                                 6     "Import"             [ "Overture" ]
                                                 6     "Pair"               [ "Overture" ]
                                                 7     "Queue"              [ ]
                                                 
  8         "Koshucode.Baala.Base.MixText"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "LineBreak"          [ ]
                                                 6     "MixText"            [ "Overture" ]
                                                 7     "MixClass"           [ ]
                                                 7     "MixEncode"          [ "Overture" ]
                                                 8     "Deriv"              [ ]
                                                 
  9         "Koshucode.Baala.Base.List"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Split"              [ ]
                                                 6     "Assoc"              [ "Overture" ]
                                                 6     "List"               [ "Overture" ]
                                                 6     "Select"             [ "Overture" ]
                                                 6     "Set"                [ "Overture" ]
                                                 7     "Picker"             [ "Overture" ]
                                                 9     "Order"              [ "Overture" | "Prelude" ]
                                                 
  11        "Koshucode.Baala.Base.Text"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 6     "Comment"            [ "Overture" ]
                                                 6     "Suffix"             [ "Overture" ]
                                                 6     "TextTable"          [ "Overture" ]
                                                 9     "PPrint"             [ "Overture" | "Prelude" ]
                                                 11    "Dots"               [ "List" | "Overture" ]
                                                 
  15        "Koshucode.Baala.Base.Abort"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 9     "CodePos"            [ "Overture" | "Prelude" ]
                                                 11    "Reason"             [ "List" | "Overture" ]
                                                 13    "Report"             [ "List" | "Overture" | "Text" ]
                                                 15    "Message"            [ "Abort" ]
                                                 
  15        "Koshucode.Baala.Base.IO"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 9     "Encoding"           [ "Prelude" ]
                                                 9     "Http"               [ "Overture" | "Prelude" ]
                                                 11    "IOPoint"            [ "List" | "Overture" | "Prelude" ]
                                                 15    "BzFile"             [ "Abort" | "Overture" ]
                                                 
  18        "Koshucode.Baala.Base.Code"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 15    "Message"            [ "Abort" ]
                                                 16    "Bracket"            [ "Abort" | "List" | "Overture" ]
                                                 17    "Line"               [ "Abort" | "IO" | "Overture" ]
                                                 17    "Tree"               [ "Abort" | "Overture" ]
                                                 18    "Clause"             [ "Abort" | "List" | "Prelude" ]
                                                 18    "Infix"              [ "Overture" | "Prelude" ]
                                                 18    "Scan"               [ "Abort" | "IO" | "Overture" | "Prelude" ]
                                                 
  19        "Koshucode.Baala.Base"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 8     "Prelude"            [ ]
                                                 9     "MixText"            [ ]
                                                 10    "List"               [ ]
                                                 12    "Text"               [ ]
                                                 14    "Abort"              [ ]
                                                 16    "IO"                 [ ]
                                                 16    "Message"            [ ]
                                                 19    "Code"               [ ]
                                                 
  23        "Koshucode.Baala.Subtext.Sieve"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Token"              [ "Base" | "Overture" ]
                                                 22    "Tree"               [ "Base" | "Operator" | "Overture" ]
                                                 23    "Sivmap"             [ "Operator" | "Overture" ]
                                                 
  23        "Koshucode.Baala.Syntax.Para"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Para"               [ "Base" | "Overture" ]
                                                 22    "Get"                [ "Base" ]
                                                 22    "ParaSpec"           [ "Base" | "Overture" ]
                                                 23    "Parse"              [ "Base" | "Overture" ]
                                                 
  24        "Koshucode.Baala.Subtext"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "MinMax"             [ ]
                                                 2     "Expr"               [ ]
                                                 7     "Operator"           [ ]
                                                 8     "Bundle"             [ "Operator" ]
                                                 9     "Para"               [ "Operator" | "Overture" ]
                                                 10    "Match"              [ "Operator" ]
                                                 24    "Sieve"              [ ]
                                                 
  24        "Koshucode.Baala.Syntax.Symbol"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 6     "Term"               [ "Overture" ]
                                                 21    "AngleText"          [ "Base" | "Overture" ]
                                                 21    "Message"            [ "Base" ]
                                                 22    "Next"               [ "Base" | "Overture" ]
                                                 23    "Symbol"             [ "Base" | "Overture" ]
                                                 24    "Short"              [ "Base" | "Overture" ]
                                                 
  24        "Koshucode.Baala.Type.Decimal"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 6     "Rational"           [ "Overture" ]
                                                 21    "Decimal"            [ "Base" | "Overture" ]
                                                 22    "Fraction"           [ "Overture" ]
                                                 23    "Coder"              [ "Base" | "Overture" ]
                                                 24    "BinaryAb"           [ "Base" ]
                                                 
  27        "Koshucode.Baala.Type.Time"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 22    "Parts"              [ "Base" ]
                                                 23    "Date"               [ "Base" | "Overture" ]
                                                 24    "Clock"              [ "Base" | "Overture" ]
                                                 25    "ClockCalc"          [ "Base" | "Overture" ]
                                                 26    "Time"               [ "Base" | "Overture" ]
                                                 27    "TimeCalc"           [ "Base" | "Overture" ]
                                                 
  30        "Koshucode.Baala.Syntax.Token"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Message"            [ "Base" | "Overture" ]
                                                 26    "Token"              [ "Base" | "Overture" | "Symbol" ]
                                                 27    "Clip"               [ "Base" | "Overture" | "Symbol" ]
                                                 27    "Pattern"            [ ]
                                                 27    "Utility"            [ "Base" | "Overture" | "Symbol" ]
                                                 28    "Section"            [ "Base" | "Overture" ]
                                                 29    "Rel"                [ "Base" | "Overture" | "Symbol" ]
                                                 30    "Clause"             [ "Base" | "Overture" | "Symbol" ]
                                                 
  34        "Koshucode.Baala.Syntax.Tree"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 32    "Bracket"            [ "Base" | "Overture" | "Token" ]
                                                 33    "Parse"              [ "Base" | "Overture" | "Symbol" | "Token" ]
                                                 33    "Pattern"            [ "Base" | "Token" ]
                                                 34    "Split"              [ "Base" | "Overture" | "Token" ]
                                                 
  36        "Koshucode.Baala.Syntax.Subtree"     /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 26    "Subtree"            [ "Base" | "Overture" | "Subtext" | "Symbol" ]
                                                 27    "DirTree"            [ "Base" | "Overture" ]
                                                 27    "Xml"                [ "Base" | "Overture" | "Subtext" | "Symbol" ]
                                                 36    "Decode"             [ "Base" | "Overture" | "Subtext" | "Symbol" | "Token" | "Tree" ]
                                                 
  37        "Koshucode.Baala.Syntax.Attr"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "AttrName"           [ ]
                                                 26    "Message"            [ "Base" | "Para" | "Symbol" ]
                                                 36    "Attr"               [ "Base" | "Overture" | "Para" | "Tree" ]
                                                 36    "Slot"               [ "Base" | "Overture" | "Token" | "Tree" ]
                                                 37    "AttrEd"             [ "Base" | "Overture" | "Symbol" | "Token" | "Tree" ]
                                                 
  38        "Koshucode.Baala.Syntax"             /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 24    "Para"               [ ]
                                                 25    "Symbol"             [ ]
                                                 27    "Message"            [ ]
                                                 31    "Token"              [ ]
                                                 34    "Pattern"            [ ]
                                                 35    "Tree"               [ ]
                                                 37    "Subtree"            [ ]
                                                 38    "Attr"               [ ]
                                                 
  42        "Koshucode.Baala.Type.Judge"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 40    "JudgeClass"         [ "Base" | "Syntax" ]
                                                 41    "Judge"              [ "Base" | "Overture" | "Syntax" ]
                                                 42    "Interp"             [ "Base" | "Syntax" ]
                                                 
  47        "Koshucode.Baala.Type.Rel"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 44    "Order"              [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 44    "TermPicker"         [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 45    "Head"               [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ]
                                                 46    "Rel"                [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 47    "Mono"               [ "Base" ]
                                                 
  48        "Koshucode.Baala.Type"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Message"            [ "Base" | "Overture" ]
                                                 25    "Decimal"            [ ]
                                                 28    "Time"               [ ]
                                                 43    "Judge"              [ ]
                                                 44    "Type"               [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 48    "Rel"                [ ]
                                                 
  53        "Koshucode.Baala.Core.Lexmap"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Message"            [ "Base" ]
                                                 40    "Lexmap"             [ "Base" | "Overture" | "Syntax" ]
                                                 52    "LexmapTrees"        [ "Base" | "Syntax" ]
                                                 53    "Construct"          [ "Base" | "Overture" | "Syntax" ]
                                                 
  54        "Koshucode.Baala.Data.Class"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 22    "Message"            [ "Base" ]
                                                 50    "Edge"               [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 51    "Simple"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 52    "Complex"            [ "Base" | "Syntax" | "Type" ]
                                                 53    "Content"            [ "Base" | "Overture" | "Type" ]
                                                 53    "Map"                [ "Overture" ]
                                                 54    "Encode"             [ "Base" | "Syntax" | "Type" ]
                                                 
  57        "Koshucode.Baala.Data.Decode"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 40    "Message"            [ "Base" | "Syntax" ]
                                                 41    "Term"               [ "Base" | "Overture" | "Syntax" ]
                                                 50    "Type"               [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 51    "Numeric"            [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 56    "Content"            [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                 57    "Dataset"            [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                 
  60        "Koshucode.Baala.Data.Church"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 50    "Message"            [ "Base" | "Syntax" | "Type" ]
                                                 51    "Cox"                [ "Base" | "Overture" | "Syntax" ]
                                                 52    "Cop"                [ "Base" | "Overture" | "Syntax" ]
                                                 59    "Build"              [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ]
                                                 60    "Run"                [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                 
  61        "Koshucode.Baala.Data"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 51    "Message"            [ ]
                                                 55    "Class"              [ ]
                                                 58    "Decode"             [ ]
                                                 59    "Content"            [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                 61    "Church"             [ ]
                                                 
  63        "Koshucode.Baala.Core.Relkit"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 40    "Message"            [ "Base" | "Syntax" ]
                                                 55    "Relkit"             [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ]
                                                 56    "Construct"          [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 63    "Run"                [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 
  63        "Koshucode.Baala.Type.Type"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 56    "Genus"              [ "Class" | "Overture" ]
                                                 63    "Predefined where"   [ "Class" | "Data" ]
                                                 
  67        "Koshucode.Baala.Core.Relmap"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 50    "Message"            [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 50    "Result"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 63    "Option"             [ "Base" | "Data" | "Syntax" ]
                                                 65    "Relmap"             [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 66    "Rop"                [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                 66    "Specialize"         [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 67    "Construct"          [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 67    "Global"             [ "Base" | "Data" | "Overture" | "Type" ]
                                                 
  70        "Koshucode.Baala.Core.Assert"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 40    "Message"            [ "Base" | "Syntax" ]
                                                 63    "RelTable"           [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 69    "Assert"             [ "Base" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 70    "Run"                [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ]
                                                 
  74        "Koshucode.Baala.Core.Resource"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Message"            [ "Base" ]
                                                 55    "Clause"             [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ]
                                                 72    "Resource"           [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ]
                                                 73    "Concrete"           [ "Base" | "Data" | "Relmap" | "Syntax" | "Type" ]
                                                 73    "Include"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 73    "Run"                [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 74    "Read"               [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ]
                                                 
  75        "Koshucode.Baala.Core"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 52    "Message"            [ ]
                                                 54    "Lexmap"             [ ]
                                                 64    "Relkit"             [ ]
                                                 68    "Relmap"             [ ]
                                                 71    "Assert"             [ ]
                                                 75    "Resource"           [ ]
                                                 
  78        "Koshucode.Baala.Writer"             /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 77    "Csv"                [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 77    "Html"               [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 77    "Json"               [ "Core" | "Data" | "Syntax" | "Type" ]
                                                 77    "Judge"              [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ]
                                                 78    "Koshu"              [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ]
                                                 
  79        "Koshucode.Baala.Cop"                /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Replace"            [ ]
                                                 63    "Coxhand"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                 64    "Logic"              [ "Data" | "Overture" ]
                                                 78    "Arith"              [ "Base" | "Data" | "Overture" | "Type" ]
                                                 78    "List"               [ "Base" | "Data" | "Overture" | "Type" ]
                                                 78    "Misc"               [ "Base" | "Data" | "Syntax" ]
                                                 78    "Order"              [ "Data" | "Overture" ]
                                                 78    "Text"               [ "Base" | "Data" | "Overture" | "Subtext" | "Syntax" | "Type" ]
                                                 78    "Time"               [ "Data" | "Overture" | "Type" ]
                                                 78    "Type"               [ "Data" | "Overture" ]
                                                 79    "Bundle"             [ "Base" | "Data" ]
                                                 
  79        "Koshucode.Baala.Rop.Base"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 77    "Message"            [ "Core" ]
                                                 78    "Define"             [ "Core" ]
                                                 79    "Rops"               [ "Core" ]
                                                 
  80        "Koshucode.Baala"                    /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 1     "System"             [ ]
                                                 5     "Overture"           [ "Text" ]
                                                 20    "Base"               [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ]
                                                 25    "Subtext"            [ "Operator" | "Sieve" ]
                                                 39    "Syntax"             [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" | "Tree" ]
                                                 49    "Type"               [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ]
                                                 62    "Data"               [ "Church" | "Class" | "Decode" | "Type" ]
                                                 63    "DataPlus"           [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 76    "Core"               [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                 79    "Writer"             [ ]
                                                 80    "Cop"                [ ]
                                                 
  80        "Koshucode.Baala.Rop.Base.Get"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 78    "Get"                [ "Core" ]
                                                 79    "Rel"                [ "Core" ]
                                                 80    "Cox"                [ "Core" ]
                                                 
  81        "Koshucode.Baala.Toolkit.Main"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 64    "KoshuRdf"           [ "Base" | "Data" | "Syntax" | "Type" ]
                                                 77    "KoshuSyntax"        [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 81    "KoshuChange"        [ "Base" ]
                                                 81    "KoshuFilter"        [ "Base" | "Core" | "Data" | "Type" ]
                                                 81    "KoshuMain"          [ "Base" | "Core" | "Data" | "Overture" | "Type" | "Writer" ]
                                                 
  82        "Koshucode.Baala.Rop.Cox.Type"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 82    "Clock"              [ "Base" | "Core" ]
                                                 82    "Dec"                [ "Base" | "Core" ]
                                                 
  84        "Koshucode.Baala.Rop.Flat.Applied"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 64    "PoScale"            [ ]
                                                 82    "Gadget"             [ "Base" | "Core" ]
                                                 82    "Subtext"            [ "Base" | "Core" | "Subtext" ]
                                                 83    "Peripheral"         [ "Base" | "Core" ]
                                                 84    "Rops"               [ "Core" ]
                                                 
  84        "Koshucode.Baala.Rop.Flat.Lattice"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 82    "Tropashko"          [ "Base" | "Core" ]
                                                 83    "Restrict"           [ "Base" | "Core" ]
                                                 84    "Rop"                [ "Base" | "Core" ]
                                                 
  87        "Koshucode.Baala.Rop.Flat"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 82    "Elem"               [ "Base" | "Core" ]
                                                 82    "Meta"               [ "Base" | "Core" ]
                                                 82    "Order"              [ "Base" | "Core" ]
                                                 82    "Resource"           [ "Base" | "Core" ]
                                                 82    "Source"             [ "Base" | "Core" ]
                                                 82    "Term"               [ "Base" | "Core" ]
                                                 83    "TermGadget"         [ "Base" | "Core" ]
                                                 85    "Applied"            [ ]
                                                 85    "Lattice"            [ ]
                                                 86    "Check"              [ "Base" | "Core" | "Lattice" ]
                                                 86    "Control"            [ "Base" | "Core" | "Lattice" ]
                                                 87    "Rops"               [ "Applied" | "Base" | "Core" | "Lattice" ]
                                                 
  90        "Koshucode.Baala.Rop.Cox"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "GeoDatumJp"         [ ]
                                                 82    "Calc"               [ "Base" | "Core" ]
                                                 82    "Filter"             [ "Base" | "Core" ]
                                                 82    "Gadget"             [ "Base" | "Core" ]
                                                 82    "Range"              [ "Base" | "Core" ]
                                                 89    "Empty"              [ "Base" | "Core" | "Flat" ]
                                                 90    "Rops"               [ "Core" ]
                                                 
  91        "Koshucode.Baala.Rop.Nest"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 82    "Flow"               [ "Base" | "Core" ]
                                                 89    "Confl"              [ "Base" | "Core" | "Flat" ]
                                                 90    "Deriv"              [ "Base" | "Core" | "Flat" ]
                                                 91    "Rops"               [ "Base" | "Core" ]
                                                 
  92        "Koshucode.Baala.Rop"                /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 81    "Base"               [ ]
                                                 88    "Flat"               [ "Applied" | "Lattice" ]
                                                 91    "Cox"                [ ]
                                                 92    "Nest"               [ ]
                                                 
  93        "Koshucode.Baala.Toolkit.Library"    /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Version"            [ ]
                                                 63    "RDF"                [ "Data" | "Type" ]
                                                 77    "Element"            [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 77    "Input"              [ "Base" | "Core" | "Data" ]
                                                 80    "Change"             [ "Base" | "Data" | "Type" | "Writer" ]
                                                 80    "Run"                [ "Base" | "Core" | "Data" | "Writer" ]
                                                 93    "Global"             [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                 

=== rel

**
**  SUMMARY
**      52 judges on DIR-RANK
**      52 judges in total
**

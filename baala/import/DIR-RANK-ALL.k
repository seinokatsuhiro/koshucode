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
|-- DIR-RANK  /dir-rank 24  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 6 |
    "AngleText" | [ "Overture" ] ] [ 6 | "Term" | [ "Overture" ] ] [ 21 | "Message" | [ "Base" ] ] [ 22 | "Next" | [
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
    | [ "Base" | "Overture" | "Token" ] ] [ 34 | "Split" | [ "Base" | "Overture" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 36  /dir "Koshucode.Baala.Syntax.Subtree"  /base-rank {= /rank /base /import-dir [ 26 |
    "Subtree" | [ "Base" | "Overture" | "Subtext" | "Symbol" ] ] [ 27 | "DirTree" | [ "Base" | "Overture" ] ] [ 27 |
    "Xml" | [ "Base" | "Overture" | "Subtext" | "Symbol" ] ] [ 36 | "Decode" | [ "Base" | "Overture" | "Subtext" |
    "Symbol" | "Token" | "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 26 | "Message" |
    [ "Base" | "Overture" | "Para" | "Symbol" ] ] [ 36 | "AttrName" | [ "Overture" | "Tree" ] ] [ 37 | "Attr" | [ "Base"
    | "Overture" | "Para" | "Tree" ] ] [ 37 | "Slot" | [ "Base" | "Overture" | "Token" | "Tree" ] ] [ 38 | "AttrEd" | [
    "Base" | "Overture" | "Symbol" | "Token" | "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 39  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 24 | "Para" | [ ] ] [
    25 | "Symbol" | [ ] ] [ 27 | "Message" | [ ] ] [ 31 | "Token" | [ ] ] [ 34 | "Pattern" | [ ] ] [ 35 | "Tree" | [ ] ]
    [ 37 | "Subtree" | [ ] ] [ 39 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 43  /dir "Koshucode.Baala.Type.Judge"  /base-rank {= /rank /base /import-dir [ 41 | "JudgeClass"
    | [ "Base" | "Syntax" ] ] [ 42 | "Judge" | [ "Base" | "Overture" | "Syntax" ] ] [ 43 | "Interp" | [ "Base" |
    "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 48  /dir "Koshucode.Baala.Type.Rel"  /base-rank {= /rank /base /import-dir [ 45 | "Order" | [
    "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 45 | "TermPicker" | [ "Base" | "Judge" | "Overture" | "Syntax" ] ] [
    46 | "Head" | [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ] ] [ 47 | "Rel" | [ "Base" | "Judge" | "Overture"
    | "Syntax" ] ] [ 48 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 49  /dir "Koshucode.Baala.Type"  /base-rank {= /rank /base /import-dir [ 21 | "Message" | [
    "Base" | "Overture" ] ] [ 25 | "Decimal" | [ ] ] [ 28 | "Time" | [ ] ] [ 44 | "Judge" | [ ] ] [ 45 | "Type" | [
    "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 49 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 54  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 21 | "Message" |
    [ "Base" ] ] [ 41 | "Lexmap" | [ "Base" | "Overture" | "Syntax" ] ] [ 53 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [
    54 | "Construct" | [ "Base" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 55  /dir "Koshucode.Baala.Data.Class"  /base-rank {= /rank /base /import-dir [ 22 | "Message" |
    [ "Base" ] ] [ 51 | "Edge" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 52 | "Simple" | [ "Base" | "Overture" |
    "Syntax" | "Type" ] ] [ 53 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 54 | "Content" | [ "Base" | "Overture" |
    "Type" ] ] [ 54 | "Map" | [ "Overture" | "Syntax" ] ] [ 55 | "Encode" | [ "Base" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 58  /dir "Koshucode.Baala.Data.Decode"  /base-rank {= /rank /base /import-dir [ 21 | "Message" |
    [ "Base" | "Overture" ] ] [ 41 | "Term" | [ "Base" | "Overture" | "Syntax" ] ] [ 51 | "Type" | [ "Base" | "Overture"
    | "Syntax" | "Type" ] ] [ 52 | "Numeric" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 57 | "Content" | [ "Base"
    | "Class" | "Overture" | "Syntax" | "Type" ] ] [ 58 | "Dataset" | [ "Base" | "Class" | "Overture" | "Syntax" |
    "Type" ] ] =}

|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 51 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 52 | "Cox" | [ "Base" | "Overture" | "Syntax" ] ] [ 53 | "Cop" | [ "Base" |
    "Overture" | "Syntax" ] ] [ 60 | "Build" | [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ] ] [ 61 | "Run" |
    [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 62  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 52 | "Message" | [ ] ]
    [ 56 | "Class" | [ ] ] [ 59 | "Decode" | [ ] ] [ 60 | "Content" | [ "Base" | "Class" | "Decode" | "Overture" |
    "Syntax" | "Type" ] ] [ 62 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 64  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 41 | "Message" |
    [ "Base" | "Syntax" ] ] [ 56 | "Relkit" | [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ] ] [ 57 |
    "Construct" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 64 | "Run" | [ "Base" | "Data" | "Overture" | "Syntax"
    | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 64  /dir "Koshucode.Baala.Type.Type"  /base-rank {= /rank /base /import-dir [ 57 | "Genus" | [
    "Class" | "Overture" ] ] [ 64 | "Predefined where" | [ "Class" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 68  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 51 | "Message" |
    [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 51 | "Result" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 64 |
    "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 66 | "Relmap" | [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax"
    | "Type" ] ] [ 67 | "Rop" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 67 | "Specialize" | [ "Base" | "Lexmap" |
    "Overture" | "Relkit" | "Syntax" | "Type" ] ] [ 68 | "Construct" | [ "Base" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" | "Type" ] ] [ 68 | "Global" | [ "Base" | "Data" | "Overture" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 71  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 41 | "Message" |
    [ "Base" | "Syntax" ] ] [ 64 | "RelTable" | [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 70 | "Assert" |
    [ "Base" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ] ] [ 71 | "Run" | [ "Base" | "Data" | "Lexmap" |
    "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 75  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 21 | "Message"
    | [ "Base" | "Overture" ] ] [ 56 | "Clause" | [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ] ] [ 73 |
    "Resource" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ] ] [ 74
    | "Concrete" | [ "Base" | "Data" | "Relmap" | "Syntax" | "Type" ] ] [ 74 | "Include" | [ "Assert" | "Base" | "Data"
    | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ] ] [ 74 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" |
    "Overture" | "Relmap" | "Syntax" | "Type" ] ] [ 75 | "Read" | [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ]
    ] =}
|-- DIR-RANK  /dir-rank 76  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 53 | "Message" | [ ] ]
    [ 55 | "Lexmap" | [ ] ] [ 65 | "Relkit" | [ ] ] [ 69 | "Relmap" | [ ] ] [ 72 | "Assert" | [ ] ] [ 76 | "Resource" |
    [ ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 78 | "Csv" | [ "Base"
    | "Core" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 78 | "Html" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 78 | "Json" | [ "Core" | "Data" | "Syntax" | "Type" ] ] [ 78 | "Judge" | [ "Base" | "Core" |
    "Overture" | "Syntax" | "Type" ] ] [ 79 | "Koshu" | [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    64 | "Coxhand" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 65 | "Logic" | [ "Data" | "Overture" ] ] [ 79 |
    "Arith" | [ "Base" | "Data" | "Overture" | "Type" ] ] [ 79 | "List" | [ "Base" | "Data" | "Overture" | "Type" ] ] [
    79 | "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 79 | "Order" | [ "Data" | "Overture" ] ] [ 79 | "Text" | [ "Base" |
    "Data" | "Overture" | "Subtext" | "Syntax" | "Type" ] ] [ 79 | "Time" | [ "Data" | "Overture" | "Type" ] ] [ 79 |
    "Type" | [ "Data" | "Overture" ] ] [ 80 | "Bundle" | [ "Base" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 78 | "Message" | [
    "Core" ] ] [ 79 | "Define" | [ "Core" ] ] [ 80 | "Rops" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 1 | "System" | [ ] ] [ 5 |
    "Overture" | [ "Text" ] ] [ 20 | "Base" | [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ] ] [
    25 | "Subtext" | [ "Operator" | "Sieve" ] ] [ 40 | "Syntax" | [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" |
    "Tree" ] ] [ 50 | "Type" | [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ] ] [ 63 | "Data" | [ "Church" | "Class"
    | "Decode" | "Type" ] ] [ 64 | "DataPlus" | [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 77 | "Core" | [
    "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 80 | "Writer" | [ ] ] [ 81 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 81  /dir "Koshucode.Baala.Rop.Base.Get"  /base-rank {= /rank /base /import-dir [ 79 | "Get" | [
    "Core" ] ] [ 80 | "Rel" | [ "Core" ] ] [ 81 | "Cox" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 82  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 65 | "KoshuRdf"
    | [ "Base" | "Data" | "Syntax" | "Type" ] ] [ 78 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 82 | "KoshuChange" | [ "Base" ] ] [ 82 | "KoshuFilter" | [ "Base" | "Core" | "Data" | "Type"
    ] ] [ 82 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Overture" | "Type" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 83  /dir "Koshucode.Baala.Rop.Cox.Type"  /base-rank {= /rank /base /import-dir [ 83 | "Clock" |
    [ "Base" | "Core" ] ] [ 83 | "Dec" | [ "Base" | "Core" ] ] [ 83 | "Time" | [ "Base" | "Core" ] ] =}

|-- DIR-RANK  /dir-rank 85  /dir "Koshucode.Baala.Rop.Flat.Applied"  /base-rank {= /rank /base /import-dir [ 65 |
    "PoScale" | [ ] ] [ 83 | "Gadget" | [ "Base" | "Core" ] ] [ 83 | "Subtext" | [ "Base" | "Core" | "Subtext" ] ] [ 84
    | "Peripheral" | [ "Base" | "Core" ] ] [ 85 | "Rops" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 85  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 83 |
    "Tropashko" | [ "Base" | "Core" ] ] [ 84 | "Restrict" | [ "Base" | "Core" ] ] [ 85 | "Rop" | [ "Base" | "Core" ] ]
    =}
|-- DIR-RANK  /dir-rank 88  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 83 | "Elem" | [
    "Base" | "Core" ] ] [ 83 | "Meta" | [ "Base" | "Core" ] ] [ 83 | "Order" | [ "Base" | "Core" ] ] [ 83 | "Resource" |
    [ "Base" | "Core" ] ] [ 83 | "Source" | [ "Base" | "Core" ] ] [ 83 | "Term" | [ "Base" | "Core" ] ] [ 84 |
    "TermGadget" | [ "Base" | "Core" ] ] [ 86 | "Applied" | [ ] ] [ 86 | "Lattice" | [ ] ] [ 87 | "Check" | [ "Base" |
    "Core" | "Lattice" ] ] [ 87 | "Control" | [ "Base" | "Core" | "Lattice" ] ] [ 88 | "Rops" | [ "Applied" | "Base" |
    "Core" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 91  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 83 | "Calc" | [ "Base" | "Core" ] ] [ 83 | "Filter" | [ "Base" | "Core" ] ] [ 83 | "Gadget" | [ "Base" |
    "Core" ] ] [ 83 | "Range" | [ "Base" | "Core" ] ] [ 90 | "Empty" | [ "Base" | "Core" | "Flat" ] ] [ 91 | "Rops" | [
    "Core" ] ] =}
|-- DIR-RANK  /dir-rank 92  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 83 | "Flow" | [
    "Base" | "Core" ] ] [ 90 | "Confl" | [ "Base" | "Core" | "Flat" ] ] [ 91 | "Deriv" | [ "Base" | "Core" | "Flat" ] ]
    [ 92 | "Rops" | [ "Base" | "Core" ] ] =}
*** 50

|-- DIR-RANK  /dir-rank 93  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 82 | "Base" | [ ] ] [ 89
    | "Flat" | [ "Applied" | "Lattice" ] ] [ 92 | "Cox" | [ ] ] [ 93 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 94  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 64 | "RDF" | [ "Data" | "Type" ] ] [ 78 | "Element" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 78 | "Input" | [ "Base" | "Core" | "Data" ] ] [ 81 | "Change" | [ "Base" | "Data" | "Type" |
    "Writer" ] ] [ 81 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 94 | "Global" | [ "Base" | "Cop" | "Core" |
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
                                                 6     "AngleText"          [ "Overture" ]
                                                 6     "Term"               [ "Overture" ]
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
                                                 33    "Pattern"            [ "Base" | "Overture" | "Token" ]
                                                 34    "Split"              [ "Base" | "Overture" | "Token" ]
                                                 
  36        "Koshucode.Baala.Syntax.Subtree"     /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 26    "Subtree"            [ "Base" | "Overture" | "Subtext" | "Symbol" ]
                                                 27    "DirTree"            [ "Base" | "Overture" ]
                                                 27    "Xml"                [ "Base" | "Overture" | "Subtext" | "Symbol" ]
                                                 36    "Decode"             [ "Base" | "Overture" | "Subtext" | "Symbol" | "Token" | "Tree" ]
                                                 
  38        "Koshucode.Baala.Syntax.Attr"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 26    "Message"            [ "Base" | "Overture" | "Para" | "Symbol" ]
                                                 36    "AttrName"           [ "Overture" | "Tree" ]
                                                 37    "Attr"               [ "Base" | "Overture" | "Para" | "Tree" ]
                                                 37    "Slot"               [ "Base" | "Overture" | "Token" | "Tree" ]
                                                 38    "AttrEd"             [ "Base" | "Overture" | "Symbol" | "Token" | "Tree" ]
                                                 
  39        "Koshucode.Baala.Syntax"             /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 24    "Para"               [ ]
                                                 25    "Symbol"             [ ]
                                                 27    "Message"            [ ]
                                                 31    "Token"              [ ]
                                                 34    "Pattern"            [ ]
                                                 35    "Tree"               [ ]
                                                 37    "Subtree"            [ ]
                                                 39    "Attr"               [ ]
                                                 
  43        "Koshucode.Baala.Type.Judge"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 41    "JudgeClass"         [ "Base" | "Syntax" ]
                                                 42    "Judge"              [ "Base" | "Overture" | "Syntax" ]
                                                 43    "Interp"             [ "Base" | "Syntax" ]
                                                 
  48        "Koshucode.Baala.Type.Rel"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 45    "Order"              [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 45    "TermPicker"         [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 46    "Head"               [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ]
                                                 47    "Rel"                [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 48    "Mono"               [ "Base" ]
                                                 
  49        "Koshucode.Baala.Type"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Message"            [ "Base" | "Overture" ]
                                                 25    "Decimal"            [ ]
                                                 28    "Time"               [ ]
                                                 44    "Judge"              [ ]
                                                 45    "Type"               [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 49    "Rel"                [ ]
                                                 
  54        "Koshucode.Baala.Core.Lexmap"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Message"            [ "Base" ]
                                                 41    "Lexmap"             [ "Base" | "Overture" | "Syntax" ]
                                                 53    "LexmapTrees"        [ "Base" | "Syntax" ]
                                                 54    "Construct"          [ "Base" | "Overture" | "Syntax" ]
                                                 
  55        "Koshucode.Baala.Data.Class"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 22    "Message"            [ "Base" ]
                                                 51    "Edge"               [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 52    "Simple"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 53    "Complex"            [ "Base" | "Syntax" | "Type" ]
                                                 54    "Content"            [ "Base" | "Overture" | "Type" ]
                                                 54    "Map"                [ "Overture" | "Syntax" ]
                                                 55    "Encode"             [ "Base" | "Syntax" | "Type" ]
                                                 
  58        "Koshucode.Baala.Data.Decode"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Message"            [ "Base" | "Overture" ]
                                                 41    "Term"               [ "Base" | "Overture" | "Syntax" ]
                                                 51    "Type"               [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 52    "Numeric"            [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 57    "Content"            [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                 58    "Dataset"            [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                 
  61        "Koshucode.Baala.Data.Church"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 51    "Message"            [ "Base" | "Syntax" | "Type" ]
                                                 52    "Cox"                [ "Base" | "Overture" | "Syntax" ]
                                                 53    "Cop"                [ "Base" | "Overture" | "Syntax" ]
                                                 60    "Build"              [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ]
                                                 61    "Run"                [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                 
  62        "Koshucode.Baala.Data"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 52    "Message"            [ ]
                                                 56    "Class"              [ ]
                                                 59    "Decode"             [ ]
                                                 60    "Content"            [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                 62    "Church"             [ ]
                                                 
  64        "Koshucode.Baala.Core.Relkit"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 41    "Message"            [ "Base" | "Syntax" ]
                                                 56    "Relkit"             [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ]
                                                 57    "Construct"          [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 64    "Run"                [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 
  64        "Koshucode.Baala.Type.Type"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 57    "Genus"              [ "Class" | "Overture" ]
                                                 64    "Predefined where"   [ "Class" | "Data" ]
                                                 
  68        "Koshucode.Baala.Core.Relmap"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 51    "Message"            [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 51    "Result"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 64    "Option"             [ "Base" | "Data" | "Syntax" ]
                                                 66    "Relmap"             [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 67    "Rop"                [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                 67    "Specialize"         [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 68    "Construct"          [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 68    "Global"             [ "Base" | "Data" | "Overture" | "Type" ]
                                                 
  71        "Koshucode.Baala.Core.Assert"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 41    "Message"            [ "Base" | "Syntax" ]
                                                 64    "RelTable"           [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 70    "Assert"             [ "Base" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 71    "Run"                [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ]
                                                 
  75        "Koshucode.Baala.Core.Resource"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 21    "Message"            [ "Base" | "Overture" ]
                                                 56    "Clause"             [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ]
                                                 73    "Resource"           [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ]
                                                 74    "Concrete"           [ "Base" | "Data" | "Relmap" | "Syntax" | "Type" ]
                                                 74    "Include"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 74    "Run"                [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 75    "Read"               [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ]
                                                 
  76        "Koshucode.Baala.Core"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 53    "Message"            [ ]
                                                 55    "Lexmap"             [ ]
                                                 65    "Relkit"             [ ]
                                                 69    "Relmap"             [ ]
                                                 72    "Assert"             [ ]
                                                 76    "Resource"           [ ]
                                                 
  79        "Koshucode.Baala.Writer"             /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 78    "Csv"                [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 78    "Html"               [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 78    "Json"               [ "Core" | "Data" | "Syntax" | "Type" ]
                                                 78    "Judge"              [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ]
                                                 79    "Koshu"              [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ]
                                                 
  80        "Koshucode.Baala.Cop"                /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Replace"            [ ]
                                                 64    "Coxhand"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                 65    "Logic"              [ "Data" | "Overture" ]
                                                 79    "Arith"              [ "Base" | "Data" | "Overture" | "Type" ]
                                                 79    "List"               [ "Base" | "Data" | "Overture" | "Type" ]
                                                 79    "Misc"               [ "Base" | "Data" | "Syntax" ]
                                                 79    "Order"              [ "Data" | "Overture" ]
                                                 79    "Text"               [ "Base" | "Data" | "Overture" | "Subtext" | "Syntax" | "Type" ]
                                                 79    "Time"               [ "Data" | "Overture" | "Type" ]
                                                 79    "Type"               [ "Data" | "Overture" ]
                                                 80    "Bundle"             [ "Base" | "Data" ]
                                                 
  80        "Koshucode.Baala.Rop.Base"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 78    "Message"            [ "Core" ]
                                                 79    "Define"             [ "Core" ]
                                                 80    "Rops"               [ "Core" ]
                                                 
  81        "Koshucode.Baala"                    /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 1     "System"             [ ]
                                                 5     "Overture"           [ "Text" ]
                                                 20    "Base"               [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ]
                                                 25    "Subtext"            [ "Operator" | "Sieve" ]
                                                 40    "Syntax"             [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" | "Tree" ]
                                                 50    "Type"               [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ]
                                                 63    "Data"               [ "Church" | "Class" | "Decode" | "Type" ]
                                                 64    "DataPlus"           [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 77    "Core"               [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                 80    "Writer"             [ ]
                                                 81    "Cop"                [ ]
                                                 
  81        "Koshucode.Baala.Rop.Base.Get"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 79    "Get"                [ "Core" ]
                                                 80    "Rel"                [ "Core" ]
                                                 81    "Cox"                [ "Core" ]
                                                 
  82        "Koshucode.Baala.Toolkit.Main"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 65    "KoshuRdf"           [ "Base" | "Data" | "Syntax" | "Type" ]
                                                 78    "KoshuSyntax"        [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 82    "KoshuChange"        [ "Base" ]
                                                 82    "KoshuFilter"        [ "Base" | "Core" | "Data" | "Type" ]
                                                 82    "KoshuMain"          [ "Base" | "Core" | "Data" | "Overture" | "Type" | "Writer" ]
                                                 
  83        "Koshucode.Baala.Rop.Cox.Type"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 83    "Clock"              [ "Base" | "Core" ]
                                                 83    "Dec"                [ "Base" | "Core" ]
                                                 83    "Time"               [ "Base" | "Core" ]
                                                 
  85        "Koshucode.Baala.Rop.Flat.Applied"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 65    "PoScale"            [ ]
                                                 83    "Gadget"             [ "Base" | "Core" ]
                                                 83    "Subtext"            [ "Base" | "Core" | "Subtext" ]
                                                 84    "Peripheral"         [ "Base" | "Core" ]
                                                 85    "Rops"               [ "Core" ]
                                                 
  85        "Koshucode.Baala.Rop.Flat.Lattice"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 83    "Tropashko"          [ "Base" | "Core" ]
                                                 84    "Restrict"           [ "Base" | "Core" ]
                                                 85    "Rop"                [ "Base" | "Core" ]
                                                 
  88        "Koshucode.Baala.Rop.Flat"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 83    "Elem"               [ "Base" | "Core" ]
                                                 83    "Meta"               [ "Base" | "Core" ]
                                                 83    "Order"              [ "Base" | "Core" ]
                                                 83    "Resource"           [ "Base" | "Core" ]
                                                 83    "Source"             [ "Base" | "Core" ]
                                                 83    "Term"               [ "Base" | "Core" ]
                                                 84    "TermGadget"         [ "Base" | "Core" ]
                                                 86    "Applied"            [ ]
                                                 86    "Lattice"            [ ]
                                                 87    "Check"              [ "Base" | "Core" | "Lattice" ]
                                                 87    "Control"            [ "Base" | "Core" | "Lattice" ]
                                                 88    "Rops"               [ "Applied" | "Base" | "Core" | "Lattice" ]
                                                 
  91        "Koshucode.Baala.Rop.Cox"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "GeoDatumJp"         [ ]
                                                 83    "Calc"               [ "Base" | "Core" ]
                                                 83    "Filter"             [ "Base" | "Core" ]
                                                 83    "Gadget"             [ "Base" | "Core" ]
                                                 83    "Range"              [ "Base" | "Core" ]
                                                 90    "Empty"              [ "Base" | "Core" | "Flat" ]
                                                 91    "Rops"               [ "Core" ]
                                                 
  92        "Koshucode.Baala.Rop.Nest"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 83    "Flow"               [ "Base" | "Core" ]
                                                 90    "Confl"              [ "Base" | "Core" | "Flat" ]
                                                 91    "Deriv"              [ "Base" | "Core" | "Flat" ]
                                                 92    "Rops"               [ "Base" | "Core" ]
                                                 
  93        "Koshucode.Baala.Rop"                /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 82    "Base"               [ ]
                                                 89    "Flat"               [ "Applied" | "Lattice" ]
                                                 92    "Cox"                [ ]
                                                 93    "Nest"               [ ]
                                                 
  94        "Koshucode.Baala.Toolkit.Library"    /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Version"            [ ]
                                                 64    "RDF"                [ "Data" | "Type" ]
                                                 78    "Element"            [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 78    "Input"              [ "Base" | "Core" | "Data" ]
                                                 81    "Change"             [ "Base" | "Data" | "Type" | "Writer" ]
                                                 81    "Run"                [ "Base" | "Core" | "Data" | "Writer" ]
                                                 94    "Global"             [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                 

=== rel

**
**  SUMMARY
**      52 judges on DIR-RANK
**      52 judges in total
**

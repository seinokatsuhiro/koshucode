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
|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Overture.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Category"
    | [ ] ] [ 0 | "Integer" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 1 | "Utility" | [ ] ] [ 2 | "Textual" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Overture"  /base-rank {= /rank /base /import-dir [ 0 | "Cache" | [ ] ]
    [ 0 | "Index" | [ ] ] [ 0 | "Some" | [ ] ] [ 0 | "Type" | [ ] ] [ 0 | "Value" | [ ] ] [ 1 | "Fn" | [ ] ] [ 1 |
    "Infix" | [ ] ] [ 1 | "Misc" | [ ] ] [ 1 | "Shorthand" | [ ] ] [ 3 | "Text" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Case" | [ ]
    ] [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Queue" | [ ] ] [ 5 | "Pair" | [ "Overture" ] ] =}

|-- DIR-RANK  /dir-rank 6  /dir "Koshucode.Baala.Subtext.Operator"  /base-rank {= /rank /base /import-dir [ 3 | "Basic"
    | [ ] ] [ 4 | "Combine" | [ ] ] [ 5 | "Repeat" | [ ] ] [ 6 | "Char" | [ "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Split" | [ ] ]
    [ 5 | "Assoc" | [ "Overture" ] ] [ 5 | "List" | [ "Overture" ] ] [ 5 | "Select" | [ "Overture" ] ] [ 5 | "Set" | [
    "Overture" ] ] [ 6 | "Picker" | [ "Overture" ] ] [ 7 | "Order" | [ "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.MixText"  /base-rank {= /rank /base /import-dir [ 0 | "LineBreak"
    | [ ] ] [ 5 | "MixText" | [ "Overture" ] ] [ 6 | "MixClass" | [ ] ] [ 6 | "MixEncode" | [ "Overture" ] ] [ 7 |
    "Deriv" | [ ] ] =}
|-- DIR-RANK  /dir-rank 9  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 5 | "Comment" | [
    "Overture" ] ] [ 5 | "Suffix" | [ "Overture" ] ] [ 5 | "TextTable" | [ "Overture" ] ] [ 7 | "PPrint" | [ "Overture"
    | "Prelude" ] ] [ 9 | "Dots" | [ "List" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 13  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 7 | "CodePos" | [
    "Overture" | "Prelude" ] ] [ 9 | "Reason" | [ "List" | "Overture" ] ] [ 11 | "Report" | [ "List" | "Overture" |
    "Text" ] ] [ 13 | "Message" | [ "Abort" ] ] =}

|-- DIR-RANK  /dir-rank 13  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 7 | "Encoding" | [
    "Prelude" ] ] [ 7 | "Http" | [ "Overture" | "Prelude" ] ] [ 9 | "IOPoint" | [ "List" | "Overture" | "Prelude" ] ] [
    13 | "BzFile" | [ "Abort" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala.Base.Code"  /base-rank {= /rank /base /import-dir [ 13 | "Message" | [
    "Abort" ] ] [ 14 | "Bracket" | [ "Abort" | "List" | "Overture" ] ] [ 15 | "Line" | [ "Abort" | "IO" | "Prelude" ] ]
    [ 15 | "Tree" | [ "Abort" | "Overture" ] ] [ 16 | "Clause" | [ "Abort" | "List" | "Prelude" ] ] [ 16 | "Infix" | [
    "Overture" | "Prelude" ] ] [ 16 | "Scan" | [ "Abort" | "IO" | "Overture" | "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 6 | "Prelude" | [ ] ] [
    8 | "List" | [ ] ] [ 8 | "MixText" | [ ] ] [ 10 | "Text" | [ ] ] [ 12 | "Abort" | [ ] ] [ 14 | "IO" | [ ] ] [ 14 |
    "Message" | [ ] ] [ 17 | "Code" | [ ] ] =}
|-- DIR-RANK  /dir-rank 20  /dir "Koshucode.Baala.Subtext.Sieve"  /base-rank {= /rank /base /import-dir [ 19 | "Token" |
    [ "Base" | "Overture" ] ] [ 20 | "Tree" | [ "Base" | "Operator" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 21  /dir "Koshucode.Baala.Subtext"  /base-rank {= /rank /base /import-dir [ 0 | "MinMax" | [ ] ]
    [ 2 | "Expr" | [ ] ] [ 7 | "Operator" | [ ] ] [ 8 | "Bundle" | [ "Operator" ] ] [ 9 | "Para" | [ "Operator" |
    "Overture" ] ] [ 10 | "Match" | [ "Operator" ] ] [ 21 | "Sieve" | [ ] ] =}

|-- DIR-RANK  /dir-rank 21  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 19 | "Para" | [
    "Base" | "Overture" ] ] [ 20 | "Get" | [ "Base" ] ] [ 20 | "ParaSpec" | [ "Base" | "Overture" ] ] [ 21 | "Parse" | [
    "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 22  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 5 |
    "AngleText" | [ "Overture" ] ] [ 5 | "Term" | [ "Overture" ] ] [ 19 | "Message" | [ "Base" ] ] [ 20 | "Next" | [
    "Base" | "Overture" ] ] [ 21 | "Symbol" | [ "Base" | "Overture" ] ] [ 22 | "Short" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 22  /dir "Koshucode.Baala.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 5 | "Rational"
    | [ "Overture" ] ] [ 19 | "Decimal" | [ "Base" | "Overture" ] ] [ 20 | "Fraction" | [ "Overture" ] ] [ 21 | "Coder"
    | [ "Base" | "Overture" ] ] [ 22 | "BinaryAb" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 25  /dir "Koshucode.Baala.Type.Time"  /base-rank {= /rank /base /import-dir [ 20 | "Parts" | [
    "Base" ] ] [ 21 | "Date" | [ "Base" | "Overture" ] ] [ 22 | "Clock" | [ "Base" | "Overture" ] ] [ 23 | "ClockCalc" |
    [ "Base" | "Overture" ] ] [ 24 | "Time" | [ "Base" | "Overture" ] ] [ 25 | "TimeCalc" | [ "Base" | "Overture" ] ] =}
|-- DIR-RANK  /dir-rank 28  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 19 | "Message"
    | [ "Base" ] ] [ 24 | "Token" | [ "Base" | "Symbol" ] ] [ 25 | "Clip" | [ "Base" | "Overture" | "Symbol" ] ] [ 25 |
    "Pattern" | [ ] ] [ 25 | "Utility" | [ "Base" | "Overture" | "Symbol" ] ] [ 26 | "Section" | [ "Base" | "Overture" |
    "Symbol" ] ] [ 27 | "Rel" | [ "Base" | "Overture" | "Symbol" ] ] [ 28 | "Clause" | [ "Base" | "Overture" | "Symbol"
    ] ] =}

|-- DIR-RANK  /dir-rank 32  /dir "Koshucode.Baala.Syntax.Tree"  /base-rank {= /rank /base /import-dir [ 30 | "Bracket" |
    [ "Base" | "Token" ] ] [ 31 | "Parse" | [ "Base" | "Overture" | "Token" ] ] [ 31 | "Pattern" | [ "Base" | "Token" ]
    ] [ 32 | "Split" | [ "Base" | "Overture" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 34  /dir "Koshucode.Baala.Syntax.Subtree"  /base-rank {= /rank /base /import-dir [ 23 | "Filter"
    | [ "Overture" | "Subtext" ] ] [ 24 | "Subtree" | [ "Base" | "Overture" | "Symbol" ] ] [ 25 | "DirTree" | [ "Base" |
    "Overture" ] ] [ 25 | "Xml" | [ "Base" | "Overture" | "Symbol" ] ] [ 34 | "Decode" | [ "Base" | "Overture" |
    "Symbol" | "Token" | "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 35  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" |
    [ ] ] [ 24 | "Message" | [ "Base" | "Para" | "Symbol" ] ] [ 34 | "Attr" | [ "Base" | "Overture" | "Para" | "Tree" ]
    ] [ 34 | "Slot" | [ "Base" | "Overture" | "Token" | "Tree" ] ] [ 35 | "AttrEd" | [ "Base" | "Overture" | "Symbol" |
    "Token" | "Tree" ] ] =}
|-- DIR-RANK  /dir-rank 36  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 22 | "Para" | [ ] ] [
    23 | "Symbol" | [ ] ] [ 25 | "Message" | [ ] ] [ 29 | "Token" | [ ] ] [ 32 | "Pattern" | [ ] ] [ 33 | "Tree" | [ ] ]
    [ 35 | "Subtree" | [ ] ] [ 36 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 40  /dir "Koshucode.Baala.Type.Judge"  /base-rank {= /rank /base /import-dir [ 38 | "JudgeClass"
    | [ "Base" | "Syntax" ] ] [ 39 | "Judge" | [ "Base" | "Overture" | "Syntax" ] ] [ 40 | "Interp" | [ "Base" |
    "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 45  /dir "Koshucode.Baala.Type.Rel"  /base-rank {= /rank /base /import-dir [ 42 | "Order" | [
    "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 42 | "TermPicker" | [ "Base" | "Judge" | "Overture" | "Syntax" ] ] [
    43 | "Head" | [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ] ] [ 44 | "Rel" | [ "Base" | "Judge" | "Overture"
    | "Syntax" ] ] [ 45 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 46  /dir "Koshucode.Baala.Type"  /base-rank {= /rank /base /import-dir [ 19 | "Message" | [
    "Base" | "Overture" ] ] [ 23 | "Decimal" | [ ] ] [ 26 | "Time" | [ ] ] [ 41 | "Judge" | [ ] ] [ 42 | "Type" | [
    "Base" | "Judge" | "Overture" | "Syntax" ] ] [ 46 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 51  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 19 | "Message" |
    [ "Base" ] ] [ 38 | "Lexmap" | [ "Base" | "Overture" | "Syntax" ] ] [ 50 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [
    51 | "Construct" | [ "Base" | "Overture" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 52  /dir "Koshucode.Baala.Data.Class"  /base-rank {= /rank /base /import-dir [ 20 | "Message" |
    [ "Base" ] ] [ 48 | "Edge" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 49 | "Simple" | [ "Base" | "Overture" |
    "Syntax" | "Type" ] ] [ 50 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 51 | "Content" | [ "Base" | "Overture" |
    "Type" ] ] [ 51 | "Map" | [ "Overture" ] ] [ 52 | "Encode" | [ "Base" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 55  /dir "Koshucode.Baala.Data.Decode"  /base-rank {= /rank /base /import-dir [ 38 | "Message" |
    [ "Base" | "Syntax" ] ] [ 39 | "Term" | [ "Base" | "Overture" | "Syntax" ] ] [ 48 | "Type" | [ "Base" | "Overture" |
    "Syntax" | "Type" ] ] [ 49 | "Numeric" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 54 | "Content" | [ "Base" |
    "Class" | "Overture" | "Syntax" | "Type" ] ] [ 55 | "Dataset" | [ "Base" | "Class" | "Overture" | "Syntax" | "Type"
    ] ] =}

|-- DIR-RANK  /dir-rank 58  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 48 | "Message" |
    [ "Base" | "Syntax" | "Type" ] ] [ 49 | "Cox" | [ "Base" | "Overture" | "Syntax" ] ] [ 50 | "Cop" | [ "Base" |
    "Overture" | "Syntax" ] ] [ 57 | "Build" | [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ] ] [ 58 | "Run" |
    [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 49 | "Message" | [ ] ]
    [ 53 | "Class" | [ ] ] [ 56 | "Decode" | [ ] ] [ 57 | "Content" | [ "Base" | "Class" | "Decode" | "Overture" |
    "Syntax" | "Type" ] ] [ 59 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 38 | "Message" |
    [ "Base" | "Syntax" ] ] [ 53 | "Relkit" | [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ] ] [ 54 |
    "Construct" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 61 | "Run" | [ "Base" | "Data" | "Overture" | "Syntax"
    | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Type.Type"  /base-rank {= /rank /base /import-dir [ 54 | "Genus" | [
    "Class" | "Overture" ] ] [ 61 | "Predefined where" | [ "Class" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 65  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 48 | "Message" |
    [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 48 | "Result" | [ "Base" | "Overture" | "Syntax" | "Type" ] ] [ 61 |
    "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 63 | "Relmap" | [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax"
    | "Type" ] ] [ 64 | "Rop" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 64 | "Specialize" | [ "Base" | "Lexmap" |
    "Overture" | "Relkit" | "Syntax" | "Type" ] ] [ 65 | "Construct" | [ "Base" | "Lexmap" | "Overture" | "Relkit" |
    "Syntax" | "Type" ] ] [ 65 | "Global" | [ "Base" | "Data" | "Overture" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 68  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 38 | "Message" |
    [ "Base" | "Syntax" ] ] [ 61 | "RelTable" | [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 67 | "Assert" |
    [ "Base" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ] ] [ 68 | "Run" | [ "Base" | "Data" | "Lexmap" |
    "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 72  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 19 | "Message"
    | [ "Base" ] ] [ 53 | "Clause" | [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ] ] [ 70 | "Resource" | [
    "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ] ] [ 71 | "Concrete" |
    [ "Base" | "Data" | "Relmap" | "Syntax" | "Type" ] ] [ 71 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" |
    "Overture" | "Relmap" | "Syntax" | "Type" ] ] [ 71 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" |
    "Relmap" | "Syntax" | "Type" ] ] [ 72 | "Read" | [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 73  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 50 | "Message" | [ ] ]
    [ 52 | "Lexmap" | [ ] ] [ 62 | "Relkit" | [ ] ] [ 66 | "Relmap" | [ ] ] [ 69 | "Assert" | [ ] ] [ 73 | "Resource" |
    [ ] ] =}
|-- DIR-RANK  /dir-rank 76  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 75 | "Csv" | [ "Base"
    | "Core" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 75 | "Html" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 75 | "Json" | [ "Core" | "Data" | "Syntax" | "Type" ] ] [ 75 | "Judge" | [ "Base" | "Core" |
    "Overture" | "Syntax" | "Type" ] ] [ 76 | "Koshu" | [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 77  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [
    61 | "Coxhand" | [ "Base" | "Data" | "Overture" | "Syntax" ] ] [ 62 | "Logic" | [ "Data" | "Overture" ] ] [ 76 |
    "Arith" | [ "Base" | "Data" | "Overture" | "Type" ] ] [ 76 | "List" | [ "Base" | "Data" | "Overture" | "Type" ] ] [
    76 | "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 76 | "Order" | [ "Data" | "Overture" ] ] [ 76 | "Text" | [ "Base" |
    "Data" | "Overture" | "Subtext" | "Syntax" | "Type" ] ] [ 76 | "Time" | [ "Data" | "Overture" | "Type" ] ] [ 76 |
    "Type" | [ "Data" | "Overture" ] ] [ 77 | "Bundle" | [ "Base" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 77  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 75 | "Message" | [
    "Core" ] ] [ 76 | "Define" | [ "Core" ] ] [ 77 | "Rops" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 1 | "System" | [ ] ] [ 4 |
    "Overture" | [ "Text" ] ] [ 18 | "Base" | [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ] ] [
    22 | "Subtext" | [ "Operator" | "Sieve" ] ] [ 37 | "Syntax" | [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" |
    "Tree" ] ] [ 47 | "Type" | [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ] ] [ 60 | "Data" | [ "Church" | "Class"
    | "Decode" | "Type" ] ] [ 61 | "DataPlus" | [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ] ] [ 74 | "Core" | [
    "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 77 | "Writer" | [ ] ] [ 78 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala.Rop.Base.Get"  /base-rank {= /rank /base /import-dir [ 76 | "Get" | [
    "Core" ] ] [ 77 | "Rel" | [ "Core" ] ] [ 78 | "Cox" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 62 | "KoshuRdf"
    | [ "Base" | "Data" | "Syntax" | "Type" ] ] [ 75 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 79 | "KoshuChange" | [ "Base" ] ] [ 79 | "KoshuFilter" | [ "Base" | "Core" | "Data" | "Type"
    ] ] [ 79 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Overture" | "Type" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Rop.Cox.Type"  /base-rank {= /rank /base /import-dir [ 80 | "Clock" |
    [ "Base" | "Core" ] ] [ 80 | "Dec" | [ "Base" | "Core" ] ] =}

|-- DIR-RANK  /dir-rank 82  /dir "Koshucode.Baala.Rop.Flat.Applied"  /base-rank {= /rank /base /import-dir [ 62 |
    "PoScale" | [ ] ] [ 80 | "Gadget" | [ "Base" | "Core" ] ] [ 80 | "Subtext" | [ "Base" | "Core" | "Subtext" ] ] [ 81
    | "Peripheral" | [ "Base" | "Core" ] ] [ 82 | "Rops" | [ "Core" ] ] =}
|-- DIR-RANK  /dir-rank 82  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 80 |
    "Tropashko" | [ "Base" | "Core" ] ] [ 81 | "Restrict" | [ "Base" | "Core" ] ] [ 82 | "Rop" | [ "Base" | "Core" ] ]
    =}
|-- DIR-RANK  /dir-rank 85  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 80 | "Elem" | [
    "Base" | "Core" ] ] [ 80 | "Meta" | [ "Base" | "Core" ] ] [ 80 | "Order" | [ "Base" | "Core" ] ] [ 80 | "Resource" |
    [ "Base" | "Core" ] ] [ 80 | "Source" | [ "Base" | "Core" ] ] [ 80 | "Term" | [ "Base" | "Core" ] ] [ 81 |
    "TermGadget" | [ "Base" | "Core" ] ] [ 83 | "Applied" | [ ] ] [ 83 | "Lattice" | [ ] ] [ 84 | "Check" | [ "Base" |
    "Core" | "Lattice" ] ] [ 84 | "Control" | [ "Base" | "Core" | "Lattice" ] ] [ 85 | "Rops" | [ "Applied" | "Base" |
    "Core" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 88  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [
    ] ] [ 80 | "Calc" | [ "Base" | "Core" ] ] [ 80 | "Filter" | [ "Base" | "Core" ] ] [ 80 | "Gadget" | [ "Base" |
    "Core" ] ] [ 80 | "Range" | [ "Base" | "Core" ] ] [ 87 | "Empty" | [ "Base" | "Core" | "Flat" ] ] [ 88 | "Rops" | [
    "Core" ] ] =}
|-- DIR-RANK  /dir-rank 89  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 80 | "Flow" | [
    "Base" | "Core" ] ] [ 87 | "Confl" | [ "Base" | "Core" | "Flat" ] ] [ 88 | "Deriv" | [ "Base" | "Core" | "Flat" ] ]
    [ 89 | "Rops" | [ "Base" | "Core" ] ] =}
*** 50

|-- DIR-RANK  /dir-rank 90  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 79 | "Base" | [ ] ] [ 86
    | "Flat" | [ "Applied" | "Lattice" ] ] [ 89 | "Cox" | [ ] ] [ 90 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 91  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 |
    "Version" | [ ] ] [ 61 | "RDF" | [ "Data" | "Type" ] ] [ 75 | "Element" | [ "Base" | "Core" | "Data" | "Overture" |
    "Syntax" | "Type" ] ] [ 75 | "Input" | [ "Base" | "Core" | "Data" ] ] [ 78 | "Change" | [ "Base" | "Data" | "Type" |
    "Writer" ] ] [ 78 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 91 | "Global" | [ "Base" | "Cop" | "Core" |
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
                                                 
  2         "Koshucode.Baala.Overture.Text"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Category"           [ ]
                                                 0     "Integer"            [ ]
                                                 0     "Unicode"            [ ]
                                                 1     "Utility"            [ ]
                                                 2     "Textual"            [ ]
                                                 
  3         "Koshucode.Baala.Overture"           /rank /base                /import-dir
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
                                                 3     "Text"               [ ]
                                                 
  5         "Koshucode.Baala.Base.Prelude"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Case"               [ ]
                                                 0     "Class"              [ ]
                                                 0     "Import"             [ ]
                                                 1     "Queue"              [ ]
                                                 5     "Pair"               [ "Overture" ]
                                                 
  6         "Koshucode.Baala.Subtext.Operator"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 3     "Basic"              [ ]
                                                 4     "Combine"            [ ]
                                                 5     "Repeat"             [ ]
                                                 6     "Char"               [ "Overture" ]
                                                 
  7         "Koshucode.Baala.Base.List"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Split"              [ ]
                                                 5     "Assoc"              [ "Overture" ]
                                                 5     "List"               [ "Overture" ]
                                                 5     "Select"             [ "Overture" ]
                                                 5     "Set"                [ "Overture" ]
                                                 6     "Picker"             [ "Overture" ]
                                                 7     "Order"              [ "Overture" | "Prelude" ]
                                                 
  7         "Koshucode.Baala.Base.MixText"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "LineBreak"          [ ]
                                                 5     "MixText"            [ "Overture" ]
                                                 6     "MixClass"           [ ]
                                                 6     "MixEncode"          [ "Overture" ]
                                                 7     "Deriv"              [ ]
                                                 
  9         "Koshucode.Baala.Base.Text"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 5     "Comment"            [ "Overture" ]
                                                 5     "Suffix"             [ "Overture" ]
                                                 5     "TextTable"          [ "Overture" ]
                                                 7     "PPrint"             [ "Overture" | "Prelude" ]
                                                 9     "Dots"               [ "List" | "Overture" ]
                                                 
  13        "Koshucode.Baala.Base.Abort"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 7     "CodePos"            [ "Overture" | "Prelude" ]
                                                 9     "Reason"             [ "List" | "Overture" ]
                                                 11    "Report"             [ "List" | "Overture" | "Text" ]
                                                 13    "Message"            [ "Abort" ]
                                                 
  13        "Koshucode.Baala.Base.IO"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 7     "Encoding"           [ "Prelude" ]
                                                 7     "Http"               [ "Overture" | "Prelude" ]
                                                 9     "IOPoint"            [ "List" | "Overture" | "Prelude" ]
                                                 13    "BzFile"             [ "Abort" | "Prelude" ]
                                                 
  16        "Koshucode.Baala.Base.Code"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 13    "Message"            [ "Abort" ]
                                                 14    "Bracket"            [ "Abort" | "List" | "Overture" ]
                                                 15    "Line"               [ "Abort" | "IO" | "Prelude" ]
                                                 15    "Tree"               [ "Abort" | "Overture" ]
                                                 16    "Clause"             [ "Abort" | "List" | "Prelude" ]
                                                 16    "Infix"              [ "Overture" | "Prelude" ]
                                                 16    "Scan"               [ "Abort" | "IO" | "Overture" | "Prelude" ]
                                                 
  17        "Koshucode.Baala.Base"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 6     "Prelude"            [ ]
                                                 8     "List"               [ ]
                                                 8     "MixText"            [ ]
                                                 10    "Text"               [ ]
                                                 12    "Abort"              [ ]
                                                 14    "IO"                 [ ]
                                                 14    "Message"            [ ]
                                                 17    "Code"               [ ]
                                                 
  20        "Koshucode.Baala.Subtext.Sieve"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 19    "Token"              [ "Base" | "Overture" ]
                                                 20    "Tree"               [ "Base" | "Operator" | "Overture" ]
                                                 
  21        "Koshucode.Baala.Subtext"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "MinMax"             [ ]
                                                 2     "Expr"               [ ]
                                                 7     "Operator"           [ ]
                                                 8     "Bundle"             [ "Operator" ]
                                                 9     "Para"               [ "Operator" | "Overture" ]
                                                 10    "Match"              [ "Operator" ]
                                                 21    "Sieve"              [ ]
                                                 
  21        "Koshucode.Baala.Syntax.Para"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 19    "Para"               [ "Base" | "Overture" ]
                                                 20    "Get"                [ "Base" ]
                                                 20    "ParaSpec"           [ "Base" | "Overture" ]
                                                 21    "Parse"              [ "Base" | "Overture" ]
                                                 
  22        "Koshucode.Baala.Syntax.Symbol"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 5     "AngleText"          [ "Overture" ]
                                                 5     "Term"               [ "Overture" ]
                                                 19    "Message"            [ "Base" ]
                                                 20    "Next"               [ "Base" | "Overture" ]
                                                 21    "Symbol"             [ "Base" | "Overture" ]
                                                 22    "Short"              [ "Base" | "Overture" ]
                                                 
  22        "Koshucode.Baala.Type.Decimal"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 5     "Rational"           [ "Overture" ]
                                                 19    "Decimal"            [ "Base" | "Overture" ]
                                                 20    "Fraction"           [ "Overture" ]
                                                 21    "Coder"              [ "Base" | "Overture" ]
                                                 22    "BinaryAb"           [ "Base" ]
                                                 
  25        "Koshucode.Baala.Type.Time"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 20    "Parts"              [ "Base" ]
                                                 21    "Date"               [ "Base" | "Overture" ]
                                                 22    "Clock"              [ "Base" | "Overture" ]
                                                 23    "ClockCalc"          [ "Base" | "Overture" ]
                                                 24    "Time"               [ "Base" | "Overture" ]
                                                 25    "TimeCalc"           [ "Base" | "Overture" ]
                                                 
  28        "Koshucode.Baala.Syntax.Token"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 19    "Message"            [ "Base" ]
                                                 24    "Token"              [ "Base" | "Symbol" ]
                                                 25    "Clip"               [ "Base" | "Overture" | "Symbol" ]
                                                 25    "Pattern"            [ ]
                                                 25    "Utility"            [ "Base" | "Overture" | "Symbol" ]
                                                 26    "Section"            [ "Base" | "Overture" | "Symbol" ]
                                                 27    "Rel"                [ "Base" | "Overture" | "Symbol" ]
                                                 28    "Clause"             [ "Base" | "Overture" | "Symbol" ]
                                                 
  32        "Koshucode.Baala.Syntax.Tree"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 30    "Bracket"            [ "Base" | "Token" ]
                                                 31    "Parse"              [ "Base" | "Overture" | "Token" ]
                                                 31    "Pattern"            [ "Base" | "Token" ]
                                                 32    "Split"              [ "Base" | "Overture" | "Token" ]
                                                 
  34        "Koshucode.Baala.Syntax.Subtree"     /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 23    "Filter"             [ "Overture" | "Subtext" ]
                                                 24    "Subtree"            [ "Base" | "Overture" | "Symbol" ]
                                                 25    "DirTree"            [ "Base" | "Overture" ]
                                                 25    "Xml"                [ "Base" | "Overture" | "Symbol" ]
                                                 34    "Decode"             [ "Base" | "Overture" | "Symbol" | "Token" | "Tree" ]
                                                 
  35        "Koshucode.Baala.Syntax.Attr"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "AttrName"           [ ]
                                                 24    "Message"            [ "Base" | "Para" | "Symbol" ]
                                                 34    "Attr"               [ "Base" | "Overture" | "Para" | "Tree" ]
                                                 34    "Slot"               [ "Base" | "Overture" | "Token" | "Tree" ]
                                                 35    "AttrEd"             [ "Base" | "Overture" | "Symbol" | "Token" | "Tree" ]
                                                 
  36        "Koshucode.Baala.Syntax"             /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 22    "Para"               [ ]
                                                 23    "Symbol"             [ ]
                                                 25    "Message"            [ ]
                                                 29    "Token"              [ ]
                                                 32    "Pattern"            [ ]
                                                 33    "Tree"               [ ]
                                                 35    "Subtree"            [ ]
                                                 36    "Attr"               [ ]
                                                 
  40        "Koshucode.Baala.Type.Judge"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 38    "JudgeClass"         [ "Base" | "Syntax" ]
                                                 39    "Judge"              [ "Base" | "Overture" | "Syntax" ]
                                                 40    "Interp"             [ "Base" | "Syntax" ]
                                                 
  45        "Koshucode.Baala.Type.Rel"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 42    "Order"              [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 42    "TermPicker"         [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 43    "Head"               [ "Base" | "Judge" | "Overture" | "Syntax" | "Type" ]
                                                 44    "Rel"                [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 45    "Mono"               [ "Base" ]
                                                 
  46        "Koshucode.Baala.Type"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 19    "Message"            [ "Base" | "Overture" ]
                                                 23    "Decimal"            [ ]
                                                 26    "Time"               [ ]
                                                 41    "Judge"              [ ]
                                                 42    "Type"               [ "Base" | "Judge" | "Overture" | "Syntax" ]
                                                 46    "Rel"                [ ]
                                                 
  51        "Koshucode.Baala.Core.Lexmap"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 19    "Message"            [ "Base" ]
                                                 38    "Lexmap"             [ "Base" | "Overture" | "Syntax" ]
                                                 50    "LexmapTrees"        [ "Base" | "Syntax" ]
                                                 51    "Construct"          [ "Base" | "Overture" | "Syntax" ]
                                                 
  52        "Koshucode.Baala.Data.Class"         /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 20    "Message"            [ "Base" ]
                                                 48    "Edge"               [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 49    "Simple"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 50    "Complex"            [ "Base" | "Syntax" | "Type" ]
                                                 51    "Content"            [ "Base" | "Overture" | "Type" ]
                                                 51    "Map"                [ "Overture" ]
                                                 52    "Encode"             [ "Base" | "Syntax" | "Type" ]
                                                 
  55        "Koshucode.Baala.Data.Decode"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 38    "Message"            [ "Base" | "Syntax" ]
                                                 39    "Term"               [ "Base" | "Overture" | "Syntax" ]
                                                 48    "Type"               [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 49    "Numeric"            [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 54    "Content"            [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                 55    "Dataset"            [ "Base" | "Class" | "Overture" | "Syntax" | "Type" ]
                                                 
  58        "Koshucode.Baala.Data.Church"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 48    "Message"            [ "Base" | "Syntax" | "Type" ]
                                                 49    "Cox"                [ "Base" | "Overture" | "Syntax" ]
                                                 50    "Cop"                [ "Base" | "Overture" | "Syntax" ]
                                                 57    "Build"              [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" ]
                                                 58    "Run"                [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                 
  59        "Koshucode.Baala.Data"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 49    "Message"            [ ]
                                                 53    "Class"              [ ]
                                                 56    "Decode"             [ ]
                                                 57    "Content"            [ "Base" | "Class" | "Decode" | "Overture" | "Syntax" | "Type" ]
                                                 59    "Church"             [ ]
                                                 
  61        "Koshucode.Baala.Core.Relkit"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 38    "Message"            [ "Base" | "Syntax" ]
                                                 53    "Relkit"             [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ]
                                                 54    "Construct"          [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 61    "Run"                [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 
  61        "Koshucode.Baala.Type.Type"          /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 54    "Genus"              [ "Class" | "Overture" ]
                                                 61    "Predefined where"   [ "Class" | "Data" ]
                                                 
  65        "Koshucode.Baala.Core.Relmap"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 48    "Message"            [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 48    "Result"             [ "Base" | "Overture" | "Syntax" | "Type" ]
                                                 61    "Option"             [ "Base" | "Data" | "Syntax" ]
                                                 63    "Relmap"             [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 64    "Rop"                [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                 64    "Specialize"         [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 65    "Construct"          [ "Base" | "Lexmap" | "Overture" | "Relkit" | "Syntax" | "Type" ]
                                                 65    "Global"             [ "Base" | "Data" | "Overture" | "Type" ]
                                                 
  68        "Koshucode.Baala.Core.Assert"        /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 38    "Message"            [ "Base" | "Syntax" ]
                                                 61    "RelTable"           [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 67    "Assert"             [ "Base" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 68    "Run"                [ "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ]
                                                 
  72        "Koshucode.Baala.Core.Resource"      /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 19    "Message"            [ "Base" ]
                                                 53    "Clause"             [ "Base" | "Lexmap" | "Overture" | "Syntax" | "Type" ]
                                                 70    "Resource"           [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relkit" | "Relmap" | "Syntax" | "Type" ]
                                                 71    "Concrete"           [ "Base" | "Data" | "Relmap" | "Syntax" | "Type" ]
                                                 71    "Include"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 71    "Run"                [ "Assert" | "Base" | "Data" | "Lexmap" | "Overture" | "Relmap" | "Syntax" | "Type" ]
                                                 72    "Read"               [ "Base" | "Data" | "Overture" | "Relmap" | "Syntax" ]
                                                 
  73        "Koshucode.Baala.Core"               /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 50    "Message"            [ ]
                                                 52    "Lexmap"             [ ]
                                                 62    "Relkit"             [ ]
                                                 66    "Relmap"             [ ]
                                                 69    "Assert"             [ ]
                                                 73    "Resource"           [ ]
                                                 
  76        "Koshucode.Baala.Writer"             /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 75    "Csv"                [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 75    "Html"               [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 75    "Json"               [ "Core" | "Data" | "Syntax" | "Type" ]
                                                 75    "Judge"              [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ]
                                                 76    "Koshu"              [ "Base" | "Core" | "Overture" | "Syntax" | "Type" ]
                                                 
  77        "Koshucode.Baala.Cop"                /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Replace"            [ ]
                                                 61    "Coxhand"            [ "Base" | "Data" | "Overture" | "Syntax" ]
                                                 62    "Logic"              [ "Data" | "Overture" ]
                                                 76    "Arith"              [ "Base" | "Data" | "Overture" | "Type" ]
                                                 76    "List"               [ "Base" | "Data" | "Overture" | "Type" ]
                                                 76    "Misc"               [ "Base" | "Data" | "Syntax" ]
                                                 76    "Order"              [ "Data" | "Overture" ]
                                                 76    "Text"               [ "Base" | "Data" | "Overture" | "Subtext" | "Syntax" | "Type" ]
                                                 76    "Time"               [ "Data" | "Overture" | "Type" ]
                                                 76    "Type"               [ "Data" | "Overture" ]
                                                 77    "Bundle"             [ "Base" | "Data" ]
                                                 
  77        "Koshucode.Baala.Rop.Base"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 75    "Message"            [ "Core" ]
                                                 76    "Define"             [ "Core" ]
                                                 77    "Rops"               [ "Core" ]
                                                 
  78        "Koshucode.Baala"                    /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 1     "System"             [ ]
                                                 4     "Overture"           [ "Text" ]
                                                 18    "Base"               [ "Abort" | "Code" | "IO" | "List" | "MixText" | "Prelude" | "Text" ]
                                                 22    "Subtext"            [ "Operator" | "Sieve" ]
                                                 37    "Syntax"             [ "Attr" | "Para" | "Subtree" | "Symbol" | "Token" | "Tree" ]
                                                 47    "Type"               [ "Decimal" | "Judge" | "Rel" | "Time" | "Type" ]
                                                 60    "Data"               [ "Church" | "Class" | "Decode" | "Type" ]
                                                 61    "DataPlus"           [ "Base" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 74    "Core"               [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                 77    "Writer"             [ ]
                                                 78    "Cop"                [ ]
                                                 
  78        "Koshucode.Baala.Rop.Base.Get"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 76    "Get"                [ "Core" ]
                                                 77    "Rel"                [ "Core" ]
                                                 78    "Cox"                [ "Core" ]
                                                 
  79        "Koshucode.Baala.Toolkit.Main"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 62    "KoshuRdf"           [ "Base" | "Data" | "Syntax" | "Type" ]
                                                 75    "KoshuSyntax"        [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 79    "KoshuChange"        [ "Base" ]
                                                 79    "KoshuFilter"        [ "Base" | "Core" | "Data" | "Type" ]
                                                 79    "KoshuMain"          [ "Base" | "Core" | "Data" | "Overture" | "Type" | "Writer" ]
                                                 
  80        "Koshucode.Baala.Rop.Cox.Type"       /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 80    "Clock"              [ "Base" | "Core" ]
                                                 80    "Dec"                [ "Base" | "Core" ]
                                                 
  82        "Koshucode.Baala.Rop.Flat.Applied"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 62    "PoScale"            [ ]
                                                 80    "Gadget"             [ "Base" | "Core" ]
                                                 80    "Subtext"            [ "Base" | "Core" | "Subtext" ]
                                                 81    "Peripheral"         [ "Base" | "Core" ]
                                                 82    "Rops"               [ "Core" ]
                                                 
  82        "Koshucode.Baala.Rop.Flat.Lattice"   /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 80    "Tropashko"          [ "Base" | "Core" ]
                                                 81    "Restrict"           [ "Base" | "Core" ]
                                                 82    "Rop"                [ "Base" | "Core" ]
                                                 
  85        "Koshucode.Baala.Rop.Flat"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 80    "Elem"               [ "Base" | "Core" ]
                                                 80    "Meta"               [ "Base" | "Core" ]
                                                 80    "Order"              [ "Base" | "Core" ]
                                                 80    "Resource"           [ "Base" | "Core" ]
                                                 80    "Source"             [ "Base" | "Core" ]
                                                 80    "Term"               [ "Base" | "Core" ]
                                                 81    "TermGadget"         [ "Base" | "Core" ]
                                                 83    "Applied"            [ ]
                                                 83    "Lattice"            [ ]
                                                 84    "Check"              [ "Base" | "Core" | "Lattice" ]
                                                 84    "Control"            [ "Base" | "Core" | "Lattice" ]
                                                 85    "Rops"               [ "Applied" | "Base" | "Core" | "Lattice" ]
                                                 
  88        "Koshucode.Baala.Rop.Cox"            /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "GeoDatumJp"         [ ]
                                                 80    "Calc"               [ "Base" | "Core" ]
                                                 80    "Filter"             [ "Base" | "Core" ]
                                                 80    "Gadget"             [ "Base" | "Core" ]
                                                 80    "Range"              [ "Base" | "Core" ]
                                                 87    "Empty"              [ "Base" | "Core" | "Flat" ]
                                                 88    "Rops"               [ "Core" ]
                                                 
  89        "Koshucode.Baala.Rop.Nest"           /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 80    "Flow"               [ "Base" | "Core" ]
                                                 87    "Confl"              [ "Base" | "Core" | "Flat" ]
                                                 88    "Deriv"              [ "Base" | "Core" | "Flat" ]
                                                 89    "Rops"               [ "Base" | "Core" ]
                                                 
  90        "Koshucode.Baala.Rop"                /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 79    "Base"               [ ]
                                                 86    "Flat"               [ "Applied" | "Lattice" ]
                                                 89    "Cox"                [ ]
                                                 90    "Nest"               [ ]
                                                 
  91        "Koshucode.Baala.Toolkit.Library"    /rank /base                /import-dir
                                                 ----- -------------------- --------------------------------------------------------------------------------------------------
                                                 0     "Version"            [ ]
                                                 61    "RDF"                [ "Data" | "Type" ]
                                                 75    "Element"            [ "Base" | "Core" | "Data" | "Overture" | "Syntax" | "Type" ]
                                                 75    "Input"              [ "Base" | "Core" | "Data" ]
                                                 78    "Change"             [ "Base" | "Data" | "Type" | "Writer" ]
                                                 78    "Run"                [ "Base" | "Core" | "Data" | "Writer" ]
                                                 91    "Global"             [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                 

=== rel

**
**  SUMMARY
**      52 judges on DIR-RANK
**      52 judges in total
**

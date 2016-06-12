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

|-- DIR-RANK  /dir-rank 1  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Case" | [ ] ] [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Pair" | [ ] ] [ 1 | "Queue" | [ ] ] =}
|-- DIR-RANK  /dir-rank 3  /dir "Koshucode.Baala.Base.IO"  /base-rank {= /rank /base /import-dir [ 3 | "BzFile" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 4  /dir "Koshucode.Baala.Base.List"  /base-rank {= /rank /base /import-dir [ 0 | "Split" | [ ] ] [ 1 | "Dots" | [ ] ] [ 3 | "Assoc" | [ "Prelude" ] ] [ 3 | "List" | [ "Prelude" ] ] [ 3 | "Snip" | [ "Prelude" ] ] [ 4 | "Order" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 7  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ] ] [ 0 | "LineBreak" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 3 | "Utility" | [ "Prelude" ] ] [ 4 | "Http" | [ "Prelude" ] ] [ 4 | "MixText" | [ ] ] [ 6 | "IOPoint" | [ "List" | "Prelude" ] ] [ 6 | "TextTable" | [ "List" | "Prelude" ] ] [ 6 | "Write" | [ "List" | "Prelude" ] ] [ 7 | "CodePt" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 9 | "Reason" | [ "Text" ] ] [ 10 | "Abortable" | [ "Prelude" | "Text" ] ] [ 10 | "Report" | [ "List" | "Prelude" | "Text" ] ] [ 12 | "Message" | [ "Abort" ] ] =}

|-- DIR-RANK  /dir-rank 14  /dir "Koshucode.Baala.Base.Syntax"  /base-rank {= /rank /base /import-dir [ 12 | "Message" | [ "Abort" | "Prelude" | "Text" ] ] [ 13 | "Line" | [ "Abort" | "Prelude" | "Text" ] ] [ 13 | "Tree" | [ "Abort" | "List" | "Prelude" | "Text" ] ] [ 14 | "Clause" | [ "List" | "Prelude" | "Text" ] ] [ 14 | "Infix" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 15  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 2 | "Prelude" | [ ] ] [ 4 | "IO" | [ ] ] [ 5 | "List" | [ ] ] [ 8 | "Text" | [ ] ] [ 11 | "Abort" | [ ] ] [ 13 | "Message" | [ ] ] [ 15 | "Syntax" | [ ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 17 | "Para" | [ "Base" ] ] [ 18 | "Get" | [ "Base" ] ] [ 18 | "ParaSpec" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 19  /dir "Koshucode.Baala.Data.Type.Time"  /base-rank {= /rank /base /import-dir [ 17 | "Clock" | [ "Base" ] ] [ 18 | "Date" | [ "Base" ] ] [ 19 | "Time" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 19  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 0 | "Term" | [ ] ] [ 17 | "AngleText" | [ "Base" ] ] [ 17 | "Message" | [ "Base" ] ] [ 18 | "Next" | [ "Base" ] ] [ 19 | "Short" | [ "Base" ] ] =}

|-- DIR-RANK  /dir-rank 20  /dir "Koshucode.Baala.Data.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 17 | "Decimal" | [ "Base" ] ] [ 17 | "Rational" | [ "Base" ] ] [ 18 | "Fraction" | [ "Base" ] ] [ 19 | "Coder" | [ "Base" ] ] [ 20 | "BinaryAb" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 24  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 17 | "Message" | [ "Base" ] ] [ 21 | "Token" | [ "Base" | "Symbol" ] ] [ 22 | "Pattern" | [ ] ] [ 22 | "Utility" | [ "Base" ] ] [ 23 | "TokenLine" | [ "Base" | "Symbol" ] ] [ 24 | "TokenClause" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 28  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 26 | "Bracket" | [ "Base" | "Token" ] ] [ 27 | "TokenTree" | [ "Base" | "Token" ] ] [ 28 | "Parse" | [ "Base" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 32  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" | [ ] ] [ 30 | "Message" | [ "Base" | "Para" | "Symbol" | "TTree" ] ] [ 31 | "Attr" | [ "Base" | "Para" | "TTree" ] ] [ 31 | "Slot" | [ "Base" | "TTree" | "Token" ] ] [ 32 | "AttrEd" | [ "Base" | "TTree" | "Token" ] ] [ 32 | "Parse" | [ "Base" | "Para" ] ] =}
|-- DIR-RANK  /dir-rank 33  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 19 | "Para" | [ ] ] [ 20 | "Symbol" | [ ] ] [ 25 | "Token" | [ ] ] [ 29 | "TTree" | [ ] ] [ 31 | "Message" | [ ] ] [ 33 | "Attr" | [ ] ] =}

|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 36 | "Head" | [ "Base" | "Syntax" ] ] [ 37 | "Rel" | [ "Base" | "Syntax" ] ] [ 38 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 39  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 17 | "Message" | [ "Base" ] ] [ 20 | "Time" | [ ] ] [ 21 | "Decimal" | [ ] ] [ 35 | "Interp" | [ "Base" | "Syntax" ] ] [ 35 | "Judge" | [ "Base" | "Syntax" ] ] [ 35 | "Type" | [ "Base" | "Syntax" ] ] [ 39 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 44  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 35 | "Lexmap" | [ "Base" | "Syntax" ] ] [ 43 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [ 43 | "Message" | [ "Base" | "Syntax" ] ] [ 44 | "Construct" | [ "Base" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 44  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 35 | "Message" | [ "Base" | "Syntax" ] ] [ 41 | "Singleton" | [ "Base" | "Type" ] ] [ 41 | "Tree" | [ "Base" | "Syntax" | "Type" ] ] [ 42 | "Complex" | [ "Base" | "Syntax" | "Type" ] ] [ 42 | "Simple" | [ "Base" | "Type" ] ] [ 43 | "Utility" | [ "Base" | "Syntax" | "Type" ] ] [ 44 | "BaalaC" | [ "Base" | "Syntax" | "Type" ] ] [ 44 | "Decode" | [ "Base" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 47  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 41 | "Message" | [ "Base" | "Syntax" | "Type" ] ] [ 42 | "Cox" | [ "Base" | "Syntax" ] ] [ 43 | "Cop" | [ "Base" | "Syntax" ] ] [ 46 | "Build" | [ "Base" | "Content" | "Syntax" ] ] [ 47 | "Run" | [ "Base" | "Content" | "Syntax" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 48  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 40 | "Type" | [ "Decimal" | "Rel" | "Time" ] ] [ 42 | "Message" | [ ] ] [ 45 | "Content" | [ ] ] [ 48 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 52  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 35 | "Message" | [ "Base" | "Syntax" ] ] [ 50 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 51 | "Construct" | [ "Base" | "Data" | "Syntax" ] ] [ 52 | "Run" | [ "Base" | "Data" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 56  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 50 | "Message" | [ "Base" | "Data" | "Syntax" ] ] [ 50 | "Result" | [ "Base" | "Data" | "Syntax" ] ] [ 51 | "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 54 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 55 | "Rop" | [ "Base" | "Lexmap" | "Syntax" ] ] [ 55 | "Specialize" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 56 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 56 | "Global" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 35 | "Message" | [ "Base" | "Syntax" ] ] [ 50 | "RelTable" | [ "Base" | "Data" | "Syntax" ] ] [ 54 | "Dataset" | [ "Base" | "Data" | "Relkit" | "Syntax" ] ] [ 58 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 59 | "Run" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 63  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 17 | "Message" | [ "Base" ] ] [ 50 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 61 | "Resource" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] [ 62 | "Concrete" | [ "Base" | "Data" | "Relmap" | "Syntax" ] ] [ 62 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 62 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 63 | "Read" | [ "Base" | "Data" | "Relmap" | "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 64  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 45 | "Lexmap" | [ ] ] [ 51 | "Message" | [ ] ] [ 53 | "Relkit" | [ ] ] [ 57 | "Relmap" | [ ] ] [ 60 | "Assert" | [ ] ] [ 64 | "Resource" | [ ] ] =}
|-- DIR-RANK  /dir-rank 67  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 52 | "Message" | [ "Base" ] ] [ 53 | "Term" | [ "Base" | "Data" | "Syntax" ] ] [ 66 | "Define" | [ "Core" | "Syntax" ] ] [ 66 | "Get" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 67 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 67  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 66 | "Csv" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "Html" | [ "Base" | "Core" | "Syntax" ] ] [ 66 | "Json" | [ "Core" | "Data" | "Syntax" ] ] [ 66 | "Judge" | [ "Base" | "Core" | "Data" ] ] [ 67 | "Koshu" | [ "Base" | "Core" | "Data" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [ 50 | "Coxhand" | [ "Base" | "Data" | "Syntax" ] ] [ 51 | "Logic" | [ "Base" | "Data" ] ] [ 67 | "Message" | [ ] ] [ 68 | "Arith" | [ "Base" | "Data" ] ] [ 68 | "List" | [ "Base" | "Data" ] ] [ 68 | "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 68 | "Order" | [ "Base" | "Data" ] ] [ 68 | "Text" | [ "Base" | "Data" | "Syntax" ] ] [ 68 | "Time" | [ "Base" | "Data" ] ] [ 68 | "Type" | [ "Base" | "Data" ] ] [ 69 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 16 | "Base" | [ "Abort" | "List" | "Prelude" | "Syntax" | "Text" ] ] [ 34 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ] ] [ 49 | "Data" | [ "Church" | "Content" | "Type" ] ] [ 65 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 68 | "Writer" | [ ] ] [ 70 | "Cop" | [ ] ] =}

|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 67 | "KoshuRdf" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 67 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "KoshuChange" | [ "Base" ] ] [ 70 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 70 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 71  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 69 | "Tropashko" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Restrict" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 71 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 74  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 17 | "PoScale" | [ "Base" ] ] [ 66 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "Elem" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "Meta" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "Order" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "Resource" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "Source" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 69 | "Term" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Peripheral" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "TermGadget" | [ "Base" | "Core" | "Data" ] ] [ 72 | "Lattice" | [ ] ] [ 73 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ] ] [ 73 | "Control" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 74 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 77  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [ ] ] [ 67 | "Message" | [ ] ] [ 69 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Accessor" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Filter" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 70 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 70 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 76 | "Empty" | [ "Base" | "Base" | "Core" | "Data" | "Flat" ] ] [ 77 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 78  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 67 | "Message" | [ "Base" | "Data" | "Syntax" ] ] [ 69 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 76 | "Confl" | [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ] ] [ 77 | "Deriv" | [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ] ] [ 78 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 79  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 68 | "Base" | [ ] ] [ 75 | "Flat" | [ "Lattice" ] ] [ 78 | "Cox" | [ ] ] [ 79 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 80  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" | [ ] ] [ 0 | "Version" | [ ] ] [ 50 | "RDF" | [ "Data" ] ] [ 66 | "Element" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "Exit" | [ "Base" | "Core" | "Data" ] ] [ 66 | "Input" | [ "Base" | "Core" ] ] [ 69 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 69 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 80 | "Global" | [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ] ] =}

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
                                                  
  3         "Koshucode.Baala.Base.IO"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  3     "BzFile"         [ "Prelude" ]
                                                  
  4         "Koshucode.Baala.Base.List"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Split"          [ ]
                                                  1     "Dots"           [ ]
                                                  3     "Assoc"          [ "Prelude" ]
                                                  3     "List"           [ "Prelude" ]
                                                  3     "Snip"           [ "Prelude" ]
                                                  4     "Order"          [ "Prelude" ]
                                                  
  7         "Koshucode.Baala.Base.Text"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Comment"        [ ]
                                                  0     "LineBreak"      [ ]
                                                  0     "Unicode"        [ ]
                                                  3     "Utility"        [ "Prelude" ]
                                                  4     "Http"           [ "Prelude" ]
                                                  4     "MixText"        [ ]
                                                  6     "IOPoint"        [ "List" | "Prelude" ]
                                                  6     "TextTable"      [ "List" | "Prelude" ]
                                                  6     "Write"          [ "List" | "Prelude" ]
                                                  7     "CodePt"         [ "Prelude" ]
                                                  
  12        "Koshucode.Baala.Base.Abort"          /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  9     "Reason"         [ "Text" ]
                                                  10    "Abortable"      [ "Prelude" | "Text" ]
                                                  10    "Report"         [ "List" | "Prelude" | "Text" ]
                                                  12    "Message"        [ "Abort" ]
                                                  
  14        "Koshucode.Baala.Base.Syntax"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  12    "Message"        [ "Abort" | "Prelude" | "Text" ]
                                                  13    "Line"           [ "Abort" | "Prelude" | "Text" ]
                                                  13    "Tree"           [ "Abort" | "List" | "Prelude" | "Text" ]
                                                  14    "Clause"         [ "List" | "Prelude" | "Text" ]
                                                  14    "Infix"          [ "Prelude" ]
                                                  
  15        "Koshucode.Baala.Base"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  2     "Prelude"        [ ]
                                                  4     "IO"             [ ]
                                                  5     "List"           [ ]
                                                  8     "Text"           [ ]
                                                  11    "Abort"          [ ]
                                                  13    "Message"        [ ]
                                                  15    "Syntax"         [ ]
                                                  
  18        "Koshucode.Baala.Syntax.Para"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Para"           [ "Base" ]
                                                  18    "Get"            [ "Base" ]
                                                  18    "ParaSpec"       [ "Base" ]
                                                  
  19        "Koshucode.Baala.Data.Type.Time"      /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Clock"          [ "Base" ]
                                                  18    "Date"           [ "Base" ]
                                                  19    "Time"           [ "Base" ]
                                                  
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
                                                  
  24        "Koshucode.Baala.Syntax.Token"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  21    "Token"          [ "Base" | "Symbol" ]
                                                  22    "Pattern"        [ ]
                                                  22    "Utility"        [ "Base" ]
                                                  23    "TokenLine"      [ "Base" | "Symbol" ]
                                                  24    "TokenClause"    [ "Base" ]
                                                  
  28        "Koshucode.Baala.Syntax.TTree"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  26    "Bracket"        [ "Base" | "Token" ]
                                                  27    "TokenTree"      [ "Base" | "Token" ]
                                                  28    "Parse"          [ "Base" | "Token" ]
                                                  
  32        "Koshucode.Baala.Syntax.Attr"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "AttrName"       [ ]
                                                  30    "Message"        [ "Base" | "Para" | "Symbol" | "TTree" ]
                                                  31    "Attr"           [ "Base" | "Para" | "TTree" ]
                                                  31    "Slot"           [ "Base" | "TTree" | "Token" ]
                                                  32    "AttrEd"         [ "Base" | "TTree" | "Token" ]
                                                  32    "Parse"          [ "Base" | "Para" ]
                                                  
  33        "Koshucode.Baala.Syntax"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  19    "Para"           [ ]
                                                  20    "Symbol"         [ ]
                                                  25    "Token"          [ ]
                                                  29    "TTree"          [ ]
                                                  31    "Message"        [ ]
                                                  33    "Attr"           [ ]
                                                  
  38        "Koshucode.Baala.Data.Type.Rel"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  36    "Head"           [ "Base" | "Syntax" ]
                                                  37    "Rel"            [ "Base" | "Syntax" ]
                                                  38    "Mono"           [ "Base" ]
                                                  
  39        "Koshucode.Baala.Data.Type"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  20    "Time"           [ ]
                                                  21    "Decimal"        [ ]
                                                  35    "Interp"         [ "Base" | "Syntax" ]
                                                  35    "Judge"          [ "Base" | "Syntax" ]
                                                  35    "Type"           [ "Base" | "Syntax" ]
                                                  39    "Rel"            [ ]
                                                  
  44        "Koshucode.Baala.Core.Lexmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  35    "Lexmap"         [ "Base" | "Syntax" ]
                                                  43    "LexmapTrees"    [ "Base" | "Syntax" ]
                                                  43    "Message"        [ "Base" | "Syntax" ]
                                                  44    "Construct"      [ "Base" | "Syntax" ]
                                                  
  44        "Koshucode.Baala.Data.Content"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  35    "Message"        [ "Base" | "Syntax" ]
                                                  41    "Singleton"      [ "Base" | "Type" ]
                                                  41    "Tree"           [ "Base" | "Syntax" | "Type" ]
                                                  42    "Complex"        [ "Base" | "Syntax" | "Type" ]
                                                  42    "Simple"         [ "Base" | "Type" ]
                                                  43    "Utility"        [ "Base" | "Syntax" | "Type" ]
                                                  44    "BaalaC"         [ "Base" | "Syntax" | "Type" ]
                                                  44    "Decode"         [ "Base" | "Syntax" | "Type" ]
                                                  
  47        "Koshucode.Baala.Data.Church"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  41    "Message"        [ "Base" | "Syntax" | "Type" ]
                                                  42    "Cox"            [ "Base" | "Syntax" ]
                                                  43    "Cop"            [ "Base" | "Syntax" ]
                                                  46    "Build"          [ "Base" | "Content" | "Syntax" ]
                                                  47    "Run"            [ "Base" | "Content" | "Syntax" | "Type" ]
                                                  
  48        "Koshucode.Baala.Data"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  40    "Type"           [ "Decimal" | "Rel" | "Time" ]
                                                  42    "Message"        [ ]
                                                  45    "Content"        [ ]
                                                  48    "Church"         [ ]
                                                  
  52        "Koshucode.Baala.Core.Relkit"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  35    "Message"        [ "Base" | "Syntax" ]
                                                  50    "Relkit"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  51    "Construct"      [ "Base" | "Data" | "Syntax" ]
                                                  52    "Run"            [ "Base" | "Data" | "Syntax" ]
                                                  
  56        "Koshucode.Baala.Core.Relmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  50    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  50    "Result"         [ "Base" | "Data" | "Syntax" ]
                                                  51    "Option"         [ "Base" | "Data" | "Syntax" ]
                                                  54    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  55    "Rop"            [ "Base" | "Lexmap" | "Syntax" ]
                                                  55    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  56    "Construct"      [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  56    "Global"         [ "Base" | "Data" ]
                                                  
  59        "Koshucode.Baala.Core.Assert"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  35    "Message"        [ "Base" | "Syntax" ]
                                                  50    "RelTable"       [ "Base" | "Data" | "Syntax" ]
                                                  54    "Dataset"        [ "Base" | "Data" | "Relkit" | "Syntax" ]
                                                  58    "Assert"         [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  59    "Run"            [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  63        "Koshucode.Baala.Core.Resource"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Message"        [ "Base" ]
                                                  50    "Clause"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  61    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  62    "Concrete"       [ "Base" | "Data" | "Relmap" | "Syntax" ]
                                                  62    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  62    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  63    "Read"           [ "Base" | "Data" | "Relmap" | "Syntax" ]
                                                  
  64        "Koshucode.Baala.Core"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  45    "Lexmap"         [ ]
                                                  51    "Message"        [ ]
                                                  53    "Relkit"         [ ]
                                                  57    "Relmap"         [ ]
                                                  60    "Assert"         [ ]
                                                  64    "Resource"       [ ]
                                                  
  67        "Koshucode.Baala.Rop.Base"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  52    "Message"        [ "Base" ]
                                                  53    "Term"           [ "Base" | "Data" | "Syntax" ]
                                                  66    "Define"         [ "Core" | "Syntax" ]
                                                  66    "Get"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  67    "Rop"            [ "Base" | "Core" ]
                                                  
  67        "Koshucode.Baala.Writer"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  66    "Csv"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "Html"           [ "Base" | "Core" | "Syntax" ]
                                                  66    "Json"           [ "Core" | "Data" | "Syntax" ]
                                                  66    "Judge"          [ "Base" | "Core" | "Data" ]
                                                  67    "Koshu"          [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  
  69        "Koshucode.Baala.Cop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Replace"        [ ]
                                                  50    "Coxhand"        [ "Base" | "Data" | "Syntax" ]
                                                  51    "Logic"          [ "Base" | "Data" ]
                                                  67    "Message"        [ ]
                                                  68    "Arith"          [ "Base" | "Data" ]
                                                  68    "List"           [ "Base" | "Data" ]
                                                  68    "Misc"           [ "Base" | "Data" | "Syntax" ]
                                                  68    "Order"          [ "Base" | "Data" ]
                                                  68    "Text"           [ "Base" | "Data" | "Syntax" ]
                                                  68    "Time"           [ "Base" | "Data" ]
                                                  68    "Type"           [ "Base" | "Data" ]
                                                  69    "Bundle"         [ "Base" | "Data" ]
                                                  
  70        "Koshucode.Baala"                     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  16    "Base"           [ "Abort" | "List" | "Prelude" | "Syntax" | "Text" ]
                                                  34    "Syntax"         [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                                  49    "Data"           [ "Church" | "Content" | "Type" ]
                                                  65    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  68    "Writer"         [ ]
                                                  70    "Cop"            [ ]
                                                  
  70        "Koshucode.Baala.Toolkit.Main"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  67    "KoshuRdf"       [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  67    "KoshuSyntax"    [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "KoshuChange"    [ "Base" ]
                                                  70    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                  70    "KoshuMain"      [ "Base" | "Core" | "Data" | "Writer" ]
                                                  
  71        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  69    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Restrict"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  71    "Rop"            [ "Base" | "Core" ]
                                                  
  74        "Koshucode.Baala.Rop.Flat"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "PoScale"        [ "Base" ]
                                                  66    "Message"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Elem"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Meta"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Order"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Resource"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Source"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  69    "Term"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                  72    "Lattice"        [ ]
                                                  73    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  73    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                  74    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  77        "Koshucode.Baala.Rop.Cox"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "GeoDatumJp"     [ ]
                                                  67    "Message"        [ ]
                                                  69    "Get"            [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Accessor"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Calc"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Filter"         [ "Base" | "Base" | "Core" | "Data" ]
                                                  70    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  70    "Range"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  76    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                  77    "Bundle"         [ "Core" | "Data" ]
                                                  
  78        "Koshucode.Baala.Rop.Nest"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  67    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  69    "Flow"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  76    "Confl"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  77    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  78    "Rop"            [ "Base" | "Core" | "Data" ]
                                                  
  79        "Koshucode.Baala.Rop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  68    "Base"           [ ]
                                                  75    "Flat"           [ "Lattice" ]
                                                  78    "Cox"            [ ]
                                                  79    "Nest"           [ ]
                                                  
  80        "Koshucode.Baala.Toolkit.Library"     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "SimpleOption"   [ ]
                                                  0     "Version"        [ ]
                                                  50    "RDF"            [ "Data" ]
                                                  66    "Element"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "Exit"           [ "Base" | "Core" | "Data" ]
                                                  66    "Input"          [ "Base" | "Core" ]
                                                  69    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                  69    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                  80    "Global"         [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      37 judges on DIR-RANK
**      37 judges in total
**

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

|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 0 | "Split" | [ ] ] [ 1 | "Assoc" | [ ] ] [ 1 | "Dots" | [ ] ] [ 1 | "List" | [ ] ] [ 1 | "Pair" | [ ] ] [ 1 | "Snip" | [ ] ] [ 2 | "Order" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 4 | "IOPoint" | [ "Prelude" ] ] [ 4 | "TextTable" | [ "Prelude" ] ] [ 4 | "Utility" | [ "Prelude" ] ] [ 4 | "Write" | [ "Prelude" ] ] [ 5 | "CodePt" | [ "Prelude" ] ] [ 5 | "Http" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 7 | "Reason" | [ "Text" ] ] [ 8 | "Abortable" | [ "Prelude" | "Text" ] ] [ 8 | "Report" | [ "Prelude" | "Text" ] ] [ 10 | "Message" | [ "Abort" ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Base.Syntax"  /base-rank {= /rank /base /import-dir [ 10 | "Message" | [ "Abort" | "Prelude" | "Text" ] ] [ 11 | "Line" | [ "Abort" | "Prelude" | "Text" ] ] [ 11 | "Tree" | [ "Abort" | "Prelude" | "Text" ] ] [ 12 | "Clause" | [ "Prelude" | "Text" ] ] [ 12 | "Infix" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 13  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 3 | "Prelude" | [ ] ] [ 6 | "Text" | [ ] ] [ 9 | "Abort" | [ ] ] [ 11 | "Message" | [ ] ] [ 13 | "Syntax" | [ ] ] =}

|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 15 | "Para" | [ "Base" ] ] [ 16 | "Get" | [ "Base" ] ] [ 16 | "ParaSpec" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Data.Type.Time"  /base-rank {= /rank /base /import-dir [ 15 | "Clock" | [ "Base" ] ] [ 16 | "Date" | [ "Base" ] ] [ 17 | "Time" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 0 | "Term" | [ ] ] [ 15 | "AngleText" | [ "Base" ] ] [ 15 | "Message" | [ "Base" ] ] [ 16 | "Next" | [ "Base" ] ] [ 17 | "Short" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Data.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 15 | "Decimal" | [ "Base" ] ] [ 15 | "Rational" | [ "Base" ] ] [ 16 | "Fraction" | [ "Base" ] ] [ 17 | "Coder" | [ "Base" ] ] [ 18 | "BinaryAb" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 22  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 19 | "Token" | [ "Base" | "Symbol" ] ] [ 20 | "Pattern" | [ ] ] [ 20 | "Utility" | [ "Base" ] ] [ 21 | "TokenLine" | [ "Base" | "Symbol" ] ] [ 22 | "TokenClause" | [ "Base" ] ] =}

|-- DIR-RANK  /dir-rank 26  /dir "Koshucode.Baala.Syntax.TTree"  /base-rank {= /rank /base /import-dir [ 24 | "Bracket" | [ "Base" | "Token" ] ] [ 25 | "TokenTree" | [ "Base" | "Token" ] ] [ 26 | "Parse" | [ "Base" | "Token" ] ] =}
|-- DIR-RANK  /dir-rank 30  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" | [ ] ] [ 28 | "Message" | [ "Base" | "Para" | "Symbol" | "TTree" ] ] [ 29 | "Attr" | [ "Base" | "Para" | "TTree" ] ] [ 29 | "Slot" | [ "Base" | "TTree" | "Token" ] ] [ 30 | "AttrEd" | [ "Base" | "TTree" | "Token" ] ] [ 30 | "Parse" | [ "Base" | "Para" ] ] =}
|-- DIR-RANK  /dir-rank 31  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 17 | "Para" | [ ] ] [ 18 | "Symbol" | [ ] ] [ 23 | "Token" | [ ] ] [ 27 | "TTree" | [ ] ] [ 29 | "Message" | [ ] ] [ 31 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 36  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 34 | "Head" | [ "Base" | "Syntax" ] ] [ 35 | "Rel" | [ "Base" | "Syntax" ] ] [ 36 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 37  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 18 | "Time" | [ ] ] [ 19 | "Decimal" | [ ] ] [ 33 | "Interp" | [ "Base" | "Syntax" ] ] [ 33 | "Judge" | [ "Base" | "Syntax" ] ] [ 33 | "Type" | [ "Base" | "Syntax" ] ] [ 37 | "Rel" | [ ] ] =}

|-- DIR-RANK  /dir-rank 40  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 33 | "Message" | [ "Base" | "Syntax" ] ] [ 39 | "Class" | [ "Base" | "Syntax" | "Type" ] ] [ 39 | "Tree" | [ "Base" | "Syntax" | "Type" ] ] [ 40 | "BaalaC" | [ "Base" | "Syntax" | "Type" ] ] [ 40 | "Decode" | [ "Base" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 42  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 33 | "Lexmap" | [ "Base" | "Syntax" ] ] [ 41 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [ 41 | "Message" | [ "Base" | "Syntax" ] ] [ 42 | "Construct" | [ "Base" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 43  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 39 | "Message" | [ "Base" | "Syntax" | "Type" ] ] [ 40 | "Cox" | [ "Base" | "Syntax" ] ] [ 41 | "Cop" | [ "Base" | "Syntax" ] ] [ 42 | "Build" | [ "Base" | "Content" | "Syntax" ] ] [ 43 | "Run" | [ "Base" | "Content" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 44  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 38 | "Type" | [ "Decimal" | "Rel" | "Time" ] ] [ 40 | "Message" | [ ] ] [ 41 | "Content" | [ ] ] [ 44 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 48  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 33 | "Message" | [ "Base" | "Syntax" ] ] [ 46 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 47 | "Construct" | [ "Base" | "Data" | "Syntax" ] ] [ 48 | "Run" | [ "Base" | "Data" | "Syntax" ] ] =}

|-- DIR-RANK  /dir-rank 52  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 46 | "Message" | [ "Base" | "Data" | "Syntax" ] ] [ 46 | "Result" | [ "Base" | "Data" | "Syntax" ] ] [ 47 | "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 50 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 51 | "Rop" | [ "Base" | "Lexmap" | "Syntax" ] ] [ 51 | "Specialize" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 52 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 52 | "Global" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 55  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 33 | "Message" | [ "Base" | "Syntax" ] ] [ 46 | "RelTable" | [ "Base" | "Data" | "Syntax" ] ] [ 50 | "Dataset" | [ "Base" | "Data" | "Relkit" | "Syntax" ] ] [ 54 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 55 | "Run" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 15 | "Queue" | [ "Base" ] ] [ 46 | "About" | [ "Base" | "Data" | "Syntax" ] ] [ 46 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 57 | "Resource" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] [ 58 | "Concrete" | [ "Data" | "Relmap" ] ] [ 58 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 58 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 59 | "Read" | [ "Base" | "Data" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 60  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 43 | "Lexmap" | [ ] ] [ 47 | "Message" | [ ] ] [ 49 | "Relkit" | [ ] ] [ 53 | "Relmap" | [ ] ] [ 56 | "Assert" | [ ] ] [ 60 | "Resource" | [ ] ] =}
|-- DIR-RANK  /dir-rank 63  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 48 | "Message" | [ "Base" ] ] [ 49 | "Term" | [ "Base" | "Data" | "Syntax" ] ] [ 62 | "Define" | [ "Core" | "Syntax" ] ] [ 62 | "Get" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 63 | "Rop" | [ "Base" | "Core" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 63  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 62 | "Csv" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "Html" | [ "Base" | "Core" | "Syntax" ] ] [ 62 | "Json" | [ "Core" | "Data" | "Syntax" ] ] [ 62 | "Judge" | [ "Base" | "Core" | "Data" ] ] [ 63 | "Koshu" | [ "Base" | "Core" | "Data" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 65  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [ 46 | "Coxhand" | [ "Base" | "Data" | "Syntax" ] ] [ 47 | "Logic" | [ "Base" | "Data" ] ] [ 63 | "Message" | [ ] ] [ 64 | "Arith" | [ "Base" | "Data" ] ] [ 64 | "List" | [ "Base" | "Data" ] ] [ 64 | "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 64 | "Order" | [ "Base" | "Data" ] ] [ 64 | "Text" | [ "Base" | "Data" | "Syntax" ] ] [ 64 | "Time" | [ "Base" | "Data" ] ] [ 64 | "Type" | [ "Base" | "Data" ] ] [ 65 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 66  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 14 | "Base" | [ "Abort" | "Prelude" | "Syntax" | "Text" ] ] [ 32 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ] ] [ 45 | "Data" | [ "Church" | "Content" | "Type" ] ] [ 61 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 64 | "Writer" | [ ] ] [ 66 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 66  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 63 | "KoshuRdf" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 63 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "KoshuChange" | [ "Base" ] ] [ 66 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 66 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 67  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 65 | "Tropashko" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "Restrict" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 67 | "Rop" | [ "Base" | "Core" ] ] =}

|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 15 | "PoScale" | [ "Base" ] ] [ 62 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 65 | "Elem" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 65 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 65 | "Meta" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 65 | "Order" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 65 | "Resource" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 65 | "Source" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 65 | "Term" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "Peripheral" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "TermGadget" | [ "Base" | "Core" | "Data" ] ] [ 68 | "Lattice" | [ ] ] [ 69 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ] ] [ 69 | "Control" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 70 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 73  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [ ] ] [ 63 | "Message" | [ ] ] [ 65 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "Accessor" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "Filter" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 66 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 66 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Empty" | [ "Base" | "Base" | "Core" | "Data" | "Flat" ] ] [ 73 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 74  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 63 | "Message" | [ "Base" | "Data" | "Syntax" ] ] [ 65 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 72 | "Confl" | [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ] ] [ 73 | "Deriv" | [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ] ] [ 74 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 75  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 64 | "Base" | [ ] ] [ 71 | "Flat" | [ "Lattice" ] ] [ 74 | "Cox" | [ ] ] [ 75 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 76  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" | [ ] ] [ 0 | "Version" | [ ] ] [ 46 | "RDF" | [ "Data" ] ] [ 62 | "Element" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "Exit" | [ "Base" | "Core" | "Data" ] ] [ 62 | "Input" | [ "Base" | "Core" ] ] [ 65 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 65 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 76 | "Global" | [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ] ] =}

*** 35 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                  /base-rank
  --------- ------------------------------------- ---------------------------------------------------------------------------------------------------
  2         "Koshucode.Baala.Base.Prelude"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Class"          [ ]
                                                  0     "Import"         [ ]
                                                  0     "Split"          [ ]
                                                  1     "Assoc"          [ ]
                                                  1     "Dots"           [ ]
                                                  1     "List"           [ ]
                                                  1     "Pair"           [ ]
                                                  1     "Snip"           [ ]
                                                  2     "Order"          [ ]
                                                  
  5         "Koshucode.Baala.Base.Text"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Comment"        [ ]
                                                  0     "Unicode"        [ ]
                                                  4     "IOPoint"        [ "Prelude" ]
                                                  4     "TextTable"      [ "Prelude" ]
                                                  4     "Utility"        [ "Prelude" ]
                                                  4     "Write"          [ "Prelude" ]
                                                  5     "CodePt"         [ "Prelude" ]
                                                  5     "Http"           [ "Prelude" ]
                                                  
  10        "Koshucode.Baala.Base.Abort"          /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  7     "Reason"         [ "Text" ]
                                                  8     "Abortable"      [ "Prelude" | "Text" ]
                                                  8     "Report"         [ "Prelude" | "Text" ]
                                                  10    "Message"        [ "Abort" ]
                                                  
  12        "Koshucode.Baala.Base.Syntax"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  10    "Message"        [ "Abort" | "Prelude" | "Text" ]
                                                  11    "Line"           [ "Abort" | "Prelude" | "Text" ]
                                                  11    "Tree"           [ "Abort" | "Prelude" | "Text" ]
                                                  12    "Clause"         [ "Prelude" | "Text" ]
                                                  12    "Infix"          [ "Prelude" ]
                                                  
  13        "Koshucode.Baala.Base"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  3     "Prelude"        [ ]
                                                  6     "Text"           [ ]
                                                  9     "Abort"          [ ]
                                                  11    "Message"        [ ]
                                                  13    "Syntax"         [ ]
                                                  
  16        "Koshucode.Baala.Syntax.Para"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Para"           [ "Base" ]
                                                  16    "Get"            [ "Base" ]
                                                  16    "ParaSpec"       [ "Base" ]
                                                  
  17        "Koshucode.Baala.Data.Type.Time"      /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Clock"          [ "Base" ]
                                                  16    "Date"           [ "Base" ]
                                                  17    "Time"           [ "Base" ]
                                                  
  17        "Koshucode.Baala.Syntax.Symbol"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Term"           [ ]
                                                  15    "AngleText"      [ "Base" ]
                                                  15    "Message"        [ "Base" ]
                                                  16    "Next"           [ "Base" ]
                                                  17    "Short"          [ "Base" ]
                                                  
  18        "Koshucode.Baala.Data.Type.Decimal"   /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Decimal"        [ "Base" ]
                                                  15    "Rational"       [ "Base" ]
                                                  16    "Fraction"       [ "Base" ]
                                                  17    "Coder"          [ "Base" ]
                                                  18    "BinaryAb"       [ "Base" ]
                                                  
  22        "Koshucode.Baala.Syntax.Token"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Message"        [ "Base" ]
                                                  19    "Token"          [ "Base" | "Symbol" ]
                                                  20    "Pattern"        [ ]
                                                  20    "Utility"        [ "Base" ]
                                                  21    "TokenLine"      [ "Base" | "Symbol" ]
                                                  22    "TokenClause"    [ "Base" ]
                                                  
  26        "Koshucode.Baala.Syntax.TTree"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  24    "Bracket"        [ "Base" | "Token" ]
                                                  25    "TokenTree"      [ "Base" | "Token" ]
                                                  26    "Parse"          [ "Base" | "Token" ]
                                                  
  30        "Koshucode.Baala.Syntax.Attr"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "AttrName"       [ ]
                                                  28    "Message"        [ "Base" | "Para" | "Symbol" | "TTree" ]
                                                  29    "Attr"           [ "Base" | "Para" | "TTree" ]
                                                  29    "Slot"           [ "Base" | "TTree" | "Token" ]
                                                  30    "AttrEd"         [ "Base" | "TTree" | "Token" ]
                                                  30    "Parse"          [ "Base" | "Para" ]
                                                  
  31        "Koshucode.Baala.Syntax"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Para"           [ ]
                                                  18    "Symbol"         [ ]
                                                  23    "Token"          [ ]
                                                  27    "TTree"          [ ]
                                                  29    "Message"        [ ]
                                                  31    "Attr"           [ ]
                                                  
  36        "Koshucode.Baala.Data.Type.Rel"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  34    "Head"           [ "Base" | "Syntax" ]
                                                  35    "Rel"            [ "Base" | "Syntax" ]
                                                  36    "Mono"           [ "Base" ]
                                                  
  37        "Koshucode.Baala.Data.Type"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Message"        [ "Base" ]
                                                  18    "Time"           [ ]
                                                  19    "Decimal"        [ ]
                                                  33    "Interp"         [ "Base" | "Syntax" ]
                                                  33    "Judge"          [ "Base" | "Syntax" ]
                                                  33    "Type"           [ "Base" | "Syntax" ]
                                                  37    "Rel"            [ ]
                                                  
  40        "Koshucode.Baala.Data.Content"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  33    "Message"        [ "Base" | "Syntax" ]
                                                  39    "Class"          [ "Base" | "Syntax" | "Type" ]
                                                  39    "Tree"           [ "Base" | "Syntax" | "Type" ]
                                                  40    "BaalaC"         [ "Base" | "Syntax" | "Type" ]
                                                  40    "Decode"         [ "Base" | "Syntax" | "Type" ]
                                                  
  42        "Koshucode.Baala.Core.Lexmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  33    "Lexmap"         [ "Base" | "Syntax" ]
                                                  41    "LexmapTrees"    [ "Base" | "Syntax" ]
                                                  41    "Message"        [ "Base" | "Syntax" ]
                                                  42    "Construct"      [ "Base" | "Syntax" ]
                                                  
  43        "Koshucode.Baala.Data.Church"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  39    "Message"        [ "Base" | "Syntax" | "Type" ]
                                                  40    "Cox"            [ "Base" | "Syntax" ]
                                                  41    "Cop"            [ "Base" | "Syntax" ]
                                                  42    "Build"          [ "Base" | "Content" | "Syntax" ]
                                                  43    "Run"            [ "Base" | "Content" | "Syntax" | "Type" ]
                                                  
  44        "Koshucode.Baala.Data"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  38    "Type"           [ "Decimal" | "Rel" | "Time" ]
                                                  40    "Message"        [ ]
                                                  41    "Content"        [ ]
                                                  44    "Church"         [ ]
                                                  
  48        "Koshucode.Baala.Core.Relkit"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  33    "Message"        [ "Base" | "Syntax" ]
                                                  46    "Relkit"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  47    "Construct"      [ "Base" | "Data" | "Syntax" ]
                                                  48    "Run"            [ "Base" | "Data" | "Syntax" ]
                                                  
  52        "Koshucode.Baala.Core.Relmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  46    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  46    "Result"         [ "Base" | "Data" | "Syntax" ]
                                                  47    "Option"         [ "Base" | "Data" | "Syntax" ]
                                                  50    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  51    "Rop"            [ "Base" | "Lexmap" | "Syntax" ]
                                                  51    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  52    "Construct"      [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  52    "Global"         [ "Base" | "Data" ]
                                                  
  55        "Koshucode.Baala.Core.Assert"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  33    "Message"        [ "Base" | "Syntax" ]
                                                  46    "RelTable"       [ "Base" | "Data" | "Syntax" ]
                                                  50    "Dataset"        [ "Base" | "Data" | "Relkit" | "Syntax" ]
                                                  54    "Assert"         [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  55    "Run"            [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  59        "Koshucode.Baala.Core.Resource"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Message"        [ "Base" ]
                                                  15    "Queue"          [ "Base" ]
                                                  46    "About"          [ "Base" | "Data" | "Syntax" ]
                                                  46    "Clause"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  57    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  58    "Concrete"       [ "Data" | "Relmap" ]
                                                  58    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  58    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  59    "Read"           [ "Base" | "Data" | "Relmap" | "Syntax" ]
                                                  
  60        "Koshucode.Baala.Core"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  43    "Lexmap"         [ ]
                                                  47    "Message"        [ ]
                                                  49    "Relkit"         [ ]
                                                  53    "Relmap"         [ ]
                                                  56    "Assert"         [ ]
                                                  60    "Resource"       [ ]
                                                  
  63        "Koshucode.Baala.Rop.Base"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  48    "Message"        [ "Base" ]
                                                  49    "Term"           [ "Base" | "Data" | "Syntax" ]
                                                  62    "Define"         [ "Core" | "Syntax" ]
                                                  62    "Get"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  63    "Rop"            [ "Base" | "Core" ]
                                                  
  63        "Koshucode.Baala.Writer"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  62    "Csv"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "Html"           [ "Base" | "Core" | "Syntax" ]
                                                  62    "Json"           [ "Core" | "Data" | "Syntax" ]
                                                  62    "Judge"          [ "Base" | "Core" | "Data" ]
                                                  63    "Koshu"          [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  
  65        "Koshucode.Baala.Cop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Replace"        [ ]
                                                  46    "Coxhand"        [ "Base" | "Data" | "Syntax" ]
                                                  47    "Logic"          [ "Base" | "Data" ]
                                                  63    "Message"        [ ]
                                                  64    "Arith"          [ "Base" | "Data" ]
                                                  64    "List"           [ "Base" | "Data" ]
                                                  64    "Misc"           [ "Base" | "Data" | "Syntax" ]
                                                  64    "Order"          [ "Base" | "Data" ]
                                                  64    "Text"           [ "Base" | "Data" | "Syntax" ]
                                                  64    "Time"           [ "Base" | "Data" ]
                                                  64    "Type"           [ "Base" | "Data" ]
                                                  65    "Bundle"         [ "Base" | "Data" ]
                                                  
  66        "Koshucode.Baala"                     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  14    "Base"           [ "Abort" | "Prelude" | "Syntax" | "Text" ]
                                                  32    "Syntax"         [ "Attr" | "Para" | "Symbol" | "TTree" | "Token" ]
                                                  45    "Data"           [ "Church" | "Content" | "Type" ]
                                                  61    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  64    "Writer"         [ ]
                                                  66    "Cop"            [ ]
                                                  
  66        "Koshucode.Baala.Toolkit.Main"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  63    "KoshuRdf"       [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  63    "KoshuSyntax"    [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "KoshuChange"    [ "Base" ]
                                                  66    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                  66    "KoshuMain"      [ "Base" | "Core" | "Data" | "Writer" ]
                                                  
  67        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  65    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "Restrict"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  67    "Rop"            [ "Base" | "Core" ]
                                                  
  70        "Koshucode.Baala.Rop.Flat"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "PoScale"        [ "Base" ]
                                                  62    "Message"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  65    "Elem"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  65    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  65    "Meta"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  65    "Order"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  65    "Resource"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  65    "Source"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  65    "Term"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                  68    "Lattice"        [ ]
                                                  69    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  69    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                  70    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  73        "Koshucode.Baala.Rop.Cox"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "GeoDatumJp"     [ ]
                                                  63    "Message"        [ ]
                                                  65    "Get"            [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "Accessor"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "Calc"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "Filter"         [ "Base" | "Base" | "Core" | "Data" ]
                                                  66    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  66    "Range"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                  73    "Bundle"         [ "Core" | "Data" ]
                                                  
  74        "Koshucode.Baala.Rop.Nest"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  63    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  65    "Flow"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  72    "Confl"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  73    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  74    "Rop"            [ "Base" | "Core" | "Data" ]
                                                  
  75        "Koshucode.Baala.Rop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  64    "Base"           [ ]
                                                  71    "Flat"           [ "Lattice" ]
                                                  74    "Cox"            [ ]
                                                  75    "Nest"           [ ]
                                                  
  76        "Koshucode.Baala.Toolkit.Library"     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "SimpleOption"   [ ]
                                                  0     "Version"        [ ]
                                                  46    "RDF"            [ "Data" ]
                                                  62    "Element"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "Exit"           [ "Base" | "Core" | "Data" ]
                                                  62    "Input"          [ "Base" | "Core" ]
                                                  65    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                  65    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                  76    "Global"         [ "Base" | "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      35 judges on DIR-RANK
**      35 judges in total
**

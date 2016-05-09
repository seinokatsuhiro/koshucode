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

|-- DIR-RANK  /dir-rank 2  /dir "Koshucode.Baala.Base.Prelude"  /base-rank {= /rank /base /import-dir [ 0 | "Class" | [ ] ] [ 0 | "Import" | [ ] ] [ 1 | "Assoc" | [ ] ] [ 1 | "List" | [ ] ] [ 1 | "Pair" | [ ] ] [ 1 | "Snip" | [ ] ] [ 2 | "Order" | [ ] ] =}
|-- DIR-RANK  /dir-rank 5  /dir "Koshucode.Baala.Base.Text"  /base-rank {= /rank /base /import-dir [ 0 | "Comment" | [ ] ] [ 0 | "Unicode" | [ ] ] [ 4 | "IOPoint" | [ "Prelude" ] ] [ 4 | "TextTable" | [ "Prelude" ] ] [ 4 | "Utility" | [ "Prelude" ] ] [ 4 | "Write" | [ "Prelude" ] ] [ 5 | "CodePt" | [ "Prelude" ] ] [ 5 | "Http" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 10  /dir "Koshucode.Baala.Base.Abort"  /base-rank {= /rank /base /import-dir [ 7 | "Reason" | [ "Text" ] ] [ 8 | "Abortable" | [ "Prelude" | "Text" ] ] [ 8 | "Report" | [ "Prelude" | "Text" ] ] [ 10 | "Message" | [ "Abort" ] ] =}
|-- DIR-RANK  /dir-rank 12  /dir "Koshucode.Baala.Base.Syntax"  /base-rank {= /rank /base /import-dir [ 10 | "Message" | [ "Abort" | "Prelude" | "Text" ] ] [ 11 | "Line" | [ "Abort" | "Prelude" | "Text" ] ] [ 11 | "Tree" | [ "Abort" | "Prelude" | "Text" ] ] [ 12 | "Clause" | [ "Prelude" | "Text" ] ] [ 12 | "Infix" | [ "Prelude" ] ] =}
|-- DIR-RANK  /dir-rank 13  /dir "Koshucode.Baala.Base"  /base-rank {= /rank /base /import-dir [ 3 | "Prelude" | [ ] ] [ 6 | "Text" | [ ] ] [ 9 | "Abort" | [ ] ] [ 11 | "Message" | [ ] ] [ 13 | "Syntax" | [ ] ] =}

|-- DIR-RANK  /dir-rank 16  /dir "Koshucode.Baala.Syntax.Para"  /base-rank {= /rank /base /import-dir [ 15 | "Para" | [ "Base" ] ] [ 16 | "Get" | [ "Base" ] ] [ 16 | "ParaSpec" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Data.Type.Time"  /base-rank {= /rank /base /import-dir [ 15 | "Clock" | [ "Base" ] ] [ 16 | "Date" | [ "Base" ] ] [ 17 | "Time" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Syntax.Symbol"  /base-rank {= /rank /base /import-dir [ 0 | "Term" | [ ] ] [ 15 | "AngleText" | [ "Base" ] ] [ 15 | "Message" | [ "Base" ] ] [ 16 | "Next" | [ "Base" ] ] [ 17 | "Short" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Data.Type.Decimal"  /base-rank {= /rank /base /import-dir [ 15 | "Decimal" | [ "Base" ] ] [ 15 | "Rational" | [ "Base" ] ] [ 16 | "Fraction" | [ "Base" ] ] [ 17 | "Coder" | [ "Base" ] ] [ 18 | "BinaryAb" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 21  /dir "Koshucode.Baala.Syntax.Token"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 19 | "Token" | [ "Base" | "Symbol" ] ] [ 20 | "Bracket" | [ "Base" ] ] [ 20 | "TokenLine" | [ "Base" | "Symbol" ] ] [ 21 | "TokenClause" | [ "Base" ] ] [ 21 | "TokenTree" | [ "Base" ] ] =}

|-- DIR-RANK  /dir-rank 25  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 0 | "AttrName" | [ ] ] [ 23 | "Message" | [ "Base" | "Para" | "Symbol" | "Token" ] ] [ 24 | "Attr" | [ "Base" | "Para" | "Token" ] ] [ 24 | "Slot" | [ "Base" | "Token" ] ] [ 25 | "AttrEd" | [ "Base" | "Token" ] ] [ 25 | "Parse" | [ "Base" | "Para" ] ] =}
|-- DIR-RANK  /dir-rank 26  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 17 | "Para" | [ ] ] [ 18 | "Symbol" | [ ] ] [ 22 | "Token" | [ ] ] [ 24 | "Message" | [ ] ] [ 26 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 31  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 29 | "Head" | [ "Base" | "Syntax" ] ] [ 30 | "Rel" | [ "Base" | "Syntax" ] ] [ 31 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 32  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 18 | "Time" | [ ] ] [ 19 | "Decimal" | [ ] ] [ 28 | "Interp" | [ "Base" | "Syntax" ] ] [ 28 | "Judge" | [ "Base" | "Syntax" ] ] [ 28 | "Type" | [ "Base" | "Syntax" ] ] [ 32 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 35  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 28 | "Message" | [ "Base" | "Syntax" ] ] [ 34 | "Class" | [ "Base" | "Syntax" | "Type" ] ] [ 34 | "Tree" | [ "Base" | "Syntax" | "Type" ] ] [ 35 | "BaalaC" | [ "Base" | "Syntax" | "Type" ] ] [ 35 | "Decode" | [ "Base" | "Syntax" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 37  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 28 | "Lexmap" | [ "Base" | "Syntax" ] ] [ 36 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [ 36 | "Message" | [ "Base" | "Syntax" ] ] [ 37 | "Construct" | [ "Base" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 34 | "Message" | [ "Base" | "Syntax" | "Type" ] ] [ 35 | "Cox" | [ "Base" | "Syntax" ] ] [ 36 | "Cop" | [ "Base" | "Syntax" ] ] [ 37 | "Build" | [ "Base" | "Content" | "Syntax" ] ] [ 38 | "Run" | [ "Base" | "Content" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 39  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 33 | "Type" | [ "Decimal" | "Rel" | "Time" ] ] [ 35 | "Message" | [ ] ] [ 36 | "Content" | [ ] ] [ 39 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 43  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 28 | "Message" | [ "Base" | "Syntax" ] ] [ 41 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 42 | "Construct" | [ "Base" | "Data" | "Syntax" ] ] [ 43 | "Run" | [ "Base" | "Data" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 47  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 41 | "Message" | [ "Base" | "Data" | "Syntax" ] ] [ 41 | "Result" | [ "Base" | "Data" | "Syntax" ] ] [ 42 | "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 45 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 46 | "Rop" | [ "Base" | "Lexmap" | "Syntax" ] ] [ 46 | "Specialize" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 47 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 47 | "Global" | [ "Base" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 50  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 28 | "Message" | [ "Base" | "Syntax" ] ] [ 41 | "RelTable" | [ "Base" | "Data" | "Syntax" ] ] [ 45 | "Dataset" | [ "Base" | "Data" | "Relkit" | "Syntax" ] ] [ 49 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 50 | "Run" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 54  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 41 | "About" | [ "Base" | "Data" | "Syntax" ] ] [ 41 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 52 | "Resource" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] [ 53 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 53 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 54 | "Read" | [ "Base" | "Data" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 55  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 38 | "Lexmap" | [ ] ] [ 42 | "Message" | [ ] ] [ 44 | "Relkit" | [ ] ] [ 48 | "Relmap" | [ ] ] [ 51 | "Assert" | [ ] ] [ 55 | "Resource" | [ ] ] =}
|-- DIR-RANK  /dir-rank 58  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 43 | "Message" | [ "Base" ] ] [ 44 | "Term" | [ "Base" | "Data" | "Syntax" ] ] [ 57 | "Define" | [ "Core" | "Syntax" ] ] [ 57 | "Get" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 58 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 58  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 57 | "Csv" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 57 | "Html" | [ "Base" | "Core" | "Syntax" ] ] [ 57 | "Json" | [ "Core" | "Data" | "Syntax" ] ] [ 57 | "Judge" | [ "Base" | "Core" | "Data" ] ] [ 58 | "Koshu" | [ "Base" | "Core" | "Data" | "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 60  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [ 41 | "Coxhand" | [ "Base" | "Data" | "Syntax" ] ] [ 42 | "Logic" | [ "Base" | "Data" ] ] [ 58 | "Message" | [ ] ] [ 59 | "Arith" | [ "Base" | "Data" ] ] [ 59 | "List" | [ "Base" | "Data" ] ] [ 59 | "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 59 | "Order" | [ "Base" | "Data" ] ] [ 59 | "Text" | [ "Base" | "Data" | "Syntax" ] ] [ 59 | "Time" | [ "Base" | "Data" ] ] [ 59 | "Type" | [ "Base" | "Data" | "Syntax" ] ] [ 60 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 14 | "Base" | [ "Abort" | "Prelude" | "Syntax" | "Text" ] ] [ 27 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "Token" ] ] [ 40 | "Data" | [ "Church" | "Content" | "Type" ] ] [ 56 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 59 | "Writer" | [ ] ] [ 61 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 58 | "KoshuRdf" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 58 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "KoshuChange" | [ "Base" ] ] [ 61 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 61 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 62  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 60 | "Tropashko" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 61 | "Restrict" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 65  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 15 | "PoScale" | [ "Base" ] ] [ 57 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 60 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 60 | "Meta" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 60 | "Order" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 60 | "Resource" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 60 | "Source" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 60 | "Term" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Peripheral" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "TermGadget" | [ "Base" | "Core" | "Data" ] ] [ 63 | "Lattice" | [ ] ] [ 64 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ] ] [ 64 | "Control" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 65 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}

|-- DIR-RANK  /dir-rank 68  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [ ] ] [ 58 | "Message" | [ ] ] [ 60 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Accessor" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Filter" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 61 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 67 | "Empty" | [ "Base" | "Base" | "Core" | "Data" | "Flat" ] ] [ 68 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 68  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 58 | "Message" | [ "Base" | "Data" | "Syntax" ] ] [ 60 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Confl" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 67 | "Deriv" | [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ] ] [ 68 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 59 | "Base" | [ ] ] [ 66 | "Flat" | [ "Lattice" ] ] [ 69 | "Cox" | [ ] ] [ 69 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" | [ ] ] [ 0 | "Version" | [ ] ] [ 41 | "RDF" | [ "Data" ] ] [ 57 | "Element" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 57 | "Exit" | [ "Base" | "Core" | "Data" ] ] [ 57 | "Input" | [ "Core" ] ] [ 60 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 60 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 70 | "Global" | [ "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ] ] =}

*** 34 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                  /base-rank
  --------- ------------------------------------- ---------------------------------------------------------------------------------------------------
  2         "Koshucode.Baala.Base.Prelude"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Class"          [ ]
                                                  0     "Import"         [ ]
                                                  1     "Assoc"          [ ]
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
                                                  
  21        "Koshucode.Baala.Syntax.Token"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Message"        [ "Base" ]
                                                  19    "Token"          [ "Base" | "Symbol" ]
                                                  20    "Bracket"        [ "Base" ]
                                                  20    "TokenLine"      [ "Base" | "Symbol" ]
                                                  21    "TokenClause"    [ "Base" ]
                                                  21    "TokenTree"      [ "Base" ]
                                                  
  25        "Koshucode.Baala.Syntax.Attr"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "AttrName"       [ ]
                                                  23    "Message"        [ "Base" | "Para" | "Symbol" | "Token" ]
                                                  24    "Attr"           [ "Base" | "Para" | "Token" ]
                                                  24    "Slot"           [ "Base" | "Token" ]
                                                  25    "AttrEd"         [ "Base" | "Token" ]
                                                  25    "Parse"          [ "Base" | "Para" ]
                                                  
  26        "Koshucode.Baala.Syntax"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Para"           [ ]
                                                  18    "Symbol"         [ ]
                                                  22    "Token"          [ ]
                                                  24    "Message"        [ ]
                                                  26    "Attr"           [ ]
                                                  
  31        "Koshucode.Baala.Data.Type.Rel"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  29    "Head"           [ "Base" | "Syntax" ]
                                                  30    "Rel"            [ "Base" | "Syntax" ]
                                                  31    "Mono"           [ "Base" ]
                                                  
  32        "Koshucode.Baala.Data.Type"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Message"        [ "Base" ]
                                                  18    "Time"           [ ]
                                                  19    "Decimal"        [ ]
                                                  28    "Interp"         [ "Base" | "Syntax" ]
                                                  28    "Judge"          [ "Base" | "Syntax" ]
                                                  28    "Type"           [ "Base" | "Syntax" ]
                                                  32    "Rel"            [ ]
                                                  
  35        "Koshucode.Baala.Data.Content"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  28    "Message"        [ "Base" | "Syntax" ]
                                                  34    "Class"          [ "Base" | "Syntax" | "Type" ]
                                                  34    "Tree"           [ "Base" | "Syntax" | "Type" ]
                                                  35    "BaalaC"         [ "Base" | "Syntax" | "Type" ]
                                                  35    "Decode"         [ "Base" | "Syntax" | "Type" ]
                                                  
  37        "Koshucode.Baala.Core.Lexmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  28    "Lexmap"         [ "Base" | "Syntax" ]
                                                  36    "LexmapTrees"    [ "Base" | "Syntax" ]
                                                  36    "Message"        [ "Base" | "Syntax" ]
                                                  37    "Construct"      [ "Base" | "Syntax" ]
                                                  
  38        "Koshucode.Baala.Data.Church"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  34    "Message"        [ "Base" | "Syntax" | "Type" ]
                                                  35    "Cox"            [ "Base" | "Syntax" ]
                                                  36    "Cop"            [ "Base" | "Syntax" ]
                                                  37    "Build"          [ "Base" | "Content" | "Syntax" ]
                                                  38    "Run"            [ "Base" | "Content" | "Syntax" | "Type" ]
                                                  
  39        "Koshucode.Baala.Data"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  33    "Type"           [ "Decimal" | "Rel" | "Time" ]
                                                  35    "Message"        [ ]
                                                  36    "Content"        [ ]
                                                  39    "Church"         [ ]
                                                  
  43        "Koshucode.Baala.Core.Relkit"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  28    "Message"        [ "Base" | "Syntax" ]
                                                  41    "Relkit"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  42    "Construct"      [ "Base" | "Data" | "Syntax" ]
                                                  43    "Run"            [ "Base" | "Data" | "Syntax" ]
                                                  
  47        "Koshucode.Baala.Core.Relmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  41    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  41    "Result"         [ "Base" | "Data" | "Syntax" ]
                                                  42    "Option"         [ "Base" | "Data" | "Syntax" ]
                                                  45    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  46    "Rop"            [ "Base" | "Lexmap" | "Syntax" ]
                                                  46    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  47    "Construct"      [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  47    "Global"         [ "Base" | "Data" ]
                                                  
  50        "Koshucode.Baala.Core.Assert"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  28    "Message"        [ "Base" | "Syntax" ]
                                                  41    "RelTable"       [ "Base" | "Data" | "Syntax" ]
                                                  45    "Dataset"        [ "Base" | "Data" | "Relkit" | "Syntax" ]
                                                  49    "Assert"         [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  50    "Run"            [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  54        "Koshucode.Baala.Core.Resource"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Message"        [ "Base" ]
                                                  41    "About"          [ "Base" | "Data" | "Syntax" ]
                                                  41    "Clause"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  52    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  53    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  53    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  54    "Read"           [ "Base" | "Data" | "Relmap" | "Syntax" ]
                                                  
  55        "Koshucode.Baala.Core"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  38    "Lexmap"         [ ]
                                                  42    "Message"        [ ]
                                                  44    "Relkit"         [ ]
                                                  48    "Relmap"         [ ]
                                                  51    "Assert"         [ ]
                                                  55    "Resource"       [ ]
                                                  
  58        "Koshucode.Baala.Rop.Base"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  43    "Message"        [ "Base" ]
                                                  44    "Term"           [ "Base" | "Data" | "Syntax" ]
                                                  57    "Define"         [ "Core" | "Syntax" ]
                                                  57    "Get"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  58    "Rop"            [ "Base" | "Core" ]
                                                  
  58        "Koshucode.Baala.Writer"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  57    "Csv"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  57    "Html"           [ "Base" | "Core" | "Syntax" ]
                                                  57    "Json"           [ "Core" | "Data" | "Syntax" ]
                                                  57    "Judge"          [ "Base" | "Core" | "Data" ]
                                                  58    "Koshu"          [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  
  60        "Koshucode.Baala.Cop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Replace"        [ ]
                                                  41    "Coxhand"        [ "Base" | "Data" | "Syntax" ]
                                                  42    "Logic"          [ "Base" | "Data" ]
                                                  58    "Message"        [ ]
                                                  59    "Arith"          [ "Base" | "Data" ]
                                                  59    "List"           [ "Base" | "Data" ]
                                                  59    "Misc"           [ "Base" | "Data" | "Syntax" ]
                                                  59    "Order"          [ "Base" | "Data" ]
                                                  59    "Text"           [ "Base" | "Data" | "Syntax" ]
                                                  59    "Time"           [ "Base" | "Data" ]
                                                  59    "Type"           [ "Base" | "Data" | "Syntax" ]
                                                  60    "Bundle"         [ "Base" | "Data" ]
                                                  
  61        "Koshucode.Baala"                     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  14    "Base"           [ "Abort" | "Prelude" | "Syntax" | "Text" ]
                                                  27    "Syntax"         [ "Attr" | "Para" | "Symbol" | "Token" ]
                                                  40    "Data"           [ "Church" | "Content" | "Type" ]
                                                  56    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  59    "Writer"         [ ]
                                                  61    "Cop"            [ ]
                                                  
  61        "Koshucode.Baala.Toolkit.Main"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  58    "KoshuRdf"       [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  58    "KoshuSyntax"    [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "KoshuChange"    [ "Base" ]
                                                  61    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                  61    "KoshuMain"      [ "Base" | "Core" | "Data" | "Writer" ]
                                                  
  62        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  60    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" ]
                                                  61    "Restrict"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "Rop"            [ "Base" | "Core" ]
                                                  
  65        "Koshucode.Baala.Rop.Flat"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "PoScale"        [ "Base" ]
                                                  57    "Message"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  60    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  60    "Meta"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  60    "Order"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  60    "Resource"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  60    "Source"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  60    "Term"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                  63    "Lattice"        [ ]
                                                  64    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  64    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                  65    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  68        "Koshucode.Baala.Rop.Cox"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "GeoDatumJp"     [ ]
                                                  58    "Message"        [ ]
                                                  60    "Get"            [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Accessor"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Calc"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Filter"         [ "Base" | "Base" | "Core" | "Data" ]
                                                  61    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Range"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  67    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                  68    "Bundle"         [ "Core" | "Data" ]
                                                  
  68        "Koshucode.Baala.Rop.Nest"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  58    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  60    "Flow"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Confl"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  67    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  68    "Rop"            [ "Base" | "Core" | "Data" ]
                                                  
  69        "Koshucode.Baala.Rop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  59    "Base"           [ ]
                                                  66    "Flat"           [ "Lattice" ]
                                                  69    "Cox"            [ ]
                                                  69    "Nest"           [ ]
                                                  
  70        "Koshucode.Baala.Toolkit.Library"     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "SimpleOption"   [ ]
                                                  0     "Version"        [ ]
                                                  41    "RDF"            [ "Data" ]
                                                  57    "Element"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  57    "Exit"           [ "Base" | "Core" | "Data" ]
                                                  57    "Input"          [ "Core" ]
                                                  60    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                  60    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                  70    "Global"         [ "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      34 judges on DIR-RANK
**      34 judges in total
**

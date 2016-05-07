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

|-- DIR-RANK  /dir-rank 26  /dir "Koshucode.Baala.Syntax.Attr"  /base-rank {= /rank /base /import-dir [ 23 | "Message" | [ "Base" | "Symbol" | "Token" ] ] [ 24 | "AttrPos" | [ "Base" | "Token" ] ] [ 25 | "Attr" | [ "Base" | "Para" | "Token" ] ] [ 25 | "Slot" | [ "Base" | "Token" ] ] [ 26 | "AttrEd" | [ "Base" | "Token" ] ] [ 26 | "Parse" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 27  /dir "Koshucode.Baala.Syntax"  /base-rank {= /rank /base /import-dir [ 17 | "Para" | [ ] ] [ 18 | "Symbol" | [ ] ] [ 22 | "Token" | [ ] ] [ 24 | "Message" | [ ] ] [ 27 | "Attr" | [ ] ] =}
|-- DIR-RANK  /dir-rank 32  /dir "Koshucode.Baala.Data.Type.Rel"  /base-rank {= /rank /base /import-dir [ 30 | "Head" | [ "Base" | "Syntax" ] ] [ 31 | "Rel" | [ "Base" | "Syntax" ] ] [ 32 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 33  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 18 | "Time" | [ ] ] [ 19 | "Decimal" | [ ] ] [ 29 | "Interp" | [ "Base" | "Syntax" ] ] [ 29 | "Judge" | [ "Base" | "Syntax" ] ] [ 29 | "Type" | [ "Base" | "Syntax" ] ] [ 33 | "Rel" | [ ] ] =}
|-- DIR-RANK  /dir-rank 36  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 29 | "Message" | [ "Base" | "Syntax" ] ] [ 35 | "Class" | [ "Base" | "Syntax" | "Type" ] ] [ 35 | "Tree" | [ "Base" | "Syntax" | "Type" ] ] [ 36 | "BaalaC" | [ "Base" | "Syntax" | "Type" ] ] [ 36 | "Decode" | [ "Base" | "Syntax" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 29 | "Lexmap" | [ "Base" | "Syntax" ] ] [ 37 | "LexmapTrees" | [ "Base" | "Syntax" ] ] [ 37 | "Message" | [ "Base" | "Syntax" ] ] [ 38 | "Construct" | [ "Base" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 39  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 35 | "Message" | [ "Base" | "Syntax" | "Type" ] ] [ 36 | "Cox" | [ "Base" | "Syntax" ] ] [ 37 | "Cop" | [ "Base" | "Syntax" ] ] [ 38 | "Build" | [ "Base" | "Content" | "Syntax" ] ] [ 39 | "Run" | [ "Base" | "Content" | "Syntax" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 40  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 34 | "Type" | [ "Decimal" | "Rel" | "Time" ] ] [ 36 | "Message" | [ ] ] [ 37 | "Content" | [ ] ] [ 40 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 44  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 29 | "Message" | [ "Base" | "Syntax" ] ] [ 42 | "Relkit" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 43 | "Construct" | [ "Base" | "Data" | "Syntax" ] ] [ 44 | "Run" | [ "Base" | "Data" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 48  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 42 | "Message" | [ "Base" | "Data" | "Syntax" ] ] [ 42 | "Result" | [ "Base" | "Data" | "Syntax" ] ] [ 43 | "Option" | [ "Base" | "Data" | "Syntax" ] ] [ 46 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 47 | "Rop" | [ "Base" | "Lexmap" | "Syntax" ] ] [ 47 | "Specialize" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 48 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ] ] [ 48 | "Global" | [ "Base" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 51  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 29 | "Message" | [ "Base" | "Syntax" ] ] [ 42 | "RelTable" | [ "Base" | "Data" | "Syntax" ] ] [ 46 | "Dataset" | [ "Base" | "Data" | "Relkit" | "Syntax" ] ] [ 50 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 51 | "Run" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 55  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 42 | "About" | [ "Base" | "Data" | "Syntax" ] ] [ 42 | "Clause" | [ "Base" | "Data" | "Lexmap" | "Syntax" ] ] [ 53 | "Resource" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ] ] [ 54 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 54 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ] ] [ 55 | "Read" | [ "Base" | "Data" | "Relmap" | "Syntax" ] ] =}
|-- DIR-RANK  /dir-rank 56  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 39 | "Lexmap" | [ ] ] [ 43 | "Message" | [ ] ] [ 45 | "Relkit" | [ ] ] [ 49 | "Relmap" | [ ] ] [ 52 | "Assert" | [ ] ] [ 56 | "Resource" | [ ] ] =}
|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 44 | "Message" | [ "Base" ] ] [ 45 | "Term" | [ "Base" | "Data" | "Syntax" ] ] [ 58 | "Define" | [ "Core" | "Syntax" ] ] [ 58 | "Get" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 59 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 58 | "Csv" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 58 | "Html" | [ "Base" | "Core" | "Syntax" ] ] [ 58 | "Json" | [ "Core" | "Data" | "Syntax" ] ] [ 58 | "Judge" | [ "Base" | "Core" | "Data" ] ] [ 59 | "Koshu" | [ "Base" | "Core" | "Data" | "Syntax" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [ 42 | "Coxhand" | [ "Base" | "Data" | "Syntax" ] ] [ 43 | "Logic" | [ "Base" | "Data" ] ] [ 59 | "Message" | [ ] ] [ 60 | "Arith" | [ "Base" | "Data" ] ] [ 60 | "List" | [ "Base" | "Data" ] ] [ 60 | "Misc" | [ "Base" | "Data" | "Syntax" ] ] [ 60 | "Order" | [ "Base" | "Data" ] ] [ 60 | "Text" | [ "Base" | "Data" | "Syntax" ] ] [ 60 | "Time" | [ "Base" | "Data" ] ] [ 60 | "Type" | [ "Base" | "Data" | "Syntax" ] ] [ 61 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 62  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 14 | "Base" | [ "Abort" | "Prelude" | "Syntax" | "Text" ] ] [ 28 | "Syntax" | [ "Attr" | "Para" | "Symbol" | "Token" ] ] [ 41 | "Data" | [ "Church" | "Content" | "Type" ] ] [ 57 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 60 | "Writer" | [ ] ] [ 62 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 62  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 59 | "KoshuRdf" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 59 | "KoshuSyntax" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "KoshuChange" | [ "Base" ] ] [ 62 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 62 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 63  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 61 | "Tropashko" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 62 | "Restrict" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 63 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 66  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 15 | "PoScale" | [ "Base" ] ] [ 58 | "Message" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Meta" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Order" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Resource" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Source" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 61 | "Term" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "Peripheral" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "TermGadget" | [ "Base" | "Core" | "Data" ] ] [ 64 | "Lattice" | [ ] ] [ 65 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ] ] [ 65 | "Control" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 66 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}

|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [ ] ] [ 59 | "Message" | [ ] ] [ 61 | "Get" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "Accessor" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "Calc" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "Filter" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 62 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "Range" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 68 | "Empty" | [ "Base" | "Base" | "Core" | "Data" | "Flat" ] ] [ 69 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 59 | "Message" | [ "Base" | "Data" | "Syntax" ] ] [ 61 | "Flow" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 62 | "Confl" | [ "Base" | "Base" | "Core" | "Data" | "Syntax" ] ] [ 68 | "Deriv" | [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ] ] [ 69 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 70  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 60 | "Base" | [ ] ] [ 67 | "Flat" | [ "Lattice" ] ] [ 70 | "Cox" | [ ] ] [ 70 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 71  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" | [ ] ] [ 0 | "Version" | [ ] ] [ 42 | "RDF" | [ "Data" ] ] [ 58 | "Element" | [ "Base" | "Core" | "Data" | "Syntax" ] ] [ 58 | "Exit" | [ "Base" | "Core" | "Data" ] ] [ 58 | "Input" | [ "Core" ] ] [ 61 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 61 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 71 | "Global" | [ "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ] ] =}

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
                                                  
  26        "Koshucode.Baala.Syntax.Attr"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  23    "Message"        [ "Base" | "Symbol" | "Token" ]
                                                  24    "AttrPos"        [ "Base" | "Token" ]
                                                  25    "Attr"           [ "Base" | "Para" | "Token" ]
                                                  25    "Slot"           [ "Base" | "Token" ]
                                                  26    "AttrEd"         [ "Base" | "Token" ]
                                                  26    "Parse"          [ "Base" ]
                                                  
  27        "Koshucode.Baala.Syntax"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  17    "Para"           [ ]
                                                  18    "Symbol"         [ ]
                                                  22    "Token"          [ ]
                                                  24    "Message"        [ ]
                                                  27    "Attr"           [ ]
                                                  
  32        "Koshucode.Baala.Data.Type.Rel"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  30    "Head"           [ "Base" | "Syntax" ]
                                                  31    "Rel"            [ "Base" | "Syntax" ]
                                                  32    "Mono"           [ "Base" ]
                                                  
  33        "Koshucode.Baala.Data.Type"           /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Message"        [ "Base" ]
                                                  18    "Time"           [ ]
                                                  19    "Decimal"        [ ]
                                                  29    "Interp"         [ "Base" | "Syntax" ]
                                                  29    "Judge"          [ "Base" | "Syntax" ]
                                                  29    "Type"           [ "Base" | "Syntax" ]
                                                  33    "Rel"            [ ]
                                                  
  36        "Koshucode.Baala.Data.Content"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  29    "Message"        [ "Base" | "Syntax" ]
                                                  35    "Class"          [ "Base" | "Syntax" | "Type" ]
                                                  35    "Tree"           [ "Base" | "Syntax" | "Type" ]
                                                  36    "BaalaC"         [ "Base" | "Syntax" | "Type" ]
                                                  36    "Decode"         [ "Base" | "Syntax" | "Type" ]
                                                  
  38        "Koshucode.Baala.Core.Lexmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  29    "Lexmap"         [ "Base" | "Syntax" ]
                                                  37    "LexmapTrees"    [ "Base" | "Syntax" ]
                                                  37    "Message"        [ "Base" | "Syntax" ]
                                                  38    "Construct"      [ "Base" | "Syntax" ]
                                                  
  39        "Koshucode.Baala.Data.Church"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  35    "Message"        [ "Base" | "Syntax" | "Type" ]
                                                  36    "Cox"            [ "Base" | "Syntax" ]
                                                  37    "Cop"            [ "Base" | "Syntax" ]
                                                  38    "Build"          [ "Base" | "Content" | "Syntax" ]
                                                  39    "Run"            [ "Base" | "Content" | "Syntax" | "Type" ]
                                                  
  40        "Koshucode.Baala.Data"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  34    "Type"           [ "Decimal" | "Rel" | "Time" ]
                                                  36    "Message"        [ ]
                                                  37    "Content"        [ ]
                                                  40    "Church"         [ ]
                                                  
  44        "Koshucode.Baala.Core.Relkit"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  29    "Message"        [ "Base" | "Syntax" ]
                                                  42    "Relkit"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  43    "Construct"      [ "Base" | "Data" | "Syntax" ]
                                                  44    "Run"            [ "Base" | "Data" | "Syntax" ]
                                                  
  48        "Koshucode.Baala.Core.Relmap"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  42    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  42    "Result"         [ "Base" | "Data" | "Syntax" ]
                                                  43    "Option"         [ "Base" | "Data" | "Syntax" ]
                                                  46    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  47    "Rop"            [ "Base" | "Lexmap" | "Syntax" ]
                                                  47    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  48    "Construct"      [ "Base" | "Data" | "Lexmap" | "Relkit" | "Syntax" ]
                                                  48    "Global"         [ "Base" | "Data" ]
                                                  
  51        "Koshucode.Baala.Core.Assert"         /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  29    "Message"        [ "Base" | "Syntax" ]
                                                  42    "RelTable"       [ "Base" | "Data" | "Syntax" ]
                                                  46    "Dataset"        [ "Base" | "Data" | "Relkit" | "Syntax" ]
                                                  50    "Assert"         [ "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  51    "Run"            [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  
  55        "Koshucode.Baala.Core.Resource"       /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "Message"        [ "Base" ]
                                                  42    "About"          [ "Base" | "Data" | "Syntax" ]
                                                  42    "Clause"         [ "Base" | "Data" | "Lexmap" | "Syntax" ]
                                                  53    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" | "Syntax" ]
                                                  54    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  54    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" | "Syntax" ]
                                                  55    "Read"           [ "Base" | "Data" | "Relmap" | "Syntax" ]
                                                  
  56        "Koshucode.Baala.Core"                /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  39    "Lexmap"         [ ]
                                                  43    "Message"        [ ]
                                                  45    "Relkit"         [ ]
                                                  49    "Relmap"         [ ]
                                                  52    "Assert"         [ ]
                                                  56    "Resource"       [ ]
                                                  
  59        "Koshucode.Baala.Rop.Base"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  44    "Message"        [ "Base" ]
                                                  45    "Term"           [ "Base" | "Data" | "Syntax" ]
                                                  58    "Define"         [ "Core" | "Syntax" ]
                                                  58    "Get"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  59    "Rop"            [ "Base" | "Core" ]
                                                  
  59        "Koshucode.Baala.Writer"              /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  58    "Csv"            [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  58    "Html"           [ "Base" | "Core" | "Syntax" ]
                                                  58    "Json"           [ "Core" | "Data" | "Syntax" ]
                                                  58    "Judge"          [ "Base" | "Core" | "Data" ]
                                                  59    "Koshu"          [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  
  61        "Koshucode.Baala.Cop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "Replace"        [ ]
                                                  42    "Coxhand"        [ "Base" | "Data" | "Syntax" ]
                                                  43    "Logic"          [ "Base" | "Data" ]
                                                  59    "Message"        [ ]
                                                  60    "Arith"          [ "Base" | "Data" ]
                                                  60    "List"           [ "Base" | "Data" ]
                                                  60    "Misc"           [ "Base" | "Data" | "Syntax" ]
                                                  60    "Order"          [ "Base" | "Data" ]
                                                  60    "Text"           [ "Base" | "Data" | "Syntax" ]
                                                  60    "Time"           [ "Base" | "Data" ]
                                                  60    "Type"           [ "Base" | "Data" | "Syntax" ]
                                                  61    "Bundle"         [ "Base" | "Data" ]
                                                  
  62        "Koshucode.Baala"                     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  14    "Base"           [ "Abort" | "Prelude" | "Syntax" | "Text" ]
                                                  28    "Syntax"         [ "Attr" | "Para" | "Symbol" | "Token" ]
                                                  41    "Data"           [ "Church" | "Content" | "Type" ]
                                                  57    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                  60    "Writer"         [ ]
                                                  62    "Cop"            [ ]
                                                  
  62        "Koshucode.Baala.Toolkit.Main"        /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  59    "KoshuRdf"       [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  59    "KoshuSyntax"    [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "KoshuChange"    [ "Base" ]
                                                  62    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                  62    "KoshuMain"      [ "Base" | "Core" | "Data" | "Writer" ]
                                                  
  63        "Koshucode.Baala.Rop.Flat.Lattice"    /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  61    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" ]
                                                  62    "Restrict"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  63    "Rop"            [ "Base" | "Core" ]
                                                  
  66        "Koshucode.Baala.Rop.Flat"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  15    "PoScale"        [ "Base" ]
                                                  58    "Message"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Meta"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Order"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Resource"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Source"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  61    "Term"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                  64    "Lattice"        [ ]
                                                  65    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" | "Syntax" ]
                                                  65    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                  66    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                  
  69        "Koshucode.Baala.Rop.Cox"             /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "GeoDatumJp"     [ ]
                                                  59    "Message"        [ ]
                                                  61    "Get"            [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "Accessor"       [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "Calc"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "Filter"         [ "Base" | "Base" | "Core" | "Data" ]
                                                  62    "Gadget"         [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "Range"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  68    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                  69    "Bundle"         [ "Core" | "Data" ]
                                                  
  69        "Koshucode.Baala.Rop.Nest"            /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  59    "Message"        [ "Base" | "Data" | "Syntax" ]
                                                  61    "Flow"           [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  62    "Confl"          [ "Base" | "Base" | "Core" | "Data" | "Syntax" ]
                                                  68    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" | "Syntax" ]
                                                  69    "Rop"            [ "Base" | "Core" | "Data" ]
                                                  
  70        "Koshucode.Baala.Rop"                 /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  60    "Base"           [ ]
                                                  67    "Flat"           [ "Lattice" ]
                                                  70    "Cox"            [ ]
                                                  70    "Nest"           [ ]
                                                  
  71        "Koshucode.Baala.Toolkit.Library"     /rank /base            /import-dir
                                                  ----- ---------------- ----------------------------------------------------------------------------
                                                  0     "SimpleOption"   [ ]
                                                  0     "Version"        [ ]
                                                  42    "RDF"            [ "Data" ]
                                                  58    "Element"        [ "Base" | "Core" | "Data" | "Syntax" ]
                                                  58    "Exit"           [ "Base" | "Core" | "Data" ]
                                                  58    "Input"          [ "Core" ]
                                                  61    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                  61    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                  71    "Global"         [ "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                  

=== rel

**
**  SUMMARY
**      34 judges on DIR-RANK
**      34 judges in total
**

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

|-- DIR-RANK  /dir-rank 17  /dir "Koshucode.Baala.Data.Symbol"  /base-rank {= /rank /base /import-dir [ 0 | "Term" | [ ] ] [ 15 | "AngleText" | [ "Base" ] ] [ 15 | "Message" | [ "Base" ] ] [ 16 | "Next" | [ "Base" ] ] [ 17 | "Short" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 21  /dir "Koshucode.Baala.Data.Token"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 19 | "Token" | [ "Base" | "Symbol" ] ] [ 20 | "Bracket" | [ "Base" ] ] [ 20 | "TokenLine" | [ "Base" | "Symbol" ] ] [ 21 | "TokenClause" | [ "Base" ] ] [ 21 | "TokenTree" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 24  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 15 | "Clock" | [ "Base" ] ] [ 15 | "Message" | [ "Base" ] ] [ 16 | "Date" | [ "Base" ] ] [ 16 | "Decimal" | [ "Base" ] ] [ 17 | "Arithmetic" | [ "Base" ] ] [ 17 | "Time" | [ "Base" ] ] [ 19 | "Judge" | [ "Base" | "Symbol" ] ] [ 19 | "Type" | [ "Base" | "Symbol" ] ] [ 20 | "About" | [ "Base" | "Symbol" ] ] [ 20 | "Head" | [ "Base" | "Symbol" ] ] [ 23 | "Interp" | [ "Base" | "Symbol" | "Token" ] ] [ 23 | "Rel" | [ "Base" | "Symbol" | "Token" ] ] [ 24 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 27  /dir "Koshucode.Baala.Data.Attr"  /base-rank {= /rank /base /import-dir [ 15 | "Para" | [ "Base" ] ] [ 24 | "Message" | [ "Base" | "Symbol" | "Token" ] ] [ 25 | "AttrPos" | [ "Base" | "Token" ] ] [ 26 | "Attr" | [ "Base" | "Token" ] ] [ 26 | "Slot" | [ "Base" | "Token" ] ] [ 27 | "AttrEd" | [ "Base" | "Token" ] ] [ 27 | "Parse" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 27  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 23 | "Message" | [ "Base" | "Token" ] ] [ 26 | "Class" | [ "Base" | "Symbol" | "Type" ] ] [ 26 | "Tree" | [ "Base" | "Symbol" | "Token" | "Type" ] ] [ 27 | "BaalaC" | [ "Base" | "Symbol" | "Token" | "Type" ] ] [ 27 | "Decode" | [ "Base" | "Symbol" | "Token" | "Type" ] ] =}

|-- DIR-RANK  /dir-rank 30  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 26 | "Message" | [ "Base" | "Symbol" | "Token" | "Type" ] ] [ 27 | "Cox" | [ "Base" | "Symbol" | "Token" ] ] [ 28 | "Cop" | [ "Base" | "Token" ] ] [ 29 | "Build" | [ "Base" | "Content" | "Token" ] ] [ 30 | "Run" | [ "Base" | "Content" | "Symbol" | "Token" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 31  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 18 | "Symbol" | [ ] ] [ 22 | "Token" | [ ] ] [ 25 | "Type" | [ ] ] [ 27 | "Message" | [ ] ] [ 28 | "Attr" | [ ] ] [ 28 | "Content" | [ ] ] [ 31 | "Church" | [ ] ] =}
|-- DIR-RANK  /dir-rank 34  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 33 | "Lexmap" | [ "Base" | "Data" ] ] [ 33 | "LexmapTrees" | [ "Base" | "Data" ] ] [ 33 | "Message" | [ "Base" | "Data" ] ] [ 34 | "Construct" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 33 | "Message" | [ "Base" | "Data" ] ] [ 36 | "Relkit" | [ "Base" | "Data" | "Lexmap" ] ] [ 37 | "Construct" | [ "Base" | "Data" ] ] [ 38 | "Run" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 42  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 33 | "Message" | [ "Base" | "Data" ] ] [ 33 | "Result" | [ "Base" | "Data" ] ] [ 34 | "Option" | [ "Base" | "Data" ] ] [ 40 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Relkit" ] ] [ 41 | "Rop" | [ "Base" | "Data" | "Lexmap" ] ] [ 41 | "Specialize" | [ "Base" | "Data" | "Lexmap" | "Relkit" ] ] [ 42 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Relkit" ] ] [ 42 | "Global" | [ "Base" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 45  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 33 | "Message" | [ "Base" | "Data" ] ] [ 33 | "RelTable" | [ "Base" | "Data" ] ] [ 40 | "Dataset" | [ "Base" | "Data" | "Relkit" ] ] [ 44 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Relmap" ] ] [ 45 | "Run" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" ] ] =}
|-- DIR-RANK  /dir-rank 49  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 36 | "Clause" | [ "Base" | "Data" | "Lexmap" ] ] [ 47 | "Resource" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" ] ] [ 48 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" ] ] [ 48 | "Run" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" ] ] [ 49 | "Read" | [ "Base" | "Data" | "Relmap" ] ] =}
|-- DIR-RANK  /dir-rank 50  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 34 | "Message" | [ ] ] [ 35 | "Lexmap" | [ ] ] [ 39 | "Relkit" | [ ] ] [ 43 | "Relmap" | [ ] ] [ 46 | "Assert" | [ ] ] [ 50 | "Resource" | [ ] ] =}
|-- DIR-RANK  /dir-rank 53  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 35 | "Message" | [ "Base" ] ] [ 36 | "Term" | [ "Base" | "Data" ] ] [ 52 | "Define" | [ "Core" | "Data" ] ] [ 52 | "Get" | [ "Base" | "Core" | "Data" ] ] [ 53 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 53  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 52 | "Csv" | [ "Base" | "Core" | "Data" ] ] [ 52 | "Html" | [ "Base" | "Core" | "Data" ] ] [ 52 | "Json" | [ "Core" | "Data" ] ] [ 52 | "Judge" | [ "Base" | "Core" | "Data" ] ] [ 53 | "Koshu" | [ "Base" | "Core" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 55  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [ 33 | "Coxhand" | [ "Base" | "Data" ] ] [ 34 | "Logic" | [ "Base" | "Data" ] ] [ 53 | "Message" | [ ] ] [ 54 | "Arith" | [ "Base" | "Data" ] ] [ 54 | "List" | [ "Base" | "Data" ] ] [ 54 | "Misc" | [ "Base" | "Data" ] ] [ 54 | "Order" | [ "Base" | "Data" ] ] [ 54 | "Text" | [ "Base" | "Data" ] ] [ 54 | "Time" | [ "Base" | "Data" ] ] [ 54 | "Type" | [ "Base" | "Data" ] ] [ 55 | "Bundle" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 56  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 14 | "Base" | [ "Abort" | "Prelude" | "Syntax" | "Text" ] ] [ 32 | "Data" | [ "Attr" | "Church" | "Content" | "Symbol" | "Token" | "Type" ] ] [ 51 | "Core" | [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 54 | "Writer" | [ ] ] [ 56 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 56  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 53 | "KoshuRdf" | [ "Base" | "Core" | "Data" ] ] [ 53 | "KoshuSyntax" | [ "Base" | "Core" | "Data" ] ] [ 56 | "KoshuChange" | [ "Base" ] ] [ 56 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 56 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 57  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 55 | "Tropashko" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 56 | "Restrict" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 57 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 60  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 15 | "PoScale" | [ "Base" ] ] [ 52 | "Message" | [ "Base" | "Core" | "Data" ] ] [ 55 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 55 | "Meta" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 55 | "Order" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 55 | "Resource" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 55 | "Source" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 55 | "Term" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 56 | "Peripheral" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 56 | "TermGadget" | [ "Base" | "Core" | "Data" ] ] [ 58 | "Lattice" | [ ] ] [ 59 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 59 | "Control" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 60 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 63  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [ ] ] [ 53 | "Message" | [ ] ] [ 55 | "Get" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 56 | "Accessor" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 56 | "Calc" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 56 | "Filter" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 56 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 56 | "Range" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 62 | "Empty" | [ "Base" | "Base" | "Core" | "Data" | "Flat" ] ] [ 63 | "Bundle" | [ "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 63  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 53 | "Message" | [ "Base" | "Data" ] ] [ 55 | "Flow" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 56 | "Confl" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 62 | "Deriv" | [ "Base" | "Base" | "Core" | "Data" | "Flat" ] ] [ 63 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 64  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 54 | "Base" | [ ] ] [ 61 | "Flat" | [ "Lattice" ] ] [ 64 | "Cox" | [ ] ] [ 64 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 65  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" | [ ] ] [ 0 | "Version" | [ ] ] [ 33 | "RDF" | [ "Data" ] ] [ 52 | "Element" | [ "Base" | "Core" | "Data" ] ] [ 52 | "Exit" | [ "Base" | "Core" | "Data" ] ] [ 52 | "Input" | [ "Core" ] ] [ 55 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 55 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 65 | "Global" | [ "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ] ] =}

*** 29 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                 /base-rank
  --------- ------------------------------------ ----------------------------------------------------------------------------------------
  2         "Koshucode.Baala.Base.Prelude"       /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 0     "Class"          [ ]
                                                 0     "Import"         [ ]
                                                 1     "Assoc"          [ ]
                                                 1     "List"           [ ]
                                                 1     "Pair"           [ ]
                                                 1     "Snip"           [ ]
                                                 2     "Order"          [ ]
                                                 
  5         "Koshucode.Baala.Base.Text"          /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 0     "Comment"        [ ]
                                                 0     "Unicode"        [ ]
                                                 4     "IOPoint"        [ "Prelude" ]
                                                 4     "TextTable"      [ "Prelude" ]
                                                 4     "Utility"        [ "Prelude" ]
                                                 4     "Write"          [ "Prelude" ]
                                                 5     "CodePt"         [ "Prelude" ]
                                                 5     "Http"           [ "Prelude" ]
                                                 
  10        "Koshucode.Baala.Base.Abort"         /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 7     "Reason"         [ "Text" ]
                                                 8     "Abortable"      [ "Prelude" | "Text" ]
                                                 8     "Report"         [ "Prelude" | "Text" ]
                                                 10    "Message"        [ "Abort" ]
                                                 
  12        "Koshucode.Baala.Base.Syntax"        /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 10    "Message"        [ "Abort" | "Prelude" | "Text" ]
                                                 11    "Line"           [ "Abort" | "Prelude" | "Text" ]
                                                 11    "Tree"           [ "Abort" | "Prelude" | "Text" ]
                                                 12    "Clause"         [ "Prelude" | "Text" ]
                                                 12    "Infix"          [ "Prelude" ]
                                                 
  13        "Koshucode.Baala.Base"               /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 3     "Prelude"        [ ]
                                                 6     "Text"           [ ]
                                                 9     "Abort"          [ ]
                                                 11    "Message"        [ ]
                                                 13    "Syntax"         [ ]
                                                 
  17        "Koshucode.Baala.Data.Symbol"        /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 0     "Term"           [ ]
                                                 15    "AngleText"      [ "Base" ]
                                                 15    "Message"        [ "Base" ]
                                                 16    "Next"           [ "Base" ]
                                                 17    "Short"          [ "Base" ]
                                                 
  21        "Koshucode.Baala.Data.Token"         /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 15    "Message"        [ "Base" ]
                                                 19    "Token"          [ "Base" | "Symbol" ]
                                                 20    "Bracket"        [ "Base" ]
                                                 20    "TokenLine"      [ "Base" | "Symbol" ]
                                                 21    "TokenClause"    [ "Base" ]
                                                 21    "TokenTree"      [ "Base" ]
                                                 
  24        "Koshucode.Baala.Data.Type"          /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 15    "Clock"          [ "Base" ]
                                                 15    "Message"        [ "Base" ]
                                                 16    "Date"           [ "Base" ]
                                                 16    "Decimal"        [ "Base" ]
                                                 17    "Arithmetic"     [ "Base" ]
                                                 17    "Time"           [ "Base" ]
                                                 19    "Judge"          [ "Base" | "Symbol" ]
                                                 19    "Type"           [ "Base" | "Symbol" ]
                                                 20    "About"          [ "Base" | "Symbol" ]
                                                 20    "Head"           [ "Base" | "Symbol" ]
                                                 23    "Interp"         [ "Base" | "Symbol" | "Token" ]
                                                 23    "Rel"            [ "Base" | "Symbol" | "Token" ]
                                                 24    "Mono"           [ "Base" ]
                                                 
  27        "Koshucode.Baala.Data.Attr"          /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 15    "Para"           [ "Base" ]
                                                 24    "Message"        [ "Base" | "Symbol" | "Token" ]
                                                 25    "AttrPos"        [ "Base" | "Token" ]
                                                 26    "Attr"           [ "Base" | "Token" ]
                                                 26    "Slot"           [ "Base" | "Token" ]
                                                 27    "AttrEd"         [ "Base" | "Token" ]
                                                 27    "Parse"          [ "Base" ]
                                                 
  27        "Koshucode.Baala.Data.Content"       /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 23    "Message"        [ "Base" | "Token" ]
                                                 26    "Class"          [ "Base" | "Symbol" | "Type" ]
                                                 26    "Tree"           [ "Base" | "Symbol" | "Token" | "Type" ]
                                                 27    "BaalaC"         [ "Base" | "Symbol" | "Token" | "Type" ]
                                                 27    "Decode"         [ "Base" | "Symbol" | "Token" | "Type" ]
                                                 
  30        "Koshucode.Baala.Data.Church"        /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 26    "Message"        [ "Base" | "Symbol" | "Token" | "Type" ]
                                                 27    "Cox"            [ "Base" | "Symbol" | "Token" ]
                                                 28    "Cop"            [ "Base" | "Token" ]
                                                 29    "Build"          [ "Base" | "Content" | "Token" ]
                                                 30    "Run"            [ "Base" | "Content" | "Symbol" | "Token" | "Type" ]
                                                 
  31        "Koshucode.Baala.Data"               /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 18    "Symbol"         [ ]
                                                 22    "Token"          [ ]
                                                 25    "Type"           [ ]
                                                 27    "Message"        [ ]
                                                 28    "Attr"           [ ]
                                                 28    "Content"        [ ]
                                                 31    "Church"         [ ]
                                                 
  34        "Koshucode.Baala.Core.Lexmap"        /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 33    "Lexmap"         [ "Base" | "Data" ]
                                                 33    "LexmapTrees"    [ "Base" | "Data" ]
                                                 33    "Message"        [ "Base" | "Data" ]
                                                 34    "Construct"      [ "Base" | "Data" ]
                                                 
  38        "Koshucode.Baala.Core.Relkit"        /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 33    "Message"        [ "Base" | "Data" ]
                                                 36    "Relkit"         [ "Base" | "Data" | "Lexmap" ]
                                                 37    "Construct"      [ "Base" | "Data" ]
                                                 38    "Run"            [ "Base" | "Data" ]
                                                 
  42        "Koshucode.Baala.Core.Relmap"        /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 33    "Message"        [ "Base" | "Data" ]
                                                 33    "Result"         [ "Base" | "Data" ]
                                                 34    "Option"         [ "Base" | "Data" ]
                                                 40    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Relkit" ]
                                                 41    "Rop"            [ "Base" | "Data" | "Lexmap" ]
                                                 41    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Relkit" ]
                                                 42    "Construct"      [ "Base" | "Data" | "Lexmap" | "Relkit" ]
                                                 42    "Global"         [ "Base" | "Data" ]
                                                 
  45        "Koshucode.Baala.Core.Assert"        /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 33    "Message"        [ "Base" | "Data" ]
                                                 33    "RelTable"       [ "Base" | "Data" ]
                                                 40    "Dataset"        [ "Base" | "Data" | "Relkit" ]
                                                 44    "Assert"         [ "Base" | "Data" | "Lexmap" | "Relmap" ]
                                                 45    "Run"            [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" ]
                                                 
  49        "Koshucode.Baala.Core.Resource"      /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 15    "Message"        [ "Base" ]
                                                 36    "Clause"         [ "Base" | "Data" | "Lexmap" ]
                                                 47    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" ]
                                                 48    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" ]
                                                 48    "Run"            [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" ]
                                                 49    "Read"           [ "Base" | "Data" | "Relmap" ]
                                                 
  50        "Koshucode.Baala.Core"               /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 34    "Message"        [ ]
                                                 35    "Lexmap"         [ ]
                                                 39    "Relkit"         [ ]
                                                 43    "Relmap"         [ ]
                                                 46    "Assert"         [ ]
                                                 50    "Resource"       [ ]
                                                 
  53        "Koshucode.Baala.Rop.Base"           /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 35    "Message"        [ "Base" ]
                                                 36    "Term"           [ "Base" | "Data" ]
                                                 52    "Define"         [ "Core" | "Data" ]
                                                 52    "Get"            [ "Base" | "Core" | "Data" ]
                                                 53    "Rop"            [ "Base" | "Core" ]
                                                 
  53        "Koshucode.Baala.Writer"             /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 52    "Csv"            [ "Base" | "Core" | "Data" ]
                                                 52    "Html"           [ "Base" | "Core" | "Data" ]
                                                 52    "Json"           [ "Core" | "Data" ]
                                                 52    "Judge"          [ "Base" | "Core" | "Data" ]
                                                 53    "Koshu"          [ "Base" | "Core" | "Data" ]
                                                 
  55        "Koshucode.Baala.Cop"                /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 0     "Replace"        [ ]
                                                 33    "Coxhand"        [ "Base" | "Data" ]
                                                 34    "Logic"          [ "Base" | "Data" ]
                                                 53    "Message"        [ ]
                                                 54    "Arith"          [ "Base" | "Data" ]
                                                 54    "List"           [ "Base" | "Data" ]
                                                 54    "Misc"           [ "Base" | "Data" ]
                                                 54    "Order"          [ "Base" | "Data" ]
                                                 54    "Text"           [ "Base" | "Data" ]
                                                 54    "Time"           [ "Base" | "Data" ]
                                                 54    "Type"           [ "Base" | "Data" ]
                                                 55    "Bundle"         [ "Base" | "Data" ]
                                                 
  56        "Koshucode.Baala"                    /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 14    "Base"           [ "Abort" | "Prelude" | "Syntax" | "Text" ]
                                                 32    "Data"           [ "Attr" | "Church" | "Content" | "Symbol" | "Token" | "Type" ]
                                                 51    "Core"           [ "Assert" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                 54    "Writer"         [ ]
                                                 56    "Cop"            [ ]
                                                 
  56        "Koshucode.Baala.Toolkit.Main"       /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 53    "KoshuRdf"       [ "Base" | "Core" | "Data" ]
                                                 53    "KoshuSyntax"    [ "Base" | "Core" | "Data" ]
                                                 56    "KoshuChange"    [ "Base" ]
                                                 56    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                 56    "KoshuMain"      [ "Base" | "Core" | "Data" | "Writer" ]
                                                 
  57        "Koshucode.Baala.Rop.Flat.Lattice"   /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 55    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" ]
                                                 56    "Restrict"       [ "Base" | "Base" | "Core" | "Data" ]
                                                 57    "Rop"            [ "Base" | "Core" ]
                                                 
  60        "Koshucode.Baala.Rop.Flat"           /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 15    "PoScale"        [ "Base" ]
                                                 52    "Message"        [ "Base" | "Core" | "Data" ]
                                                 55    "Gadget"         [ "Base" | "Base" | "Core" | "Data" ]
                                                 55    "Meta"           [ "Base" | "Base" | "Core" | "Data" ]
                                                 55    "Order"          [ "Base" | "Base" | "Core" | "Data" ]
                                                 55    "Resource"       [ "Base" | "Base" | "Core" | "Data" ]
                                                 55    "Source"         [ "Base" | "Base" | "Core" | "Data" ]
                                                 55    "Term"           [ "Base" | "Base" | "Core" | "Data" ]
                                                 56    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" ]
                                                 56    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                 58    "Lattice"        [ ]
                                                 59    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                 59    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                 60    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                 
  63        "Koshucode.Baala.Rop.Cox"            /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 0     "GeoDatumJp"     [ ]
                                                 53    "Message"        [ ]
                                                 55    "Get"            [ "Base" | "Base" | "Core" | "Data" ]
                                                 56    "Accessor"       [ "Base" | "Base" | "Core" | "Data" ]
                                                 56    "Calc"           [ "Base" | "Base" | "Core" | "Data" ]
                                                 56    "Filter"         [ "Base" | "Base" | "Core" | "Data" ]
                                                 56    "Gadget"         [ "Base" | "Base" | "Core" | "Data" ]
                                                 56    "Range"          [ "Base" | "Base" | "Core" | "Data" ]
                                                 62    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                 63    "Bundle"         [ "Core" | "Data" ]
                                                 
  63        "Koshucode.Baala.Rop.Nest"           /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 53    "Message"        [ "Base" | "Data" ]
                                                 55    "Flow"           [ "Base" | "Base" | "Core" | "Data" ]
                                                 56    "Confl"          [ "Base" | "Base" | "Core" | "Data" ]
                                                 62    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                 63    "Rop"            [ "Base" | "Core" | "Data" ]
                                                 
  64        "Koshucode.Baala.Rop"                /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 54    "Base"           [ ]
                                                 61    "Flat"           [ "Lattice" ]
                                                 64    "Cox"            [ ]
                                                 64    "Nest"           [ ]
                                                 
  65        "Koshucode.Baala.Toolkit.Library"    /rank /base            /import-dir
                                                 ----- ---------------- -----------------------------------------------------------------
                                                 0     "SimpleOption"   [ ]
                                                 0     "Version"        [ ]
                                                 33    "RDF"            [ "Data" ]
                                                 52    "Element"        [ "Base" | "Core" | "Data" ]
                                                 52    "Exit"           [ "Base" | "Core" | "Data" ]
                                                 52    "Input"          [ "Core" ]
                                                 55    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                 55    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                 65    "Global"         [ "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                 

=== rel

**
**  SUMMARY
**      29 judges on DIR-RANK
**      29 judges in total
**

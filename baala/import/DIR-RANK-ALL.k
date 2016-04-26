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

|-- DIR-RANK  /dir-rank 18  /dir "Koshucode.Baala.Data.Token"  /base-rank {= /rank /base /import-dir [ 0 | "Term" | [ ] ] [ 15 | "AngleText" | [ "Base" ] ] [ 15 | "Message" | [ "Base" ] ] [ 15 | "Token" | [ "Base" ] ] [ 16 | "Bracket" | [ "Base" ] ] [ 16 | "Next" | [ "Base" ] ] [ 17 | "Short" | [ "Base" ] ] [ 17 | "TokenLine" | [ "Base" ] ] [ 18 | "TokenClause" | [ "Base" ] ] [ 18 | "TokenTree" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 23  /dir "Koshucode.Baala.Data.Type"  /base-rank {= /rank /base /import-dir [ 15 | "Clock" | [ "Base" ] ] [ 15 | "Message" | [ "Base" ] ] [ 15 | "Para" | [ "Base" ] ] [ 16 | "Date" | [ "Base" ] ] [ 16 | "Decimal" | [ "Base" ] ] [ 17 | "Arithmetic" | [ "Base" ] ] [ 17 | "Time" | [ "Base" ] ] [ 20 | "Interp" | [ "Base" | "Token" ] ] [ 20 | "Judge" | [ "Base" | "Token" ] ] [ 20 | "Type" | [ "Base" | "Token" ] ] [ 21 | "About" | [ "Base" | "Token" ] ] [ 21 | "Head" | [ "Base" | "Token" ] ] [ 22 | "Rel" | [ "Base" | "Token" ] ] [ 23 | "Mono" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 26  /dir "Koshucode.Baala.Data.Content"  /base-rank {= /rank /base /import-dir [ 20 | "Message" | [ "Base" | "Token" ] ] [ 25 | "Class" | [ "Base" | "Token" | "Type" ] ] [ 25 | "Tree" | [ "Base" | "Token" | "Type" ] ] [ 26 | "BaalaC" | [ "Base" | "Token" | "Type" ] ] [ 26 | "Decode" | [ "Base" | "Token" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 29  /dir "Koshucode.Baala.Data.Church"  /base-rank {= /rank /base /import-dir [ 25 | "Message" | [ "Base" | "Token" | "Type" ] ] [ 26 | "Cox" | [ "Base" | "Token" ] ] [ 27 | "Cop" | [ "Base" | "Token" ] ] [ 28 | "Build" | [ "Base" | "Content" | "Token" ] ] [ 29 | "Run" | [ "Base" | "Content" | "Token" | "Type" ] ] =}
|-- DIR-RANK  /dir-rank 30  /dir "Koshucode.Baala.Data"  /base-rank {= /rank /base /import-dir [ 19 | "Token" | [ ] ] [ 24 | "Type" | [ ] ] [ 26 | "Message" | [ ] ] [ 27 | "Content" | [ ] ] [ 30 | "Church" | [ ] ] =}

|-- DIR-RANK  /dir-rank 35  /dir "Koshucode.Baala.Core.Attr"  /base-rank {= /rank /base /import-dir [ 32 | "Message" | [ "Base" | "Data" ] ] [ 33 | "AttrPos" | [ "Base" | "Data" ] ] [ 34 | "Attr" | [ "Base" | "Data" ] ] [ 34 | "Slot" | [ "Base" | "Data" ] ] [ 35 | "AttrEd" | [ "Base" | "Data" ] ] [ 35 | "Parse" | [ "Base" ] ] =}
|-- DIR-RANK  /dir-rank 38  /dir "Koshucode.Baala.Core.Lexmap"  /base-rank {= /rank /base /import-dir [ 37 | "Lexmap" | [ "Attr" | "Base" | "Data" ] ] [ 37 | "LexmapTrees" | [ "Attr" | "Base" | "Data" ] ] [ 38 | "Construct" | [ "Attr" | "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 42  /dir "Koshucode.Baala.Core.Relkit"  /base-rank {= /rank /base /import-dir [ 32 | "Message" | [ "Base" | "Data" ] ] [ 40 | "Relkit" | [ "Base" | "Data" | "Lexmap" ] ] [ 41 | "Construct" | [ "Base" | "Data" ] ] [ 42 | "Run" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 46  /dir "Koshucode.Baala.Core.Relmap"  /base-rank {= /rank /base /import-dir [ 32 | "Message" | [ "Base" | "Data" ] ] [ 32 | "Result" | [ "Base" | "Data" ] ] [ 33 | "Option" | [ "Base" | "Data" ] ] [ 44 | "Relmap" | [ "Base" | "Data" | "Lexmap" | "Relkit" ] ] [ 45 | "Rop" | [ "Attr" | "Base" | "Lexmap" ] ] [ 45 | "Specialize" | [ "Base" | "Data" | "Lexmap" | "Relkit" ] ] [ 46 | "Construct" | [ "Base" | "Data" | "Lexmap" | "Relkit" ] ] [ 46 | "Global" | [ "Base" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 49  /dir "Koshucode.Baala.Core.Assert"  /base-rank {= /rank /base /import-dir [ 32 | "Message" | [ "Base" | "Data" ] ] [ 32 | "RelTable" | [ "Base" | "Data" ] ] [ 44 | "Dataset" | [ "Base" | "Data" | "Relkit" ] ] [ 48 | "Assert" | [ "Base" | "Data" | "Lexmap" | "Relmap" ] ] [ 49 | "Run" | [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" ] ] =}

|-- DIR-RANK  /dir-rank 53  /dir "Koshucode.Baala.Core.Resource"  /base-rank {= /rank /base /import-dir [ 15 | "Message" | [ "Base" ] ] [ 40 | "Clause" | [ "Base" | "Data" | "Lexmap" ] ] [ 51 | "Resource" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" ] ] [ 52 | "Include" | [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" ] ] [ 52 | "Run" | [ "Assert" | "Attr" | "Base" | "Data" | "Lexmap" | "Relmap" ] ] [ 53 | "Read" | [ "Base" | "Data" | "Relmap" ] ] =}
|-- DIR-RANK  /dir-rank 54  /dir "Koshucode.Baala.Core"  /base-rank {= /rank /base /import-dir [ 33 | "Message" | [ ] ] [ 36 | "Attr" | [ ] ] [ 39 | "Lexmap" | [ ] ] [ 43 | "Relkit" | [ ] ] [ 47 | "Relmap" | [ ] ] [ 50 | "Assert" | [ ] ] [ 54 | "Resource" | [ ] ] =}
|-- DIR-RANK  /dir-rank 57  /dir "Koshucode.Baala.Rop.Base"  /base-rank {= /rank /base /import-dir [ 34 | "Message" | [ "Base" ] ] [ 35 | "Term" | [ "Base" | "Data" ] ] [ 56 | "Define" | [ "Core" ] ] [ 56 | "Get" | [ "Base" | "Core" | "Data" ] ] [ 57 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 57  /dir "Koshucode.Baala.Writer"  /base-rank {= /rank /base /import-dir [ 56 | "Csv" | [ "Base" | "Core" | "Data" ] ] [ 56 | "Html" | [ "Base" | "Core" | "Data" ] ] [ 56 | "Json" | [ "Core" | "Data" ] ] [ 56 | "Judge" | [ "Base" | "Core" | "Data" ] ] [ 57 | "Koshu" | [ "Base" | "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 59  /dir "Koshucode.Baala.Cop"  /base-rank {= /rank /base /import-dir [ 0 | "Replace" | [ ] ] [ 32 | "Coxhand" | [ "Base" | "Data" ] ] [ 33 | "Logic" | [ "Base" | "Data" ] ] [ 57 | "Message" | [ ] ] [ 58 | "Arith" | [ "Base" | "Data" ] ] [ 58 | "List" | [ "Base" | "Data" ] ] [ 58 | "Misc" | [ "Base" | "Data" ] ] [ 58 | "Order" | [ "Base" | "Data" ] ] [ 58 | "Text" | [ "Base" | "Data" ] ] [ 58 | "Time" | [ "Base" | "Data" ] ] [ 58 | "Type" | [ "Base" | "Data" ] ] [ 59 | "Bundle" | [ "Base" | "Data" ] ] =}

|-- DIR-RANK  /dir-rank 60  /dir "Koshucode.Baala"  /base-rank {= /rank /base /import-dir [ 14 | "Base" | [ "Abort" | "Prelude" | "Syntax" | "Text" ] ] [ 31 | "Data" | [ "Church" | "Content" | "Token" | "Type" ] ] [ 55 | "Core" | [ "Assert" | "Attr" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ] ] [ 58 | "Writer" | [ ] ] [ 60 | "Cop" | [ ] ] =}
|-- DIR-RANK  /dir-rank 60  /dir "Koshucode.Baala.Toolkit.Main"  /base-rank {= /rank /base /import-dir [ 57 | "KoshuRdf" | [ "Base" | "Core" | "Data" ] ] [ 57 | "KoshuSyntax" | [ "Base" | "Core" | "Data" ] ] [ 60 | "KoshuChange" | [ "Base" ] ] [ 60 | "KoshuFilter" | [ "Base" | "Core" | "Data" ] ] [ 60 | "KoshuMain" | [ "Base" | "Core" | "Data" | "Writer" ] ] =}
|-- DIR-RANK  /dir-rank 61  /dir "Koshucode.Baala.Rop.Flat.Lattice"  /base-rank {= /rank /base /import-dir [ 59 | "Tropashko" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 60 | "Restrict" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 61 | "Rop" | [ "Base" | "Core" ] ] =}
|-- DIR-RANK  /dir-rank 64  /dir "Koshucode.Baala.Rop.Flat"  /base-rank {= /rank /base /import-dir [ 15 | "PoScale" | [ "Base" ] ] [ 56 | "Message" | [ "Base" | "Core" | "Data" ] ] [ 59 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 59 | "Meta" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 59 | "Order" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 59 | "Resource" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 59 | "Source" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 59 | "Term" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 60 | "Peripheral" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 60 | "TermGadget" | [ "Base" | "Core" | "Data" ] ] [ 62 | "Lattice" | [ ] ] [ 63 | "Check" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 63 | "Control" | [ "Base" | "Base" | "Core" | "Data" | "Lattice" ] ] [ 64 | "Bundle" | [ "Base" | "Core" | "Data" | "Lattice" ] ] =}
|-- DIR-RANK  /dir-rank 67  /dir "Koshucode.Baala.Rop.Cox"  /base-rank {= /rank /base /import-dir [ 0 | "GeoDatumJp" | [ ] ] [ 57 | "Message" | [ ] ] [ 59 | "Get" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 60 | "Accessor" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 60 | "Calc" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 60 | "Filter" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 60 | "Gadget" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 60 | "Range" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 66 | "Empty" | [ "Base" | "Base" | "Core" | "Data" | "Flat" ] ] [ 67 | "Bundle" | [ "Core" | "Data" ] ] =}
*** 25

|-- DIR-RANK  /dir-rank 67  /dir "Koshucode.Baala.Rop.Nest"  /base-rank {= /rank /base /import-dir [ 57 | "Message" | [ "Base" | "Data" ] ] [ 59 | "Flow" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 60 | "Confl" | [ "Base" | "Base" | "Core" | "Data" ] ] [ 66 | "Deriv" | [ "Base" | "Base" | "Core" | "Data" | "Flat" ] ] [ 67 | "Rop" | [ "Base" | "Core" | "Data" ] ] =}
|-- DIR-RANK  /dir-rank 68  /dir "Koshucode.Baala.Rop"  /base-rank {= /rank /base /import-dir [ 58 | "Base" | [ ] ] [ 65 | "Flat" | [ "Lattice" ] ] [ 68 | "Cox" | [ ] ] [ 68 | "Nest" | [ ] ] =}
|-- DIR-RANK  /dir-rank 69  /dir "Koshucode.Baala.Toolkit.Library"  /base-rank {= /rank /base /import-dir [ 0 | "SimpleOption" | [ ] ] [ 0 | "Version" | [ ] ] [ 32 | "RDF" | [ "Data" ] ] [ 56 | "Element" | [ "Base" | "Core" | "Data" ] ] [ 56 | "Exit" | [ "Base" | "Core" | "Data" ] ] [ 56 | "Input" | [ "Core" ] ] [ 59 | "Change" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 59 | "Run" | [ "Base" | "Core" | "Data" | "Writer" ] ] [ 69 | "Global" | [ "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ] ] =}

*** 28 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                                 /base-rank
  --------- ------------------------------------ --------------------------------------------------------------------------------------------
  2         "Koshucode.Baala.Base.Prelude"       /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 0     "Class"          [ ]
                                                 0     "Import"         [ ]
                                                 1     "Assoc"          [ ]
                                                 1     "List"           [ ]
                                                 1     "Pair"           [ ]
                                                 1     "Snip"           [ ]
                                                 2     "Order"          [ ]
                                                 
  5         "Koshucode.Baala.Base.Text"          /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 0     "Comment"        [ ]
                                                 0     "Unicode"        [ ]
                                                 4     "IOPoint"        [ "Prelude" ]
                                                 4     "TextTable"      [ "Prelude" ]
                                                 4     "Utility"        [ "Prelude" ]
                                                 4     "Write"          [ "Prelude" ]
                                                 5     "CodePt"         [ "Prelude" ]
                                                 5     "Http"           [ "Prelude" ]
                                                 
  10        "Koshucode.Baala.Base.Abort"         /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 7     "Reason"         [ "Text" ]
                                                 8     "Abortable"      [ "Prelude" | "Text" ]
                                                 8     "Report"         [ "Prelude" | "Text" ]
                                                 10    "Message"        [ "Abort" ]
                                                 
  12        "Koshucode.Baala.Base.Syntax"        /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 10    "Message"        [ "Abort" | "Prelude" | "Text" ]
                                                 11    "Line"           [ "Abort" | "Prelude" | "Text" ]
                                                 11    "Tree"           [ "Abort" | "Prelude" | "Text" ]
                                                 12    "Clause"         [ "Prelude" | "Text" ]
                                                 12    "Infix"          [ "Prelude" ]
                                                 
  13        "Koshucode.Baala.Base"               /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 3     "Prelude"        [ ]
                                                 6     "Text"           [ ]
                                                 9     "Abort"          [ ]
                                                 11    "Message"        [ ]
                                                 13    "Syntax"         [ ]
                                                 
  18        "Koshucode.Baala.Data.Token"         /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 0     "Term"           [ ]
                                                 15    "AngleText"      [ "Base" ]
                                                 15    "Message"        [ "Base" ]
                                                 15    "Token"          [ "Base" ]
                                                 16    "Bracket"        [ "Base" ]
                                                 16    "Next"           [ "Base" ]
                                                 17    "Short"          [ "Base" ]
                                                 17    "TokenLine"      [ "Base" ]
                                                 18    "TokenClause"    [ "Base" ]
                                                 18    "TokenTree"      [ "Base" ]
                                                 
  23        "Koshucode.Baala.Data.Type"          /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 15    "Clock"          [ "Base" ]
                                                 15    "Message"        [ "Base" ]
                                                 15    "Para"           [ "Base" ]
                                                 16    "Date"           [ "Base" ]
                                                 16    "Decimal"        [ "Base" ]
                                                 17    "Arithmetic"     [ "Base" ]
                                                 17    "Time"           [ "Base" ]
                                                 20    "Interp"         [ "Base" | "Token" ]
                                                 20    "Judge"          [ "Base" | "Token" ]
                                                 20    "Type"           [ "Base" | "Token" ]
                                                 21    "About"          [ "Base" | "Token" ]
                                                 21    "Head"           [ "Base" | "Token" ]
                                                 22    "Rel"            [ "Base" | "Token" ]
                                                 23    "Mono"           [ "Base" ]
                                                 
  26        "Koshucode.Baala.Data.Content"       /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 20    "Message"        [ "Base" | "Token" ]
                                                 25    "Class"          [ "Base" | "Token" | "Type" ]
                                                 25    "Tree"           [ "Base" | "Token" | "Type" ]
                                                 26    "BaalaC"         [ "Base" | "Token" | "Type" ]
                                                 26    "Decode"         [ "Base" | "Token" | "Type" ]
                                                 
  29        "Koshucode.Baala.Data.Church"        /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 25    "Message"        [ "Base" | "Token" | "Type" ]
                                                 26    "Cox"            [ "Base" | "Token" ]
                                                 27    "Cop"            [ "Base" | "Token" ]
                                                 28    "Build"          [ "Base" | "Content" | "Token" ]
                                                 29    "Run"            [ "Base" | "Content" | "Token" | "Type" ]
                                                 
  30        "Koshucode.Baala.Data"               /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 19    "Token"          [ ]
                                                 24    "Type"           [ ]
                                                 26    "Message"        [ ]
                                                 27    "Content"        [ ]
                                                 30    "Church"         [ ]
                                                 
  35        "Koshucode.Baala.Core.Attr"          /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 32    "Message"        [ "Base" | "Data" ]
                                                 33    "AttrPos"        [ "Base" | "Data" ]
                                                 34    "Attr"           [ "Base" | "Data" ]
                                                 34    "Slot"           [ "Base" | "Data" ]
                                                 35    "AttrEd"         [ "Base" | "Data" ]
                                                 35    "Parse"          [ "Base" ]
                                                 
  38        "Koshucode.Baala.Core.Lexmap"        /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 37    "Lexmap"         [ "Attr" | "Base" | "Data" ]
                                                 37    "LexmapTrees"    [ "Attr" | "Base" | "Data" ]
                                                 38    "Construct"      [ "Attr" | "Base" | "Data" ]
                                                 
  42        "Koshucode.Baala.Core.Relkit"        /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 32    "Message"        [ "Base" | "Data" ]
                                                 40    "Relkit"         [ "Base" | "Data" | "Lexmap" ]
                                                 41    "Construct"      [ "Base" | "Data" ]
                                                 42    "Run"            [ "Base" | "Data" ]
                                                 
  46        "Koshucode.Baala.Core.Relmap"        /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 32    "Message"        [ "Base" | "Data" ]
                                                 32    "Result"         [ "Base" | "Data" ]
                                                 33    "Option"         [ "Base" | "Data" ]
                                                 44    "Relmap"         [ "Base" | "Data" | "Lexmap" | "Relkit" ]
                                                 45    "Rop"            [ "Attr" | "Base" | "Lexmap" ]
                                                 45    "Specialize"     [ "Base" | "Data" | "Lexmap" | "Relkit" ]
                                                 46    "Construct"      [ "Base" | "Data" | "Lexmap" | "Relkit" ]
                                                 46    "Global"         [ "Base" | "Data" ]
                                                 
  49        "Koshucode.Baala.Core.Assert"        /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 32    "Message"        [ "Base" | "Data" ]
                                                 32    "RelTable"       [ "Base" | "Data" ]
                                                 44    "Dataset"        [ "Base" | "Data" | "Relkit" ]
                                                 48    "Assert"         [ "Base" | "Data" | "Lexmap" | "Relmap" ]
                                                 49    "Run"            [ "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" ]
                                                 
  53        "Koshucode.Baala.Core.Resource"      /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 15    "Message"        [ "Base" ]
                                                 40    "Clause"         [ "Base" | "Data" | "Lexmap" ]
                                                 51    "Resource"       [ "Assert" | "Base" | "Data" | "Lexmap" | "Relkit" | "Relmap" ]
                                                 52    "Include"        [ "Assert" | "Base" | "Data" | "Lexmap" | "Relmap" ]
                                                 52    "Run"            [ "Assert" | "Attr" | "Base" | "Data" | "Lexmap" | "Relmap" ]
                                                 53    "Read"           [ "Base" | "Data" | "Relmap" ]
                                                 
  54        "Koshucode.Baala.Core"               /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 33    "Message"        [ ]
                                                 36    "Attr"           [ ]
                                                 39    "Lexmap"         [ ]
                                                 43    "Relkit"         [ ]
                                                 47    "Relmap"         [ ]
                                                 50    "Assert"         [ ]
                                                 54    "Resource"       [ ]
                                                 
  57        "Koshucode.Baala.Rop.Base"           /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 34    "Message"        [ "Base" ]
                                                 35    "Term"           [ "Base" | "Data" ]
                                                 56    "Define"         [ "Core" ]
                                                 56    "Get"            [ "Base" | "Core" | "Data" ]
                                                 57    "Rop"            [ "Base" | "Core" ]
                                                 
  57        "Koshucode.Baala.Writer"             /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 56    "Csv"            [ "Base" | "Core" | "Data" ]
                                                 56    "Html"           [ "Base" | "Core" | "Data" ]
                                                 56    "Json"           [ "Core" | "Data" ]
                                                 56    "Judge"          [ "Base" | "Core" | "Data" ]
                                                 57    "Koshu"          [ "Base" | "Core" | "Data" ]
                                                 
  59        "Koshucode.Baala.Cop"                /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 0     "Replace"        [ ]
                                                 32    "Coxhand"        [ "Base" | "Data" ]
                                                 33    "Logic"          [ "Base" | "Data" ]
                                                 57    "Message"        [ ]
                                                 58    "Arith"          [ "Base" | "Data" ]
                                                 58    "List"           [ "Base" | "Data" ]
                                                 58    "Misc"           [ "Base" | "Data" ]
                                                 58    "Order"          [ "Base" | "Data" ]
                                                 58    "Text"           [ "Base" | "Data" ]
                                                 58    "Time"           [ "Base" | "Data" ]
                                                 58    "Type"           [ "Base" | "Data" ]
                                                 59    "Bundle"         [ "Base" | "Data" ]
                                                 
  60        "Koshucode.Baala"                    /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 14    "Base"           [ "Abort" | "Prelude" | "Syntax" | "Text" ]
                                                 31    "Data"           [ "Church" | "Content" | "Token" | "Type" ]
                                                 55    "Core"           [ "Assert" | "Attr" | "Lexmap" | "Relkit" | "Relmap" | "Resource" ]
                                                 58    "Writer"         [ ]
                                                 60    "Cop"            [ ]
                                                 
  60        "Koshucode.Baala.Toolkit.Main"       /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 57    "KoshuRdf"       [ "Base" | "Core" | "Data" ]
                                                 57    "KoshuSyntax"    [ "Base" | "Core" | "Data" ]
                                                 60    "KoshuChange"    [ "Base" ]
                                                 60    "KoshuFilter"    [ "Base" | "Core" | "Data" ]
                                                 60    "KoshuMain"      [ "Base" | "Core" | "Data" | "Writer" ]
                                                 
  61        "Koshucode.Baala.Rop.Flat.Lattice"   /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 59    "Tropashko"      [ "Base" | "Base" | "Core" | "Data" ]
                                                 60    "Restrict"       [ "Base" | "Base" | "Core" | "Data" ]
                                                 61    "Rop"            [ "Base" | "Core" ]
                                                 
  64        "Koshucode.Baala.Rop.Flat"           /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 15    "PoScale"        [ "Base" ]
                                                 56    "Message"        [ "Base" | "Core" | "Data" ]
                                                 59    "Gadget"         [ "Base" | "Base" | "Core" | "Data" ]
                                                 59    "Meta"           [ "Base" | "Base" | "Core" | "Data" ]
                                                 59    "Order"          [ "Base" | "Base" | "Core" | "Data" ]
                                                 59    "Resource"       [ "Base" | "Base" | "Core" | "Data" ]
                                                 59    "Source"         [ "Base" | "Base" | "Core" | "Data" ]
                                                 59    "Term"           [ "Base" | "Base" | "Core" | "Data" ]
                                                 60    "Peripheral"     [ "Base" | "Base" | "Core" | "Data" ]
                                                 60    "TermGadget"     [ "Base" | "Core" | "Data" ]
                                                 62    "Lattice"        [ ]
                                                 63    "Check"          [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                 63    "Control"        [ "Base" | "Base" | "Core" | "Data" | "Lattice" ]
                                                 64    "Bundle"         [ "Base" | "Core" | "Data" | "Lattice" ]
                                                 
  67        "Koshucode.Baala.Rop.Cox"            /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 0     "GeoDatumJp"     [ ]
                                                 57    "Message"        [ ]
                                                 59    "Get"            [ "Base" | "Base" | "Core" | "Data" ]
                                                 60    "Accessor"       [ "Base" | "Base" | "Core" | "Data" ]
                                                 60    "Calc"           [ "Base" | "Base" | "Core" | "Data" ]
                                                 60    "Filter"         [ "Base" | "Base" | "Core" | "Data" ]
                                                 60    "Gadget"         [ "Base" | "Base" | "Core" | "Data" ]
                                                 60    "Range"          [ "Base" | "Base" | "Core" | "Data" ]
                                                 66    "Empty"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                 67    "Bundle"         [ "Core" | "Data" ]
                                                 
  67        "Koshucode.Baala.Rop.Nest"           /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 57    "Message"        [ "Base" | "Data" ]
                                                 59    "Flow"           [ "Base" | "Base" | "Core" | "Data" ]
                                                 60    "Confl"          [ "Base" | "Base" | "Core" | "Data" ]
                                                 66    "Deriv"          [ "Base" | "Base" | "Core" | "Data" | "Flat" ]
                                                 67    "Rop"            [ "Base" | "Core" | "Data" ]
                                                 
  68        "Koshucode.Baala.Rop"                /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 58    "Base"           [ ]
                                                 65    "Flat"           [ "Lattice" ]
                                                 68    "Cox"            [ ]
                                                 68    "Nest"           [ ]
                                                 
  69        "Koshucode.Baala.Toolkit.Library"    /rank /base            /import-dir
                                                 ----- ---------------- ---------------------------------------------------------------------
                                                 0     "SimpleOption"   [ ]
                                                 0     "Version"        [ ]
                                                 32    "RDF"            [ "Data" ]
                                                 56    "Element"        [ "Base" | "Core" | "Data" ]
                                                 56    "Exit"           [ "Base" | "Core" | "Data" ]
                                                 56    "Input"          [ "Core" ]
                                                 59    "Change"         [ "Base" | "Core" | "Data" | "Writer" ]
                                                 59    "Run"            [ "Base" | "Core" | "Data" | "Writer" ]
                                                 69    "Global"         [ "Cop" | "Core" | "Cox" | "Data" | "Flat" | "Nest" ]
                                                 

=== rel

**
**  SUMMARY
**      28 judges on DIR-RANK
**      28 judges in total
**

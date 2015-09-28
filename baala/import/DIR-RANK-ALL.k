** -*- koshu -*-
**
**  INPUT
**    dir-rank.k
**    ../base/data/IMPORT.k
**    ../calculator/data/IMPORT.k
**    ../content/data/IMPORT.k
**    ../core/data/IMPORT.k
**    ../data/data/IMPORT.k
**    ../operator/data/IMPORT.k
**    ../toolkit/data/IMPORT.k
**    ../writer/data/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Base.Prelude  /base-rank {| /rank /base /import-dir [ 0 | 'Class | [ ] ] [ 0 | 'Import | [ ] ] [ 1 | 'Assoc | [ ] ] [ 1 | 'List | [ ] ] [ 1 | 'Pair | [ ] ] [ 1 | 'Snip | [ ] ] [ 2 | 'Order | [ ] ] |}
|-- DIR-RANK  /dir-rank 5  /dir 'Koshucode.Baala.Base.Text  /base-rank {| /rank /base /import-dir [ 0 | 'Comment | [ ] ] [ 0 | 'Unicode | [ ] ] [ 4 | 'Http | [ 'Prelude ] ] [ 4 | 'IOPoint | [ 'Prelude ] ] [ 4 | 'TextTable | [ 'Prelude ] ] [ 4 | 'Utility | [ 'Prelude ] ] [ 4 | 'Write | [ 'Prelude ] ] [ 5 | 'CodePt | [ 'Prelude ] ] |}
|-- DIR-RANK  /dir-rank 8  /dir 'Koshucode.Baala.Base.Abort  /base-rank {| /rank /base /import-dir [ 7 | 'Reason | [ 'Text ] ] [ 8 | 'Abortable | [ 'Prelude | 'Text ] ] [ 8 | 'Report | [ 'Prelude | 'Text ] ] |}
|-- DIR-RANK  /dir-rank 12  /dir 'Koshucode.Baala.Base.Syntax  /base-rank {| /rank /base /import-dir [ 0 | 'Line | [ ] ] [ 11 | 'Code | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Tree | [ 'Abort | 'Prelude | 'Text ] ] [ 12 | 'Infix | [ 'Prelude ] ] |}
|-- DIR-RANK  /dir-rank 13  /dir 'Koshucode.Baala.Base  /base-rank {| /rank /base /import-dir [ 3 | 'Prelude | [ ] ] [ 6 | 'Text | [ ] ] [ 9 | 'Abort | [ ] ] [ 10 | 'Message | [ 'Abort | 'Prelude | 'Text ] ] [ 13 | 'Syntax | [ ] ] |}

|-- DIR-RANK  /dir-rank 18  /dir 'Koshucode.Baala.Data.Token  /base-rank {| /rank /base /import-dir [ 0 | 'Term | [ ] ] [ 15 | 'AngleText | [ 'Base ] ] [ 15 | 'Token | [ 'Base ] ] [ 16 | 'Short | [ 'Base ] ] [ 17 | 'TokenLine | [ 'Base ] ] [ 18 | 'TokenClause | [ 'Base ] ] [ 18 | 'TokenTree | [ 'Base ] ] |}
|-- DIR-RANK  /dir-rank 23  /dir 'Koshucode.Baala.Data.Type  /base-rank {| /rank /base /import-dir [ 15 | 'Clock | [ 'Base ] ] [ 15 | 'Date | [ 'Base ] ] [ 15 | 'Decimal | [ 'Base ] ] [ 15 | 'Para | [ 'Base ] ] [ 16 | 'Time | [ 'Base ] ] [ 20 | 'Interp | [ 'Base | 'Token ] ] [ 20 | 'Judge | [ 'Base | 'Token ] ] [ 20 | 'Type | [ 'Base | 'Token ] ] [ 21 | 'About | [ 'Base | 'Token ] ] [ 21 | 'Head | [ 'Base | 'Token ] ] [ 22 | 'Rel | [ 'Base | 'Token ] ] [ 23 | 'Mono | [ 'Base ] ] |}
|-- DIR-RANK  /dir-rank 27  /dir 'Koshucode.Baala.Data.Content  /base-rank {| /rank /base /import-dir [ 26 | 'Class | [ 'Base | 'Token | 'Type ] ] [ 26 | 'Tree | [ 'Base | 'Token | 'Type ] ] [ 27 | 'Literal | [ 'Base | 'Token | 'Type ] ] |}
|-- DIR-RANK  /dir-rank 30  /dir 'Koshucode.Baala.Data.Church  /base-rank {| /rank /base /import-dir [ 26 | 'Cox | [ 'Base | 'Token ] ] [ 27 | 'Cop | [ 'Base | 'Token ] ] [ 29 | 'Build | [ 'Base | 'Content | 'Token ] ] [ 30 | 'Run | [ 'Base | 'Content | 'Token | 'Type ] ] |}
|-- DIR-RANK  /dir-rank 31  /dir 'Koshucode.Baala.Data  /base-rank {| /rank /base /import-dir [ 19 | 'Token | [ ] ] [ 24 | 'Type | [ ] ] [ 25 | 'Message | [ 'Base | 'Token | 'Type ] ] [ 28 | 'Content | [ ] ] [ 31 | 'Church | [ ] ] |}

|-- DIR-RANK  /dir-rank 38  /dir 'Koshucode.Baala.Core.Lexmap  /base-rank {| /rank /base /import-dir [ 34 | 'AttrPos | [ 'Base | 'Data ] ] [ 35 | 'Attr | [ 'Base | 'Data ] ] [ 35 | 'Slot | [ 'Base | 'Data ] ] [ 36 | 'AttrEd | [ 'Base | 'Data ] ] [ 36 | 'Lexmap | [ 'Base | 'Data ] ] [ 37 | 'LexmapTrees | [ 'Base | 'Data ] ] [ 38 | 'Construct | [ 'Base | 'Data ] ] |}
|-- DIR-RANK  /dir-rank 42  /dir 'Koshucode.Baala.Core.Relkit  /base-rank {| /rank /base /import-dir [ 40 | 'Relkit | [ 'Base | 'Data | 'Lexmap ] ] [ 41 | 'Construct | [ 'Base | 'Data ] ] [ 42 | 'Run | [ 'Base | 'Data ] ] |}
|-- DIR-RANK  /dir-rank 46  /dir 'Koshucode.Baala.Core.Relmap  /base-rank {| /rank /base /import-dir [ 33 | 'Result | [ 'Base | 'Data ] ] [ 34 | 'Option | [ 'Base | 'Data ] ] [ 44 | 'Relmap | [ 'Base | 'Data | 'Lexmap | 'Relkit ] ] [ 45 | 'Rop | [ 'Base | 'Lexmap ] ] [ 45 | 'Specialize | [ 'Base | 'Data | 'Lexmap | 'Relkit ] ] [ 46 | 'Construct | [ 'Base | 'Data | 'Lexmap | 'Relkit ] ] [ 46 | 'Global | [ 'Base | 'Data ] ] |}
|-- DIR-RANK  /dir-rank 49  /dir 'Koshucode.Baala.Core.Assert  /base-rank {| /rank /base /import-dir [ 33 | 'RelTable | [ 'Base | 'Data ] ] [ 44 | 'Dataset | [ 'Base | 'Data | 'Relkit ] ] [ 48 | 'Assert | [ 'Base | 'Data | 'Lexmap | 'Relmap ] ] [ 49 | 'Run | [ 'Base | 'Data | 'Lexmap | 'Relkit | 'Relmap ] ] |}
|-- DIR-RANK  /dir-rank 53  /dir 'Koshucode.Baala.Core.Resource  /base-rank {| /rank /base /import-dir [ 40 | 'Clause | [ 'Base | 'Data | 'Lexmap ] ] [ 51 | 'Resource | [ 'Assert | 'Base | 'Data | 'Lexmap | 'Relkit | 'Relmap ] ] [ 52 | 'Include | [ 'Assert | 'Base | 'Data | 'Lexmap | 'Relmap ] ] [ 52 | 'Run | [ 'Assert | 'Base | 'Data | 'Lexmap | 'Relmap ] ] [ 53 | 'Quoter | [ 'Base | 'Data | 'Lexmap ] ] [ 53 | 'Read | [ 'Base | 'Data | 'Relmap ] ] |}

|-- DIR-RANK  /dir-rank 54  /dir 'Koshucode.Baala.Core  /base-rank {| /rank /base /import-dir [ 33 | 'Message | [ 'Base | 'Data ] ] [ 39 | 'Lexmap | [ ] ] [ 43 | 'Relkit | [ ] ] [ 47 | 'Relmap | [ ] ] [ 50 | 'Assert | [ ] ] [ 54 | 'Resource | [ ] ] |}
|-- DIR-RANK  /dir-rank 57  /dir 'Koshucode.Baala.Op.Cop  /base-rank {| /rank /base /import-dir [ 33 | 'Coxhand | [ 'Base | 'Data ] ] [ 34 | 'Logic | [ 'Base | 'Data ] ] [ 57 | 'Arith | [ 'Base | 'Data ] ] [ 57 | 'List | [ 'Base | 'Data ] ] [ 57 | 'Misc | [ 'Base | 'Data ] ] [ 57 | 'Order | [ 'Base | 'Data ] ] [ 57 | 'Time | [ 'Base | 'Data ] ] [ 57 | 'Type | [ 'Base | 'Data ] ] |}
|-- DIR-RANK  /dir-rank 57  /dir 'Koshucode.Baala.Writer  /base-rank {| /rank /base /import-dir [ 56 | 'Csv | [ 'Base | 'Core | 'Data ] ] [ 56 | 'Html | [ 'Base | 'Core | 'Data ] ] [ 56 | 'Json | [ 'Core | 'Data ] ] [ 56 | 'Judge | [ 'Base | 'Core | 'Data ] ] [ 57 | 'Koshu | [ 'Base | 'Core | 'Data ] ] |}
|-- DIR-RANK  /dir-rank 58  /dir 'Koshucode.Baala.Op.Builtin  /base-rank {| /rank /base /import-dir [ 56 | 'Define | [ 'Base | 'Core ] ] [ 57 | 'Rop | [ 'Base | 'Core ] ] [ 57 | 'Term | [ 'Base | 'Data ] ] [ 58 | 'Get | [ 'Base | 'Core | 'Data ] ] |}
|-- DIR-RANK  /dir-rank 59  /dir 'Koshucode.Baala.Type  /base-rank {| /rank /base /import-dir [ 59 | 'Vanilla | [ 'Base | 'Core | 'Data | 'Writer ] ] |}

|-- DIR-RANK  /dir-rank 61  /dir 'Koshucode.Baala.Toolkit.Library  /base-rank {| /rank /base /import-dir [ 0 | 'SimpleOption | [ ] ] [ 0 | 'Version | [ ] ] [ 33 | 'RDF | [ 'Data ] ] [ 56 | 'Element | [ 'Base | 'Core | 'Data ] ] [ 56 | 'Exit | [ 'Base | 'Core | 'Data ] ] [ 59 | 'Run | [ 'Base | 'Core | 'Data | 'Writer ] ] [ 60 | 'Input | [ 'Core | 'Data ] ] [ 61 | 'Change | [ 'Base | 'Data | 'Writer ] ] |}
|-- DIR-RANK  /dir-rank 62  /dir 'Koshucode.Baala.Op.Lattice  /base-rank {| /rank /base /import-dir [ 60 | 'Tropashko | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 61 | 'Restrict | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 62 | 'Rop | [ 'Builtin | 'Core | 'Data ] ] |}
|-- DIR-RANK  /dir-rank 62  /dir 'Koshucode.Baala.Toolkit.Main  /base-rank {| /rank /base /import-dir [ 60 | 'KoshuFilter | [ 'Base | 'Core | 'Data ] ] [ 60 | 'KoshuMain | [ 'Base | 'Core | 'Data | 'Writer ] ] [ 60 | 'KoshuRdf | [ 'Base | 'Data ] ] [ 60 | 'KoshuSyntax | [ 'Base | 'Core | 'Data ] ] [ 62 | 'KoshuChange | [ 'Base ] ] |}
|-- DIR-RANK  /dir-rank 64  /dir 'Koshucode.Baala.Op.Cox  /base-rank {| /rank /base /import-dir [ 0 | 'GeoDatumJp | [ ] ] [ 60 | 'Get | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 61 | 'Accessor | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 61 | 'Calc | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 61 | 'Filter | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 61 | 'Gadget | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 61 | 'Range | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 64 | 'Empty | [ 'Base | 'Builtin | 'Core | 'Data | 'Lattice ] ] |}
|-- DIR-RANK  /dir-rank 65  /dir 'Koshucode.Baala.Op.Nest  /base-rank {| /rank /base /import-dir [ 60 | 'Flow | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 61 | 'Confl | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 64 | 'Deriv | [ 'Base | 'Builtin | 'Core | 'Data | 'Lattice ] ] [ 65 | 'Rop | [ 'Builtin | 'Core | 'Data ] ] |}
*** 25

|-- DIR-RANK  /dir-rank 67  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 14 | 'Base | [ 'Abort | 'Prelude | 'Syntax | 'Text ] ] [ 32 | 'Data | [ 'Church | 'Content | 'Token | 'Type ] ] [ 55 | 'Core | [ 'Assert | 'Lexmap | 'Relkit | 'Relmap | 'Resource ] ] [ 58 | 'Writer | [ ] ] [ 67 | 'Op | [ 'Builtin | 'Lattice | 'Nest ] ] |}
|-- DIR-RANK  /dir-rank 68  /dir 'Koshucode.Baala.Op  /base-rank {| /rank /base /import-dir [ 15 | 'DepRank | [ 'Base ] ] [ 56 | 'Message | [ 'Base | 'Core | 'Data ] ] [ 58 | 'Cop | [ ] ] [ 59 | 'Builtin | [ ] ] [ 60 | 'Gadget | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 60 | 'Meta | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 60 | 'Quoter | [ 'Core ] ] [ 60 | 'Resource | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 60 | 'Source | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 60 | 'Term | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 61 | 'Peripheral | [ 'Base | 'Builtin | 'Core | 'Data ] ] [ 61 | 'TermGadget | [ 'Builtin | 'Core | 'Data ] ] [ 63 | 'Lattice | [ ] ] [ 64 | 'Check | [ 'Base | 'Builtin | 'Core | 'Data | 'Lattice ] ] [ 64 | 'Control | [ 'Base | 'Builtin | 'Core | 'Data | 'Lattice ] ] [ 65 | 'Cox | [ ] ] [ 66 | 'Nest | [ ] ] [ 68 | 'Global | [ 'Base | 'Cop | 'Core | 'Cox | 'Data | 'Op ] ] |}

*** 27 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                               /base-rank
  --------- ---------------------------------- ---------------------------------------------------------------------------------
  2         'Koshucode.Baala.Base.Prelude      /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               0     'Class          [ ]
                                               0     'Import         [ ]
                                               1     'Assoc          [ ]
                                               1     'List           [ ]
                                               1     'Pair           [ ]
                                               1     'Snip           [ ]
                                               2     'Order          [ ]
                                               
  5         'Koshucode.Baala.Base.Text         /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               0     'Comment        [ ]
                                               0     'Unicode        [ ]
                                               4     'Http           [ 'Prelude ]
                                               4     'IOPoint        [ 'Prelude ]
                                               4     'TextTable      [ 'Prelude ]
                                               4     'Utility        [ 'Prelude ]
                                               4     'Write          [ 'Prelude ]
                                               5     'CodePt         [ 'Prelude ]
                                               
  8         'Koshucode.Baala.Base.Abort        /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               7     'Reason         [ 'Text ]
                                               8     'Abortable      [ 'Prelude | 'Text ]
                                               8     'Report         [ 'Prelude | 'Text ]
                                               
  12        'Koshucode.Baala.Base.Syntax       /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               0     'Line           [ ]
                                               11    'Code           [ 'Abort | 'Prelude | 'Text ]
                                               11    'Tree           [ 'Abort | 'Prelude | 'Text ]
                                               12    'Infix          [ 'Prelude ]
                                               
  13        'Koshucode.Baala.Base              /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               3     'Prelude        [ ]
                                               6     'Text           [ ]
                                               9     'Abort          [ ]
                                               10    'Message        [ 'Abort | 'Prelude | 'Text ]
                                               13    'Syntax         [ ]
                                               
  18        'Koshucode.Baala.Data.Token        /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               0     'Term           [ ]
                                               15    'AngleText      [ 'Base ]
                                               15    'Token          [ 'Base ]
                                               16    'Short          [ 'Base ]
                                               17    'TokenLine      [ 'Base ]
                                               18    'TokenClause    [ 'Base ]
                                               18    'TokenTree      [ 'Base ]
                                               
  23        'Koshucode.Baala.Data.Type         /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               15    'Clock          [ 'Base ]
                                               15    'Date           [ 'Base ]
                                               15    'Decimal        [ 'Base ]
                                               15    'Para           [ 'Base ]
                                               16    'Time           [ 'Base ]
                                               20    'Interp         [ 'Base | 'Token ]
                                               20    'Judge          [ 'Base | 'Token ]
                                               20    'Type           [ 'Base | 'Token ]
                                               21    'About          [ 'Base | 'Token ]
                                               21    'Head           [ 'Base | 'Token ]
                                               22    'Rel            [ 'Base | 'Token ]
                                               23    'Mono           [ 'Base ]
                                               
  27        'Koshucode.Baala.Data.Content      /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               26    'Class          [ 'Base | 'Token | 'Type ]
                                               26    'Tree           [ 'Base | 'Token | 'Type ]
                                               27    'Literal        [ 'Base | 'Token | 'Type ]
                                               
  30        'Koshucode.Baala.Data.Church       /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               26    'Cox            [ 'Base | 'Token ]
                                               27    'Cop            [ 'Base | 'Token ]
                                               29    'Build          [ 'Base | 'Content | 'Token ]
                                               30    'Run            [ 'Base | 'Content | 'Token | 'Type ]
                                               
  31        'Koshucode.Baala.Data              /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               19    'Token          [ ]
                                               24    'Type           [ ]
                                               25    'Message        [ 'Base | 'Token | 'Type ]
                                               28    'Content        [ ]
                                               31    'Church         [ ]
                                               
  38        'Koshucode.Baala.Core.Lexmap       /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               34    'AttrPos        [ 'Base | 'Data ]
                                               35    'Attr           [ 'Base | 'Data ]
                                               35    'Slot           [ 'Base | 'Data ]
                                               36    'AttrEd         [ 'Base | 'Data ]
                                               36    'Lexmap         [ 'Base | 'Data ]
                                               37    'LexmapTrees    [ 'Base | 'Data ]
                                               38    'Construct      [ 'Base | 'Data ]
                                               
  42        'Koshucode.Baala.Core.Relkit       /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               40    'Relkit         [ 'Base | 'Data | 'Lexmap ]
                                               41    'Construct      [ 'Base | 'Data ]
                                               42    'Run            [ 'Base | 'Data ]
                                               
  46        'Koshucode.Baala.Core.Relmap       /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               33    'Result         [ 'Base | 'Data ]
                                               34    'Option         [ 'Base | 'Data ]
                                               44    'Relmap         [ 'Base | 'Data | 'Lexmap | 'Relkit ]
                                               45    'Rop            [ 'Base | 'Lexmap ]
                                               45    'Specialize     [ 'Base | 'Data | 'Lexmap | 'Relkit ]
                                               46    'Construct      [ 'Base | 'Data | 'Lexmap | 'Relkit ]
                                               46    'Global         [ 'Base | 'Data ]
                                               
  49        'Koshucode.Baala.Core.Assert       /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               33    'RelTable       [ 'Base | 'Data ]
                                               44    'Dataset        [ 'Base | 'Data | 'Relkit ]
                                               48    'Assert         [ 'Base | 'Data | 'Lexmap | 'Relmap ]
                                               49    'Run            [ 'Base | 'Data | 'Lexmap | 'Relkit | 'Relmap ]
                                               
  53        'Koshucode.Baala.Core.Resource     /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               40    'Clause         [ 'Base | 'Data | 'Lexmap ]
                                               51    'Resource       [ 'Assert | 'Base | 'Data | 'Lexmap | 'Relkit | 'Relmap ]
                                               52    'Include        [ 'Assert | 'Base | 'Data | 'Lexmap | 'Relmap ]
                                               52    'Run            [ 'Assert | 'Base | 'Data | 'Lexmap | 'Relmap ]
                                               53    'Quoter         [ 'Base | 'Data | 'Lexmap ]
                                               53    'Read           [ 'Base | 'Data | 'Relmap ]
                                               
  54        'Koshucode.Baala.Core              /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               33    'Message        [ 'Base | 'Data ]
                                               39    'Lexmap         [ ]
                                               43    'Relkit         [ ]
                                               47    'Relmap         [ ]
                                               50    'Assert         [ ]
                                               54    'Resource       [ ]
                                               
  57        'Koshucode.Baala.Op.Cop            /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               33    'Coxhand        [ 'Base | 'Data ]
                                               34    'Logic          [ 'Base | 'Data ]
                                               57    'Arith          [ 'Base | 'Data ]
                                               57    'List           [ 'Base | 'Data ]
                                               57    'Misc           [ 'Base | 'Data ]
                                               57    'Order          [ 'Base | 'Data ]
                                               57    'Time           [ 'Base | 'Data ]
                                               57    'Type           [ 'Base | 'Data ]
                                               
  57        'Koshucode.Baala.Writer            /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               56    'Csv            [ 'Base | 'Core | 'Data ]
                                               56    'Html           [ 'Base | 'Core | 'Data ]
                                               56    'Json           [ 'Core | 'Data ]
                                               56    'Judge          [ 'Base | 'Core | 'Data ]
                                               57    'Koshu          [ 'Base | 'Core | 'Data ]
                                               
  58        'Koshucode.Baala.Op.Builtin        /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               56    'Define         [ 'Base | 'Core ]
                                               57    'Rop            [ 'Base | 'Core ]
                                               57    'Term           [ 'Base | 'Data ]
                                               58    'Get            [ 'Base | 'Core | 'Data ]
                                               
  59        'Koshucode.Baala.Type              /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               59    'Vanilla        [ 'Base | 'Core | 'Data | 'Writer ]
                                               
  61        'Koshucode.Baala.Toolkit.Library   /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               0     'SimpleOption   [ ]
                                               0     'Version        [ ]
                                               33    'RDF            [ 'Data ]
                                               56    'Element        [ 'Base | 'Core | 'Data ]
                                               56    'Exit           [ 'Base | 'Core | 'Data ]
                                               59    'Run            [ 'Base | 'Core | 'Data | 'Writer ]
                                               60    'Input          [ 'Core | 'Data ]
                                               61    'Change         [ 'Base | 'Data | 'Writer ]
                                               
  62        'Koshucode.Baala.Op.Lattice        /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               60    'Tropashko      [ 'Base | 'Builtin | 'Core | 'Data ]
                                               61    'Restrict       [ 'Base | 'Builtin | 'Core | 'Data ]
                                               62    'Rop            [ 'Builtin | 'Core | 'Data ]
                                               
  62        'Koshucode.Baala.Toolkit.Main      /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               60    'KoshuFilter    [ 'Base | 'Core | 'Data ]
                                               60    'KoshuMain      [ 'Base | 'Core | 'Data | 'Writer ]
                                               60    'KoshuRdf       [ 'Base | 'Data ]
                                               60    'KoshuSyntax    [ 'Base | 'Core | 'Data ]
                                               62    'KoshuChange    [ 'Base ]
                                               
  64        'Koshucode.Baala.Op.Cox            /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               0     'GeoDatumJp     [ ]
                                               60    'Get            [ 'Base | 'Builtin | 'Core | 'Data ]
                                               61    'Accessor       [ 'Base | 'Builtin | 'Core | 'Data ]
                                               61    'Calc           [ 'Base | 'Builtin | 'Core | 'Data ]
                                               61    'Filter         [ 'Base | 'Builtin | 'Core | 'Data ]
                                               61    'Gadget         [ 'Base | 'Builtin | 'Core | 'Data ]
                                               61    'Range          [ 'Base | 'Builtin | 'Core | 'Data ]
                                               64    'Empty          [ 'Base | 'Builtin | 'Core | 'Data | 'Lattice ]
                                               
  65        'Koshucode.Baala.Op.Nest           /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               60    'Flow           [ 'Base | 'Builtin | 'Core | 'Data ]
                                               61    'Confl          [ 'Base | 'Builtin | 'Core | 'Data ]
                                               64    'Deriv          [ 'Base | 'Builtin | 'Core | 'Data | 'Lattice ]
                                               65    'Rop            [ 'Builtin | 'Core | 'Data ]
                                               
  67        'Koshucode.Baala                   /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               14    'Base           [ 'Abort | 'Prelude | 'Syntax | 'Text ]
                                               32    'Data           [ 'Church | 'Content | 'Token | 'Type ]
                                               55    'Core           [ 'Assert | 'Lexmap | 'Relkit | 'Relmap | 'Resource ]
                                               58    'Writer         [ ]
                                               67    'Op             [ 'Builtin | 'Lattice | 'Nest ]
                                               
  68        'Koshucode.Baala.Op                /rank /base           /import-dir
                                               ----- --------------- -----------------------------------------------------------
                                               15    'DepRank        [ 'Base ]
                                               56    'Message        [ 'Base | 'Core | 'Data ]
                                               58    'Cop            [ ]
                                               59    'Builtin        [ ]
                                               60    'Gadget         [ 'Base | 'Builtin | 'Core | 'Data ]
                                               60    'Meta           [ 'Base | 'Builtin | 'Core | 'Data ]
                                               60    'Quoter         [ 'Core ]
                                               60    'Resource       [ 'Base | 'Builtin | 'Core | 'Data ]
                                               60    'Source         [ 'Base | 'Builtin | 'Core | 'Data ]
                                               60    'Term           [ 'Base | 'Builtin | 'Core | 'Data ]
                                               61    'Peripheral     [ 'Base | 'Builtin | 'Core | 'Data ]
                                               61    'TermGadget     [ 'Builtin | 'Core | 'Data ]
                                               63    'Lattice        [ ]
                                               64    'Check          [ 'Base | 'Builtin | 'Core | 'Data | 'Lattice ]
                                               64    'Control        [ 'Base | 'Builtin | 'Core | 'Data | 'Lattice ]
                                               65    'Cox            [ ]
                                               66    'Nest           [ ]
                                               68    'Global         [ 'Base | 'Cop | 'Core | 'Cox | 'Data | 'Op ]
                                               

=== rel

**
**  SUMMARY
**      27 judges on DIR-RANK
**      27 judges in total
**

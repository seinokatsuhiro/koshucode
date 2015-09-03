** -*- koshu -*-
**
**  INPUT
**    dir-rank.k
**    ../base/IMPORT.k
**    ../calculator/IMPORT.k
**    ../content/IMPORT.k
**    ../core/IMPORT.k
**    ../operator/IMPORT.k
**    ../toolkit/IMPORT.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Base.Prelude  /base-rank {| /rank /base /import-dir [ 0 | 'Class | [ ] ] [ 0 | 'Import | [ ] ] [ 1 | 'Assoc | [ ] ] [ 1 | 'List | [ ] ] [ 1 | 'Pair | [ ] ] [ 1 | 'Snip | [ ] ] [ 2 | 'Order | [ ] ] |}
|-- DIR-RANK  /dir-rank 5  /dir 'Koshucode.Baala.Base.Text  /base-rank {| /rank /base /import-dir [ 0 | 'Comment | [ ] ] [ 0 | 'Unicode | [ ] ] [ 4 | 'Http | [ 'Prelude ] ] [ 4 | 'IOPoint | [ 'Prelude ] ] [ 4 | 'TextTable | [ 'Prelude ] ] [ 4 | 'Utility | [ 'Prelude ] ] [ 4 | 'Write | [ 'Prelude ] ] [ 5 | 'CodePt | [ 'Prelude ] ] |}
|-- DIR-RANK  /dir-rank 8  /dir 'Koshucode.Baala.Base.Abort  /base-rank {| /rank /base /import-dir [ 7 | 'Reason | [ 'Text ] ] [ 8 | 'Abortable | [ 'Prelude | 'Text ] ] [ 8 | 'Report | [ 'Prelude | 'Text ] ] |}
|-- DIR-RANK  /dir-rank 12  /dir 'Koshucode.Baala.Base.Syntax  /base-rank {| /rank /base /import-dir [ 0 | 'Line | [ ] ] [ 11 | 'Code | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Tree | [ 'Abort | 'Prelude | 'Text ] ] [ 12 | 'Infix | [ 'Prelude ] ] |}
|-- DIR-RANK  /dir-rank 15  /dir 'Koshucode.Baala.Base.Token  /base-rank {| /rank /base /import-dir [ 0 | 'Term | [ ] ] [ 7 | 'AngleText | [ 'Prelude | 'Text ] ] [ 7 | 'Token | [ 'Prelude | 'Text ] ] [ 8 | 'Short | [ 'Prelude | 'Text ] ] [ 14 | 'TokenLine | [ 'Abort | 'Prelude | 'Syntax | 'Text ] ] [ 15 | 'TokenClause | [ 'Prelude | 'Syntax | 'Text ] ] [ 15 | 'TokenTree | [ 'Abort | 'Prelude | 'Syntax | 'Text ] ] |}

|-- DIR-RANK  /dir-rank 20  /dir 'Koshucode.Baala.Base.Data  /base-rank {| /rank /base /import-dir [ 11 | 'Clock | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Date | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Decimal | [ 'Abort | 'Prelude ] ] [ 11 | 'Para | [ 'Abort | 'Prelude ] ] [ 12 | 'Time | [ 'Abort | 'Prelude | 'Text ] ] [ 17 | 'Interp | [ 'Prelude | 'Text | 'Token ] ] [ 17 | 'Judge | [ 'Abort | 'Prelude | 'Text | 'Token ] ] [ 17 | 'Type | [ 'Prelude | 'Text | 'Token ] ] [ 18 | 'About | [ 'Prelude | 'Text | 'Token ] ] [ 18 | 'Head | [ 'Abort | 'Prelude | 'Text | 'Token ] ] [ 19 | 'Rel | [ 'Prelude | 'Text | 'Token ] ] [ 20 | 'Mono | [ 'Abort ] ] |}
|-- DIR-RANK  /dir-rank 23  /dir 'Koshucode.Baala.Base.Result  /base-rank {| /rank /base /import-dir [ 22 | 'Result | [ 'Abort | 'Data | 'Prelude | 'Text | 'Token ] ] [ 23 | 'Html | [ 'Text | 'Token ] ] [ 23 | 'Json | [ 'Token ] ] [ 23 | 'Koshu | [ 'Data | 'Prelude | 'Text | 'Token ] ] |}
|-- DIR-RANK  /dir-rank 24  /dir 'Koshucode.Baala.Base  /base-rank {| /rank /base /import-dir [ 3 | 'Prelude | [ ] ] [ 6 | 'Text | [ ] ] [ 9 | 'Abort | [ ] ] [ 10 | 'Message | [ 'Abort | 'Prelude | 'Text ] ] [ 13 | 'Syntax | [ ] ] [ 16 | 'Token | [ ] ] [ 21 | 'Data | [ ] ] [ 24 | 'Result | [ ] ] |}
|-- DIR-RANK  /dir-rank 30  /dir 'Koshucode.Baala.Core.Content  /base-rank {| /rank /base /import-dir [ 27 | 'Class | [ 'Base ] ] [ 27 | 'Cox | [ 'Base ] ] [ 27 | 'Tree | [ 'Base ] ] [ 28 | 'Cop | [ 'Base ] ] [ 28 | 'Literal | [ 'Base ] ] [ 29 | 'Build | [ 'Base ] ] [ 30 | 'Run | [ 'Base ] ] |}
|-- DIR-RANK  /dir-rank 31  /dir 'Koshucode.Baala.Core.Lexmap  /base-rank {| /rank /base /import-dir [ 27 | 'AttrPos | [ 'Base ] ] [ 28 | 'Attr | [ 'Base ] ] [ 28 | 'Slot | [ 'Base ] ] [ 29 | 'AttrEd | [ 'Base ] ] [ 29 | 'Lexmap | [ 'Base ] ] [ 30 | 'LexmapTrees | [ 'Base ] ] [ 31 | 'Construct | [ 'Base ] ] |}

|-- DIR-RANK  /dir-rank 35  /dir 'Koshucode.Baala.Core.Relkit  /base-rank {| /rank /base /import-dir [ 33 | 'Relkit | [ 'Base | 'Lexmap ] ] [ 34 | 'Construct | [ 'Base ] ] [ 35 | 'Run | [ 'Base | 'Content ] ] |}
|-- DIR-RANK  /dir-rank 39  /dir 'Koshucode.Baala.Core.Relmap  /base-rank {| /rank /base /import-dir [ 32 | 'Option | [ 'Base | 'Content ] ] [ 37 | 'Relmap | [ 'Base | 'Lexmap | 'Relkit ] ] [ 38 | 'Rop | [ 'Base | 'Lexmap ] ] [ 38 | 'Specialize | [ 'Base | 'Lexmap | 'Relkit ] ] [ 39 | 'Construct | [ 'Base | 'Lexmap | 'Relkit ] ] [ 39 | 'Global | [ 'Base | 'Content ] ] |}
|-- DIR-RANK  /dir-rank 42  /dir 'Koshucode.Baala.Core.Assert  /base-rank {| /rank /base /import-dir [ 32 | 'RelTable | [ 'Base | 'Content ] ] [ 37 | 'Dataset | [ 'Base | 'Content | 'Relkit ] ] [ 41 | 'Assert | [ 'Base | 'Content | 'Lexmap | 'Relmap ] ] [ 42 | 'Run | [ 'Base | 'Content | 'Lexmap | 'Relkit | 'Relmap ] ] |}
|-- DIR-RANK  /dir-rank 46  /dir 'Koshucode.Baala.Core.Resource  /base-rank {| /rank /base /import-dir [ 33 | 'Clause | [ 'Base | 'Content | 'Lexmap ] ] [ 44 | 'Resource | [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relkit | 'Relmap ] ] [ 45 | 'Include | [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relmap ] ] [ 45 | 'Run | [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relmap ] ] [ 46 | 'Quoter | [ 'Base | 'Lexmap ] ] [ 46 | 'Read | [ 'Base | 'Content | 'Relmap ] ] |}
|-- DIR-RANK  /dir-rank 47  /dir 'Koshucode.Baala.Core  /base-rank {| /rank /base /import-dir [ 26 | 'Message | [ 'Base ] ] [ 31 | 'Content | [ ] ] [ 32 | 'Lexmap | [ ] ] [ 36 | 'Relkit | [ ] ] [ 40 | 'Relmap | [ ] ] [ 43 | 'Assert | [ ] ] [ 47 | 'Resource | [ ] ] |}

|-- DIR-RANK  /dir-rank 50  /dir 'Koshucode.Baala.Op.Cop  /base-rank {| /rank /base /import-dir [ 49 | 'Coxhand | [ 'Base | 'Core ] ] [ 50 | 'Arith | [ 'Base | 'Core ] ] [ 50 | 'List | [ 'Base | 'Core ] ] [ 50 | 'Logic | [ 'Base | 'Core ] ] [ 50 | 'Misc | [ 'Base | 'Core ] ] [ 50 | 'Order | [ 'Base | 'Core ] ] [ 50 | 'Time | [ 'Base | 'Core ] ] [ 50 | 'Type | [ 'Base | 'Core ] ] |}
|-- DIR-RANK  /dir-rank 50  /dir 'Koshucode.Baala.Type  /base-rank {| /rank /base /import-dir [ 50 | 'Vanilla | [ 'Base | 'Core ] ] |}
|-- DIR-RANK  /dir-rank 51  /dir 'Koshucode.Baala.Op.Builtin  /base-rank {| /rank /base /import-dir [ 49 | 'Define | [ 'Base | 'Core ] ] [ 50 | 'Rop | [ 'Base | 'Core ] ] [ 50 | 'Term | [ 'Base ] ] [ 51 | 'Get | [ 'Base | 'Core ] ] |}
|-- DIR-RANK  /dir-rank 52  /dir 'Koshucode.Baala.Toolkit.Library  /base-rank {| /rank /base /import-dir [ 0 | 'SimpleOption | [ ] ] [ 0 | 'Version | [ ] ] [ 26 | 'Exit | [ 'Base ] ] [ 49 | 'Element | [ 'Base | 'Core ] ] [ 49 | 'RDF | [ 'Base | 'Core ] ] [ 49 | 'Run | [ 'Base | 'Core ] ] [ 51 | 'Input | [ 'Base | 'Core ] ] [ 52 | 'Change | [ 'Base ] ] |}
|-- DIR-RANK  /dir-rank 53  /dir 'Koshucode.Baala.Toolkit.Main  /base-rank {| /rank /base /import-dir [ 50 | 'KoshuFilter | [ 'Base | 'Core ] ] [ 50 | 'KoshuMain | [ 'Base | 'Core ] ] [ 51 | 'KoshuRdf | [ 'Base ] ] [ 51 | 'KoshuSyntax | [ 'Base | 'Core ] ] [ 53 | 'KoshuChange | [ ] ] |}

|-- DIR-RANK  /dir-rank 55  /dir 'Koshucode.Baala.Op.Lattice  /base-rank {| /rank /base /import-dir [ 53 | 'Tropashko | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Restrict | [ 'Base | 'Builtin | 'Core ] ] [ 55 | 'Rop | [ 'Builtin | 'Core ] ] |}
|-- DIR-RANK  /dir-rank 57  /dir 'Koshucode.Baala.Op.Cox  /base-rank {| /rank /base /import-dir [ 0 | 'GeoDatumJp | [ ] ] [ 53 | 'Get | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Accessor | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Calc | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Filter | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Gadget | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Range | [ 'Base | 'Builtin | 'Core ] ] [ 57 | 'Empty | [ 'Base | 'Builtin | 'Core | 'Lattice ] ] |}
|-- DIR-RANK  /dir-rank 58  /dir 'Koshucode.Baala.Op.Nest  /base-rank {| /rank /base /import-dir [ 53 | 'Flow | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Confl | [ 'Base | 'Builtin | 'Core ] ] [ 57 | 'Deriv | [ 'Base | 'Builtin | 'Core | 'Lattice ] ] [ 58 | 'Rop | [ 'Builtin | 'Core ] ] |}
|-- DIR-RANK  /dir-rank 60  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 25 | 'Base | [ 'Abort | 'Data | 'Prelude | 'Result | 'Syntax | 'Text | 'Token ] ] [ 48 | 'Core | [ 'Assert | 'Content | 'Lexmap | 'Relkit | 'Relmap | 'Resource ] ] [ 60 | 'Op | [ 'Builtin | 'Lattice | 'Nest ] ] |}
|-- DIR-RANK  /dir-rank 61  /dir 'Koshucode.Baala.Op  /base-rank {| /rank /base /import-dir [ 26 | 'DepRank | [ 'Base ] ] [ 49 | 'Message | [ 'Base | 'Core ] ] [ 51 | 'Cop | [ ] ] [ 51 | 'Quoter | [ 'Core ] ] [ 52 | 'Builtin | [ ] ] [ 53 | 'Gadget | [ 'Base | 'Builtin | 'Core ] ] [ 53 | 'Meta | [ 'Base | 'Builtin | 'Core ] ] [ 53 | 'Resource | [ 'Base | 'Builtin | 'Core ] ] [ 53 | 'Source | [ 'Base | 'Builtin | 'Core ] ] [ 53 | 'Term | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Peripheral | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'TermGadget | [ 'Base | 'Builtin | 'Core ] ] [ 56 | 'Lattice | [ ] ] [ 57 | 'Check | [ 'Base | 'Builtin | 'Core | 'Lattice ] ] [ 57 | 'Control | [ 'Base | 'Builtin | 'Core | 'Lattice ] ] [ 58 | 'Cox | [ ] ] [ 59 | 'Nest | [ ] ] [ 61 | 'Global | [ 'Base | 'Cop | 'Core | 'Cox | 'Op ] ] |}

*** 25 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                               /base-rank
  --------- ---------------------------------- ------------------------------------------------------------------------------------------
  2         'Koshucode.Baala.Base.Prelude      /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               0     'Class          [ ]
                                               0     'Import         [ ]
                                               1     'Assoc          [ ]
                                               1     'List           [ ]
                                               1     'Pair           [ ]
                                               1     'Snip           [ ]
                                               2     'Order          [ ]
                                               
  5         'Koshucode.Baala.Base.Text         /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               0     'Comment        [ ]
                                               0     'Unicode        [ ]
                                               4     'Http           [ 'Prelude ]
                                               4     'IOPoint        [ 'Prelude ]
                                               4     'TextTable      [ 'Prelude ]
                                               4     'Utility        [ 'Prelude ]
                                               4     'Write          [ 'Prelude ]
                                               5     'CodePt         [ 'Prelude ]
                                               
  8         'Koshucode.Baala.Base.Abort        /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               7     'Reason         [ 'Text ]
                                               8     'Abortable      [ 'Prelude | 'Text ]
                                               8     'Report         [ 'Prelude | 'Text ]
                                               
  12        'Koshucode.Baala.Base.Syntax       /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               0     'Line           [ ]
                                               11    'Code           [ 'Abort | 'Prelude | 'Text ]
                                               11    'Tree           [ 'Abort | 'Prelude | 'Text ]
                                               12    'Infix          [ 'Prelude ]
                                               
  15        'Koshucode.Baala.Base.Token        /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               0     'Term           [ ]
                                               7     'AngleText      [ 'Prelude | 'Text ]
                                               7     'Token          [ 'Prelude | 'Text ]
                                               8     'Short          [ 'Prelude | 'Text ]
                                               14    'TokenLine      [ 'Abort | 'Prelude | 'Syntax | 'Text ]
                                               15    'TokenClause    [ 'Prelude | 'Syntax | 'Text ]
                                               15    'TokenTree      [ 'Abort | 'Prelude | 'Syntax | 'Text ]
                                               
  20        'Koshucode.Baala.Base.Data         /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               11    'Clock          [ 'Abort | 'Prelude | 'Text ]
                                               11    'Date           [ 'Abort | 'Prelude | 'Text ]
                                               11    'Decimal        [ 'Abort | 'Prelude ]
                                               11    'Para           [ 'Abort | 'Prelude ]
                                               12    'Time           [ 'Abort | 'Prelude | 'Text ]
                                               17    'Interp         [ 'Prelude | 'Text | 'Token ]
                                               17    'Judge          [ 'Abort | 'Prelude | 'Text | 'Token ]
                                               17    'Type           [ 'Prelude | 'Text | 'Token ]
                                               18    'About          [ 'Prelude | 'Text | 'Token ]
                                               18    'Head           [ 'Abort | 'Prelude | 'Text | 'Token ]
                                               19    'Rel            [ 'Prelude | 'Text | 'Token ]
                                               20    'Mono           [ 'Abort ]
                                               
  23        'Koshucode.Baala.Base.Result       /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               22    'Result         [ 'Abort | 'Data | 'Prelude | 'Text | 'Token ]
                                               23    'Html           [ 'Text | 'Token ]
                                               23    'Json           [ 'Token ]
                                               23    'Koshu          [ 'Data | 'Prelude | 'Text | 'Token ]
                                               
  24        'Koshucode.Baala.Base              /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               3     'Prelude        [ ]
                                               6     'Text           [ ]
                                               9     'Abort          [ ]
                                               10    'Message        [ 'Abort | 'Prelude | 'Text ]
                                               13    'Syntax         [ ]
                                               16    'Token          [ ]
                                               21    'Data           [ ]
                                               24    'Result         [ ]
                                               
  30        'Koshucode.Baala.Core.Content      /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               27    'Class          [ 'Base ]
                                               27    'Cox            [ 'Base ]
                                               27    'Tree           [ 'Base ]
                                               28    'Cop            [ 'Base ]
                                               28    'Literal        [ 'Base ]
                                               29    'Build          [ 'Base ]
                                               30    'Run            [ 'Base ]
                                               
  31        'Koshucode.Baala.Core.Lexmap       /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               27    'AttrPos        [ 'Base ]
                                               28    'Attr           [ 'Base ]
                                               28    'Slot           [ 'Base ]
                                               29    'AttrEd         [ 'Base ]
                                               29    'Lexmap         [ 'Base ]
                                               30    'LexmapTrees    [ 'Base ]
                                               31    'Construct      [ 'Base ]
                                               
  35        'Koshucode.Baala.Core.Relkit       /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               33    'Relkit         [ 'Base | 'Lexmap ]
                                               34    'Construct      [ 'Base ]
                                               35    'Run            [ 'Base | 'Content ]
                                               
  39        'Koshucode.Baala.Core.Relmap       /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               32    'Option         [ 'Base | 'Content ]
                                               37    'Relmap         [ 'Base | 'Lexmap | 'Relkit ]
                                               38    'Rop            [ 'Base | 'Lexmap ]
                                               38    'Specialize     [ 'Base | 'Lexmap | 'Relkit ]
                                               39    'Construct      [ 'Base | 'Lexmap | 'Relkit ]
                                               39    'Global         [ 'Base | 'Content ]
                                               
  42        'Koshucode.Baala.Core.Assert       /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               32    'RelTable       [ 'Base | 'Content ]
                                               37    'Dataset        [ 'Base | 'Content | 'Relkit ]
                                               41    'Assert         [ 'Base | 'Content | 'Lexmap | 'Relmap ]
                                               42    'Run            [ 'Base | 'Content | 'Lexmap | 'Relkit | 'Relmap ]
                                               
  46        'Koshucode.Baala.Core.Resource     /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               33    'Clause         [ 'Base | 'Content | 'Lexmap ]
                                               44    'Resource       [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relkit | 'Relmap ]
                                               45    'Include        [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relmap ]
                                               45    'Run            [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relmap ]
                                               46    'Quoter         [ 'Base | 'Lexmap ]
                                               46    'Read           [ 'Base | 'Content | 'Relmap ]
                                               
  47        'Koshucode.Baala.Core              /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               26    'Message        [ 'Base ]
                                               31    'Content        [ ]
                                               32    'Lexmap         [ ]
                                               36    'Relkit         [ ]
                                               40    'Relmap         [ ]
                                               43    'Assert         [ ]
                                               47    'Resource       [ ]
                                               
  50        'Koshucode.Baala.Op.Cop            /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               49    'Coxhand        [ 'Base | 'Core ]
                                               50    'Arith          [ 'Base | 'Core ]
                                               50    'List           [ 'Base | 'Core ]
                                               50    'Logic          [ 'Base | 'Core ]
                                               50    'Misc           [ 'Base | 'Core ]
                                               50    'Order          [ 'Base | 'Core ]
                                               50    'Time           [ 'Base | 'Core ]
                                               50    'Type           [ 'Base | 'Core ]
                                               
  50        'Koshucode.Baala.Type              /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               50    'Vanilla        [ 'Base | 'Core ]
                                               
  51        'Koshucode.Baala.Op.Builtin        /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               49    'Define         [ 'Base | 'Core ]
                                               50    'Rop            [ 'Base | 'Core ]
                                               50    'Term           [ 'Base ]
                                               51    'Get            [ 'Base | 'Core ]
                                               
  52        'Koshucode.Baala.Toolkit.Library   /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               0     'SimpleOption   [ ]
                                               0     'Version        [ ]
                                               26    'Exit           [ 'Base ]
                                               49    'Element        [ 'Base | 'Core ]
                                               49    'RDF            [ 'Base | 'Core ]
                                               49    'Run            [ 'Base | 'Core ]
                                               51    'Input          [ 'Base | 'Core ]
                                               52    'Change         [ 'Base ]
                                               
  53        'Koshucode.Baala.Toolkit.Main      /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               50    'KoshuFilter    [ 'Base | 'Core ]
                                               50    'KoshuMain      [ 'Base | 'Core ]
                                               51    'KoshuRdf       [ 'Base ]
                                               51    'KoshuSyntax    [ 'Base | 'Core ]
                                               53    'KoshuChange    [ ]
                                               
  55        'Koshucode.Baala.Op.Lattice        /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               53    'Tropashko      [ 'Base | 'Builtin | 'Core ]
                                               54    'Restrict       [ 'Base | 'Builtin | 'Core ]
                                               55    'Rop            [ 'Builtin | 'Core ]
                                               
  57        'Koshucode.Baala.Op.Cox            /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               0     'GeoDatumJp     [ ]
                                               53    'Get            [ 'Base | 'Builtin | 'Core ]
                                               54    'Accessor       [ 'Base | 'Builtin | 'Core ]
                                               54    'Calc           [ 'Base | 'Builtin | 'Core ]
                                               54    'Filter         [ 'Base | 'Builtin | 'Core ]
                                               54    'Gadget         [ 'Base | 'Builtin | 'Core ]
                                               54    'Range          [ 'Base | 'Builtin | 'Core ]
                                               57    'Empty          [ 'Base | 'Builtin | 'Core | 'Lattice ]
                                               
  58        'Koshucode.Baala.Op.Nest           /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               53    'Flow           [ 'Base | 'Builtin | 'Core ]
                                               54    'Confl          [ 'Base | 'Builtin | 'Core ]
                                               57    'Deriv          [ 'Base | 'Builtin | 'Core | 'Lattice ]
                                               58    'Rop            [ 'Builtin | 'Core ]
                                               
  60        'Koshucode.Baala                   /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               25    'Base           [ 'Abort | 'Data | 'Prelude | 'Result | 'Syntax | 'Text | 'Token ]
                                               48    'Core           [ 'Assert | 'Content | 'Lexmap | 'Relkit | 'Relmap | 'Resource ]
                                               60    'Op             [ 'Builtin | 'Lattice | 'Nest ]
                                               
  61        'Koshucode.Baala.Op                /rank /base           /import-dir
                                               ----- --------------- --------------------------------------------------------------------
                                               26    'DepRank        [ 'Base ]
                                               49    'Message        [ 'Base | 'Core ]
                                               51    'Cop            [ ]
                                               51    'Quoter         [ 'Core ]
                                               52    'Builtin        [ ]
                                               53    'Gadget         [ 'Base | 'Builtin | 'Core ]
                                               53    'Meta           [ 'Base | 'Builtin | 'Core ]
                                               53    'Resource       [ 'Base | 'Builtin | 'Core ]
                                               53    'Source         [ 'Base | 'Builtin | 'Core ]
                                               53    'Term           [ 'Base | 'Builtin | 'Core ]
                                               54    'Peripheral     [ 'Base | 'Builtin | 'Core ]
                                               54    'TermGadget     [ 'Base | 'Builtin | 'Core ]
                                               56    'Lattice        [ ]
                                               57    'Check          [ 'Base | 'Builtin | 'Core | 'Lattice ]
                                               57    'Control        [ 'Base | 'Builtin | 'Core | 'Lattice ]
                                               58    'Cox            [ ]
                                               59    'Nest           [ ]
                                               61    'Global         [ 'Base | 'Cop | 'Core | 'Cox | 'Op ]
                                               

=== rel

**
**  SUMMARY
**      25 judges on DIR-RANK
**      25 judges in total
**

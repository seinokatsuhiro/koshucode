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

|-- DIR-RANK  /dir-rank 20  /dir 'Koshucode.Baala.Base.Data  /base-rank {| /rank /base /import-dir [ 11 | 'Clock | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Date | [ 'Abort | 'Prelude | 'Text ] ] [ 11 | 'Decimal | [ 'Abort | 'Prelude ] ] [ 11 | 'Para | [ 'Abort | 'Prelude ] ] [ 12 | 'Time | [ 'Abort | 'Prelude | 'Text ] ] [ 17 | 'Interp | [ 'Prelude | 'Text | 'Token ] ] [ 17 | 'Judge | [ 'Abort | 'Prelude | 'Text | 'Token ] ] [ 17 | 'Type | [ 'Prelude | 'Text | 'Token ] ] [ 18 | 'About | [ 'Prelude | 'Text | 'Token ] ] [ 18 | 'Head | [ 'Abort | 'Prelude | 'Text | 'Token ] ] [ 19 | 'Rel | [ 'Prelude | 'Text | 'Token ] ] [ 20 | 'Mono | [ 'Abort ] ] [ 20 | 'Result | [ 'Abort | 'Prelude | 'Text | 'Token ] ] |}
|-- DIR-RANK  /dir-rank 21  /dir 'Koshucode.Baala.Base  /base-rank {| /rank /base /import-dir [ 3 | 'Prelude | [ ] ] [ 6 | 'Text | [ ] ] [ 9 | 'Abort | [ ] ] [ 10 | 'Message | [ 'Abort | 'Prelude | 'Text ] ] [ 13 | 'Syntax | [ ] ] [ 16 | 'Token | [ ] ] [ 21 | 'Data | [ ] ] |}
|-- DIR-RANK  /dir-rank 27  /dir 'Koshucode.Baala.Core.Content  /base-rank {| /rank /base /import-dir [ 24 | 'Class | [ 'Base ] ] [ 24 | 'Cox | [ 'Base ] ] [ 24 | 'Tree | [ 'Base ] ] [ 25 | 'Cop | [ 'Base ] ] [ 25 | 'Literal | [ 'Base ] ] [ 26 | 'Build | [ 'Base ] ] [ 27 | 'Run | [ 'Base ] ] |}
|-- DIR-RANK  /dir-rank 28  /dir 'Koshucode.Baala.Core.Lexmap  /base-rank {| /rank /base /import-dir [ 24 | 'AttrPos | [ 'Base ] ] [ 25 | 'Attr | [ 'Base ] ] [ 25 | 'Slot | [ 'Base ] ] [ 26 | 'AttrEd | [ 'Base ] ] [ 26 | 'Lexmap | [ 'Base ] ] [ 27 | 'LexmapTrees | [ 'Base ] ] [ 28 | 'Construct | [ 'Base ] ] |}
|-- DIR-RANK  /dir-rank 32  /dir 'Koshucode.Baala.Core.Relkit  /base-rank {| /rank /base /import-dir [ 30 | 'Relkit | [ 'Base | 'Lexmap ] ] [ 31 | 'Construct | [ 'Base ] ] [ 32 | 'Run | [ 'Base | 'Content ] ] |}

|-- DIR-RANK  /dir-rank 36  /dir 'Koshucode.Baala.Core.Relmap  /base-rank {| /rank /base /import-dir [ 29 | 'Option | [ 'Base | 'Content ] ] [ 34 | 'Relmap | [ 'Base | 'Lexmap | 'Relkit ] ] [ 35 | 'Rop | [ 'Base | 'Lexmap ] ] [ 35 | 'Specialize | [ 'Base | 'Lexmap | 'Relkit ] ] [ 36 | 'Construct | [ 'Base | 'Lexmap | 'Relkit ] ] [ 36 | 'Global | [ 'Base | 'Content ] ] |}
|-- DIR-RANK  /dir-rank 39  /dir 'Koshucode.Baala.Core.Assert  /base-rank {| /rank /base /import-dir [ 29 | 'RelTable | [ 'Base | 'Content ] ] [ 34 | 'Dataset | [ 'Base | 'Content | 'Relkit ] ] [ 38 | 'Assert | [ 'Base | 'Content | 'Lexmap | 'Relmap ] ] [ 39 | 'Run | [ 'Base | 'Content | 'Lexmap | 'Relkit | 'Relmap ] ] |}
|-- DIR-RANK  /dir-rank 43  /dir 'Koshucode.Baala.Core.Resource  /base-rank {| /rank /base /import-dir [ 30 | 'Clause | [ 'Base | 'Content | 'Lexmap ] ] [ 41 | 'Resource | [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relkit | 'Relmap ] ] [ 42 | 'Include | [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relmap ] ] [ 42 | 'Run | [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relmap ] ] [ 43 | 'Quoter | [ 'Base | 'Lexmap ] ] [ 43 | 'Read | [ 'Base | 'Content | 'Relmap ] ] |}
|-- DIR-RANK  /dir-rank 44  /dir 'Koshucode.Baala.Core  /base-rank {| /rank /base /import-dir [ 23 | 'Message | [ 'Base ] ] [ 28 | 'Content | [ ] ] [ 29 | 'Lexmap | [ ] ] [ 33 | 'Relkit | [ ] ] [ 37 | 'Relmap | [ ] ] [ 40 | 'Assert | [ ] ] [ 44 | 'Resource | [ ] ] |}
|-- DIR-RANK  /dir-rank 47  /dir 'Koshucode.Baala.Op.Cop  /base-rank {| /rank /base /import-dir [ 46 | 'Coxhand | [ 'Base | 'Core ] ] [ 47 | 'Arith | [ 'Base | 'Core ] ] [ 47 | 'List | [ 'Base | 'Core ] ] [ 47 | 'Logic | [ 'Base | 'Core ] ] [ 47 | 'Misc | [ 'Base | 'Core ] ] [ 47 | 'Order | [ 'Base | 'Core ] ] [ 47 | 'Time | [ 'Base | 'Core ] ] [ 47 | 'Type | [ 'Base | 'Core ] ] |}

|-- DIR-RANK  /dir-rank 47  /dir 'Koshucode.Baala.Type  /base-rank {| /rank /base /import-dir [ 47 | 'Vanilla | [ 'Base | 'Core ] ] |}
|-- DIR-RANK  /dir-rank 48  /dir 'Koshucode.Baala.Op.Builtin  /base-rank {| /rank /base /import-dir [ 46 | 'Define | [ 'Base | 'Core ] ] [ 47 | 'Rop | [ 'Base | 'Core ] ] [ 47 | 'Term | [ 'Base ] ] [ 48 | 'Get | [ 'Base | 'Core ] ] |}
|-- DIR-RANK  /dir-rank 49  /dir 'Koshucode.Baala.Toolkit.Library  /base-rank {| /rank /base /import-dir [ 0 | 'Version | [ ] ] [ 23 | 'Exit | [ 'Base ] ] [ 46 | 'Element | [ 'Base | 'Core ] ] [ 46 | 'RDF | [ 'Base | 'Core ] ] [ 46 | 'Run | [ 'Base | 'Core ] ] [ 48 | 'Input | [ 'Base | 'Core ] ] [ 49 | 'Change | [ 'Base ] ] |}
|-- DIR-RANK  /dir-rank 50  /dir 'Koshucode.Baala.Toolkit.Main  /base-rank {| /rank /base /import-dir [ 47 | 'KoshuFilter | [ 'Base | 'Core ] ] [ 47 | 'KoshuMain | [ 'Base | 'Core ] ] [ 48 | 'KoshuRdf | [ 'Base ] ] [ 48 | 'KoshuSyntax | [ 'Base | 'Core ] ] [ 50 | 'KoshuChange | [ ] ] |}
|-- DIR-RANK  /dir-rank 52  /dir 'Koshucode.Baala.Op.Lattice  /base-rank {| /rank /base /import-dir [ 50 | 'Tropashko | [ 'Base | 'Builtin | 'Core ] ] [ 51 | 'Restrict | [ 'Base | 'Builtin | 'Core ] ] [ 52 | 'Rop | [ 'Builtin | 'Core ] ] |}

|-- DIR-RANK  /dir-rank 54  /dir 'Koshucode.Baala.Op.Cox  /base-rank {| /rank /base /import-dir [ 50 | 'Get | [ 'Base | 'Builtin | 'Core ] ] [ 51 | 'Accessor | [ 'Base | 'Builtin | 'Core ] ] [ 51 | 'Calc | [ 'Base | 'Builtin | 'Core ] ] [ 51 | 'Filter | [ 'Base | 'Builtin | 'Core ] ] [ 51 | 'Gadget | [ 'Base | 'Builtin | 'Core ] ] [ 51 | 'Range | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Empty | [ 'Base | 'Builtin | 'Core | 'Lattice ] ] |}
|-- DIR-RANK  /dir-rank 55  /dir 'Koshucode.Baala.Op.Nest  /base-rank {| /rank /base /import-dir [ 50 | 'Flow | [ 'Base | 'Builtin | 'Core ] ] [ 51 | 'Confl | [ 'Base | 'Builtin | 'Core ] ] [ 54 | 'Deriv | [ 'Base | 'Builtin | 'Core | 'Lattice ] ] [ 55 | 'Rop | [ 'Builtin | 'Core ] ] |}
|-- DIR-RANK  /dir-rank 57  /dir 'Koshucode.Baala  /base-rank {| /rank /base /import-dir [ 22 | 'Base | [ 'Abort | 'Data | 'Prelude | 'Syntax | 'Text | 'Token ] ] [ 45 | 'Core | [ 'Assert | 'Content | 'Lexmap | 'Relkit | 'Relmap | 'Resource ] ] [ 57 | 'Op | [ 'Builtin | 'Lattice | 'Nest ] ] |}
|-- DIR-RANK  /dir-rank 58  /dir 'Koshucode.Baala.Op  /base-rank {| /rank /base /import-dir [ 23 | 'DepRank | [ 'Base ] ] [ 46 | 'Message | [ 'Base | 'Core ] ] [ 48 | 'Cop | [ ] ] [ 48 | 'Quoter | [ 'Core ] ] [ 49 | 'Builtin | [ ] ] [ 50 | 'Gadget | [ 'Base | 'Builtin | 'Core ] ] [ 50 | 'Meta | [ 'Base | 'Builtin | 'Core ] ] [ 50 | 'Resource | [ 'Base | 'Builtin | 'Core ] ] [ 50 | 'Source | [ 'Base | 'Builtin | 'Core ] ] [ 50 | 'Term | [ 'Base | 'Builtin | 'Core ] ] [ 51 | 'Peripheral | [ 'Base | 'Builtin | 'Core ] ] [ 51 | 'TermGadget | [ 'Base | 'Builtin | 'Core ] ] [ 53 | 'Lattice | [ ] ] [ 54 | 'Check | [ 'Base | 'Builtin | 'Core | 'Lattice ] ] [ 54 | 'Control | [ 'Base | 'Builtin | 'Core | 'Lattice ] ] [ 55 | 'Cox | [ ] ] [ 56 | 'Nest | [ ] ] [ 58 | 'Global | [ 'Base | 'Cop | 'Core | 'Cox | 'Op ] ] |}

*** 24 judges

=== note

TABLE : DIR-RANK

  /dir-rank /dir                               /base-rank
  --------- ---------------------------------- ---------------------------------------------------------------------------------------
  2         'Koshucode.Baala.Base.Prelude      /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               0     'Class         [ ]
                                               0     'Import        [ ]
                                               1     'Assoc         [ ]
                                               1     'List          [ ]
                                               1     'Pair          [ ]
                                               1     'Snip          [ ]
                                               2     'Order         [ ]
                                               
  5         'Koshucode.Baala.Base.Text         /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               0     'Comment       [ ]
                                               0     'Unicode       [ ]
                                               4     'Http          [ 'Prelude ]
                                               4     'IOPoint       [ 'Prelude ]
                                               4     'TextTable     [ 'Prelude ]
                                               4     'Utility       [ 'Prelude ]
                                               4     'Write         [ 'Prelude ]
                                               5     'CodePt        [ 'Prelude ]
                                               
  8         'Koshucode.Baala.Base.Abort        /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               7     'Reason        [ 'Text ]
                                               8     'Abortable     [ 'Prelude | 'Text ]
                                               8     'Report        [ 'Prelude | 'Text ]
                                               
  12        'Koshucode.Baala.Base.Syntax       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               0     'Line          [ ]
                                               11    'Code          [ 'Abort | 'Prelude | 'Text ]
                                               11    'Tree          [ 'Abort | 'Prelude | 'Text ]
                                               12    'Infix         [ 'Prelude ]
                                               
  15        'Koshucode.Baala.Base.Token        /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               0     'Term          [ ]
                                               7     'AngleText     [ 'Prelude | 'Text ]
                                               7     'Token         [ 'Prelude | 'Text ]
                                               8     'Short         [ 'Prelude | 'Text ]
                                               14    'TokenLine     [ 'Abort | 'Prelude | 'Syntax | 'Text ]
                                               15    'TokenClause   [ 'Prelude | 'Syntax | 'Text ]
                                               15    'TokenTree     [ 'Abort | 'Prelude | 'Syntax | 'Text ]
                                               
  20        'Koshucode.Baala.Base.Data         /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               11    'Clock         [ 'Abort | 'Prelude | 'Text ]
                                               11    'Date          [ 'Abort | 'Prelude | 'Text ]
                                               11    'Decimal       [ 'Abort | 'Prelude ]
                                               11    'Para          [ 'Abort | 'Prelude ]
                                               12    'Time          [ 'Abort | 'Prelude | 'Text ]
                                               17    'Interp        [ 'Prelude | 'Text | 'Token ]
                                               17    'Judge         [ 'Abort | 'Prelude | 'Text | 'Token ]
                                               17    'Type          [ 'Prelude | 'Text | 'Token ]
                                               18    'About         [ 'Prelude | 'Text | 'Token ]
                                               18    'Head          [ 'Abort | 'Prelude | 'Text | 'Token ]
                                               19    'Rel           [ 'Prelude | 'Text | 'Token ]
                                               20    'Mono          [ 'Abort ]
                                               20    'Result        [ 'Abort | 'Prelude | 'Text | 'Token ]
                                               
  21        'Koshucode.Baala.Base              /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               3     'Prelude       [ ]
                                               6     'Text          [ ]
                                               9     'Abort         [ ]
                                               10    'Message       [ 'Abort | 'Prelude | 'Text ]
                                               13    'Syntax        [ ]
                                               16    'Token         [ ]
                                               21    'Data          [ ]
                                               
  27        'Koshucode.Baala.Core.Content      /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               24    'Class         [ 'Base ]
                                               24    'Cox           [ 'Base ]
                                               24    'Tree          [ 'Base ]
                                               25    'Cop           [ 'Base ]
                                               25    'Literal       [ 'Base ]
                                               26    'Build         [ 'Base ]
                                               27    'Run           [ 'Base ]
                                               
  28        'Koshucode.Baala.Core.Lexmap       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               24    'AttrPos       [ 'Base ]
                                               25    'Attr          [ 'Base ]
                                               25    'Slot          [ 'Base ]
                                               26    'AttrEd        [ 'Base ]
                                               26    'Lexmap        [ 'Base ]
                                               27    'LexmapTrees   [ 'Base ]
                                               28    'Construct     [ 'Base ]
                                               
  32        'Koshucode.Baala.Core.Relkit       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               30    'Relkit        [ 'Base | 'Lexmap ]
                                               31    'Construct     [ 'Base ]
                                               32    'Run           [ 'Base | 'Content ]
                                               
  36        'Koshucode.Baala.Core.Relmap       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               29    'Option        [ 'Base | 'Content ]
                                               34    'Relmap        [ 'Base | 'Lexmap | 'Relkit ]
                                               35    'Rop           [ 'Base | 'Lexmap ]
                                               35    'Specialize    [ 'Base | 'Lexmap | 'Relkit ]
                                               36    'Construct     [ 'Base | 'Lexmap | 'Relkit ]
                                               36    'Global        [ 'Base | 'Content ]
                                               
  39        'Koshucode.Baala.Core.Assert       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               29    'RelTable      [ 'Base | 'Content ]
                                               34    'Dataset       [ 'Base | 'Content | 'Relkit ]
                                               38    'Assert        [ 'Base | 'Content | 'Lexmap | 'Relmap ]
                                               39    'Run           [ 'Base | 'Content | 'Lexmap | 'Relkit | 'Relmap ]
                                               
  43        'Koshucode.Baala.Core.Resource     /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               30    'Clause        [ 'Base | 'Content | 'Lexmap ]
                                               41    'Resource      [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relkit | 'Relmap ]
                                               42    'Include       [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relmap ]
                                               42    'Run           [ 'Assert | 'Base | 'Content | 'Lexmap | 'Relmap ]
                                               43    'Quoter        [ 'Base | 'Lexmap ]
                                               43    'Read          [ 'Base | 'Content | 'Relmap ]
                                               
  44        'Koshucode.Baala.Core              /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               23    'Message       [ 'Base ]
                                               28    'Content       [ ]
                                               29    'Lexmap        [ ]
                                               33    'Relkit        [ ]
                                               37    'Relmap        [ ]
                                               40    'Assert        [ ]
                                               44    'Resource      [ ]
                                               
  47        'Koshucode.Baala.Op.Cop            /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               46    'Coxhand       [ 'Base | 'Core ]
                                               47    'Arith         [ 'Base | 'Core ]
                                               47    'List          [ 'Base | 'Core ]
                                               47    'Logic         [ 'Base | 'Core ]
                                               47    'Misc          [ 'Base | 'Core ]
                                               47    'Order         [ 'Base | 'Core ]
                                               47    'Time          [ 'Base | 'Core ]
                                               47    'Type          [ 'Base | 'Core ]
                                               
  47        'Koshucode.Baala.Type              /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               47    'Vanilla       [ 'Base | 'Core ]
                                               
  48        'Koshucode.Baala.Op.Builtin        /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               46    'Define        [ 'Base | 'Core ]
                                               47    'Rop           [ 'Base | 'Core ]
                                               47    'Term          [ 'Base ]
                                               48    'Get           [ 'Base | 'Core ]
                                               
  49        'Koshucode.Baala.Toolkit.Library   /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               0     'Version       [ ]
                                               23    'Exit          [ 'Base ]
                                               46    'Element       [ 'Base | 'Core ]
                                               46    'RDF           [ 'Base | 'Core ]
                                               46    'Run           [ 'Base | 'Core ]
                                               48    'Input         [ 'Base | 'Core ]
                                               49    'Change        [ 'Base ]
                                               
  50        'Koshucode.Baala.Toolkit.Main      /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               47    'KoshuFilter   [ 'Base | 'Core ]
                                               47    'KoshuMain     [ 'Base | 'Core ]
                                               48    'KoshuRdf      [ 'Base ]
                                               48    'KoshuSyntax   [ 'Base | 'Core ]
                                               50    'KoshuChange   [ ]
                                               
  52        'Koshucode.Baala.Op.Lattice        /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               50    'Tropashko     [ 'Base | 'Builtin | 'Core ]
                                               51    'Restrict      [ 'Base | 'Builtin | 'Core ]
                                               52    'Rop           [ 'Builtin | 'Core ]
                                               
  54        'Koshucode.Baala.Op.Cox            /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               50    'Get           [ 'Base | 'Builtin | 'Core ]
                                               51    'Accessor      [ 'Base | 'Builtin | 'Core ]
                                               51    'Calc          [ 'Base | 'Builtin | 'Core ]
                                               51    'Filter        [ 'Base | 'Builtin | 'Core ]
                                               51    'Gadget        [ 'Base | 'Builtin | 'Core ]
                                               51    'Range         [ 'Base | 'Builtin | 'Core ]
                                               54    'Empty         [ 'Base | 'Builtin | 'Core | 'Lattice ]
                                               
  55        'Koshucode.Baala.Op.Nest           /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               50    'Flow          [ 'Base | 'Builtin | 'Core ]
                                               51    'Confl         [ 'Base | 'Builtin | 'Core ]
                                               54    'Deriv         [ 'Base | 'Builtin | 'Core | 'Lattice ]
                                               55    'Rop           [ 'Builtin | 'Core ]
                                               
  57        'Koshucode.Baala                   /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               22    'Base          [ 'Abort | 'Data | 'Prelude | 'Syntax | 'Text | 'Token ]
                                               45    'Core          [ 'Assert | 'Content | 'Lexmap | 'Relkit | 'Relmap | 'Resource ]
                                               57    'Op            [ 'Builtin | 'Lattice | 'Nest ]
                                               
  58        'Koshucode.Baala.Op                /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               23    'DepRank       [ 'Base ]
                                               46    'Message       [ 'Base | 'Core ]
                                               48    'Cop           [ ]
                                               48    'Quoter        [ 'Core ]
                                               49    'Builtin       [ ]
                                               50    'Gadget        [ 'Base | 'Builtin | 'Core ]
                                               50    'Meta          [ 'Base | 'Builtin | 'Core ]
                                               50    'Resource      [ 'Base | 'Builtin | 'Core ]
                                               50    'Source        [ 'Base | 'Builtin | 'Core ]
                                               50    'Term          [ 'Base | 'Builtin | 'Core ]
                                               51    'Peripheral    [ 'Base | 'Builtin | 'Core ]
                                               51    'TermGadget    [ 'Base | 'Builtin | 'Core ]
                                               53    'Lattice       [ ]
                                               54    'Check         [ 'Base | 'Builtin | 'Core | 'Lattice ]
                                               54    'Control       [ 'Base | 'Builtin | 'Core | 'Lattice ]
                                               55    'Cox           [ ]
                                               56    'Nest          [ ]
                                               58    'Global        [ 'Base | 'Cop | 'Core | 'Cox | 'Op ]
                                               

=== rel

**
**  SUMMARY
**      24 judges on DIR-RANK
**      24 judges in total
**

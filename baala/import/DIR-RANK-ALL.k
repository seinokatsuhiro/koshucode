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

|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Base.Prelude  /base-rank {| /rank : /base : /import-dir | 0 : 'Class : [ ] | 0 : 'Import : [ ] | 1 : 'Assoc : [ ] | 1 : 'List : [ ] | 1 : 'Pair : [ ] | 1 : 'Snip : [ ] | 2 : 'Order : [ ] |}
|-- DIR-RANK  /dir-rank 5  /dir 'Koshucode.Baala.Base.Text  /base-rank {| /rank : /base : /import-dir | 0 : 'Comment : [ ] | 0 : 'Unicode : [ ] | 4 : 'Http : [ 'Prelude ] | 4 : 'IOPoint : [ 'Prelude ] | 4 : 'TextTable : [ 'Prelude ] | 4 : 'Utility : [ 'Prelude ] | 4 : 'Write : [ 'Prelude ] | 5 : 'CodePt : [ 'Prelude ] |}
|-- DIR-RANK  /dir-rank 8  /dir 'Koshucode.Baala.Base.Abort  /base-rank {| /rank : /base : /import-dir | 7 : 'Reason : [ 'Text ] | 8 : 'Abortable : [ 'Prelude : 'Text ] | 8 : 'Report : [ 'Prelude : 'Text ] |}
|-- DIR-RANK  /dir-rank 12  /dir 'Koshucode.Baala.Base.Syntax  /base-rank {| /rank : /base : /import-dir | 0 : 'Line : [ ] | 11 : 'Code : [ 'Abort : 'Prelude : 'Text ] | 11 : 'Tree : [ 'Abort : 'Prelude : 'Text ] | 12 : 'Infix : [ 'Prelude ] |}
|-- DIR-RANK  /dir-rank 15  /dir 'Koshucode.Baala.Base.Token  /base-rank {| /rank : /base : /import-dir | 7 : 'AngleText : [ 'Prelude : 'Text ] | 7 : 'Token : [ 'Prelude : 'Text ] | 8 : 'Short : [ 'Prelude : 'Text ] | 14 : 'TokenLine : [ 'Abort : 'Prelude : 'Syntax : 'Text ] | 15 : 'TokenClause : [ 'Prelude : 'Syntax : 'Text ] | 15 : 'TokenTree : [ 'Abort : 'Prelude : 'Syntax : 'Text ] |}

|-- DIR-RANK  /dir-rank 21  /dir 'Koshucode.Baala.Base.Data  /base-rank {| /rank : /base : /import-dir | 11 : 'Clock : [ 'Abort : 'Prelude : 'Text ] | 11 : 'Date : [ 'Abort : 'Prelude : 'Text ] | 11 : 'Decimal : [ 'Abort : 'Prelude ] | 11 : 'Para : [ 'Abort : 'Prelude ] | 12 : 'Time : [ 'Abort : 'Prelude : 'Text ] | 17 : 'Interp : [ 'Prelude : 'Text : 'Token ] | 17 : 'Type : [ 'Prelude : 'Text : 'Token ] | 18 : 'Head : [ 'Abort : 'Prelude : 'Text : 'Token ] | 19 : 'Rel : [ 'Prelude : 'Text ] | 20 : 'Judge : [ 'Abort : 'Prelude : 'Text : 'Token ] | 20 : 'Mono : [ 'Abort ] | 21 : 'Output : [ 'Abort : 'Prelude : 'Text : 'Token ] |}
|-- DIR-RANK  /dir-rank 22  /dir 'Koshucode.Baala.Base  /base-rank {| /rank : /base : /import-dir | 3 : 'Prelude : [ ] | 6 : 'Text : [ ] | 9 : 'Abort : [ ] | 10 : 'Message : [ 'Abort : 'Prelude : 'Text ] | 13 : 'Syntax : [ ] | 16 : 'Token : [ ] | 22 : 'Data : [ ] |}
|-- DIR-RANK  /dir-rank 28  /dir 'Koshucode.Baala.Core.Content  /base-rank {| /rank : /base : /import-dir | 25 : 'Class : [ 'Base ] | 25 : 'Cox : [ 'Base ] | 25 : 'Tree : [ 'Base ] | 26 : 'Cop : [ 'Base ] | 26 : 'Literal : [ 'Base ] | 27 : 'Build : [ 'Base ] | 28 : 'Run : [ 'Base ] |}
|-- DIR-RANK  /dir-rank 29  /dir 'Koshucode.Baala.Core.Lexmap  /base-rank {| /rank : /base : /import-dir | 25 : 'AttrPos : [ 'Base ] | 26 : 'Attr : [ 'Base ] | 26 : 'Slot : [ 'Base ] | 27 : 'AttrEd : [ 'Base ] | 27 : 'Lexmap : [ 'Base ] | 28 : 'LexmapTrees : [ 'Base ] | 29 : 'Construct : [ 'Base ] |}
|-- DIR-RANK  /dir-rank 33  /dir 'Koshucode.Baala.Core.Relkit  /base-rank {| /rank : /base : /import-dir | 31 : 'Relkit : [ 'Base : 'Lexmap ] | 32 : 'Construct : [ 'Base ] | 33 : 'Run : [ 'Base : 'Content ] |}

|-- DIR-RANK  /dir-rank 37  /dir 'Koshucode.Baala.Core.Relmap  /base-rank {| /rank : /base : /import-dir | 30 : 'Option : [ 'Base : 'Content ] | 35 : 'Relmap : [ 'Base : 'Lexmap : 'Relkit ] | 36 : 'Rop : [ 'Base : 'Lexmap ] | 36 : 'Specialize : [ 'Base : 'Lexmap : 'Relkit ] | 37 : 'Construct : [ 'Base : 'Lexmap : 'Relkit ] | 37 : 'Global : [ 'Base : 'Content ] |}
|-- DIR-RANK  /dir-rank 40  /dir 'Koshucode.Baala.Core.Assert  /base-rank {| /rank : /base : /import-dir | 30 : 'RelTable : [ 'Base : 'Content ] | 35 : 'Dataset : [ 'Base : 'Content : 'Relkit ] | 39 : 'Assert : [ 'Base : 'Content : 'Lexmap : 'Relmap ] | 40 : 'Run : [ 'Base : 'Content : 'Lexmap : 'Relkit : 'Relmap ] |}
|-- DIR-RANK  /dir-rank 44  /dir 'Koshucode.Baala.Core.Resource  /base-rank {| /rank : /base : /import-dir | 31 : 'Clause : [ 'Base : 'Content : 'Lexmap ] | 42 : 'Resource : [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relkit : 'Relmap ] | 43 : 'Include : [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relmap ] | 43 : 'Run : [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relmap ] | 44 : 'Quoter : [ 'Base : 'Lexmap ] | 44 : 'Read : [ 'Base : 'Content : 'Relmap ] |}
|-- DIR-RANK  /dir-rank 45  /dir 'Koshucode.Baala.Core  /base-rank {| /rank : /base : /import-dir | 24 : 'Message : [ 'Base ] | 29 : 'Content : [ ] | 30 : 'Lexmap : [ ] | 34 : 'Relkit : [ ] | 38 : 'Relmap : [ ] | 41 : 'Assert : [ ] | 45 : 'Resource : [ ] |}
|-- DIR-RANK  /dir-rank 48  /dir 'Koshucode.Baala.Op.Cop  /base-rank {| /rank : /base : /import-dir | 47 : 'Coxhand : [ 'Base : 'Core ] | 48 : 'Arith : [ 'Base : 'Core ] | 48 : 'List : [ 'Base : 'Core ] | 48 : 'Logic : [ 'Base : 'Core ] | 48 : 'Misc : [ 'Base : 'Core ] | 48 : 'Order : [ 'Base : 'Core ] | 48 : 'Time : [ 'Base : 'Core ] | 48 : 'Type : [ 'Base : 'Core ] |}

|-- DIR-RANK  /dir-rank 48  /dir 'Koshucode.Baala.Type  /base-rank {| /rank : /base : /import-dir | 48 : 'Vanilla : [ 'Base : 'Core ] |}
|-- DIR-RANK  /dir-rank 49  /dir 'Koshucode.Baala.Op.Builtin  /base-rank {| /rank : /base : /import-dir | 47 : 'Define : [ 'Base : 'Core ] | 48 : 'Rop : [ 'Base : 'Core ] | 48 : 'Term : [ 'Base ] | 49 : 'Get : [ 'Base : 'Core ] |}
|-- DIR-RANK  /dir-rank 50  /dir 'Koshucode.Baala.Toolkit.Library  /base-rank {| /rank : /base : /import-dir | 0 : 'Version : [ ] | 24 : 'Exit : [ 'Base ] | 47 : 'Element : [ 'Base : 'Core ] | 47 : 'RDF : [ 'Base : 'Core ] | 47 : 'Run : [ 'Base : 'Core ] | 49 : 'Input : [ 'Base : 'Core ] | 50 : 'Change : [ 'Base ] |}
|-- DIR-RANK  /dir-rank 51  /dir 'Koshucode.Baala.Toolkit.Main  /base-rank {| /rank : /base : /import-dir | 48 : 'KoshuFilter : [ 'Base : 'Core ] | 48 : 'KoshuMain : [ 'Base : 'Core ] | 49 : 'KoshuRdf : [ 'Base ] | 49 : 'KoshuSyntax : [ 'Base : 'Core ] | 51 : 'KoshuChange : [ ] |}
|-- DIR-RANK  /dir-rank 53  /dir 'Koshucode.Baala.Op.Lattice  /base-rank {| /rank : /base : /import-dir | 51 : 'Tropashko : [ 'Base : 'Builtin : 'Core ] | 52 : 'Restrict : [ 'Base : 'Builtin : 'Core ] | 53 : 'Rop : [ 'Builtin : 'Core ] |}

|-- DIR-RANK  /dir-rank 55  /dir 'Koshucode.Baala.Op.Cox  /base-rank {| /rank : /base : /import-dir | 51 : 'Get : [ 'Base : 'Builtin : 'Core ] | 52 : 'Accessor : [ 'Base : 'Builtin : 'Core ] | 52 : 'Calc : [ 'Base : 'Builtin : 'Core ] | 52 : 'Filter : [ 'Base : 'Builtin : 'Core ] | 52 : 'Gadget : [ 'Base : 'Builtin : 'Core ] | 52 : 'Range : [ 'Base : 'Builtin : 'Core ] | 55 : 'Empty : [ 'Base : 'Builtin : 'Core : 'Lattice ] |}
|-- DIR-RANK  /dir-rank 56  /dir 'Koshucode.Baala.Op.Nest  /base-rank {| /rank : /base : /import-dir | 51 : 'Flow : [ 'Base : 'Builtin : 'Core ] | 52 : 'Confl : [ 'Base : 'Builtin : 'Core ] | 55 : 'Deriv : [ 'Base : 'Builtin : 'Core : 'Lattice ] | 56 : 'Rop : [ 'Builtin : 'Core ] |}
|-- DIR-RANK  /dir-rank 58  /dir 'Koshucode.Baala  /base-rank {| /rank : /base : /import-dir | 23 : 'Base : [ 'Abort : 'Data : 'Prelude : 'Syntax : 'Text : 'Token ] | 46 : 'Core : [ 'Assert : 'Content : 'Lexmap : 'Relkit : 'Relmap : 'Resource ] | 58 : 'Op : [ 'Builtin : 'Lattice : 'Nest ] |}
|-- DIR-RANK  /dir-rank 59  /dir 'Koshucode.Baala.Op  /base-rank {| /rank : /base : /import-dir | 24 : 'DepRank : [ 'Base ] | 47 : 'Message : [ 'Base : 'Core ] | 49 : 'Cop : [ ] | 49 : 'Quoter : [ 'Core ] | 50 : 'Builtin : [ ] | 51 : 'Gadget : [ 'Base : 'Builtin : 'Core ] | 51 : 'Meta : [ 'Base : 'Builtin : 'Core ] | 51 : 'Resource : [ 'Base : 'Builtin : 'Core ] | 51 : 'Source : [ 'Base : 'Builtin : 'Core ] | 51 : 'Term : [ 'Base : 'Builtin : 'Core ] | 52 : 'Peripheral : [ 'Base : 'Builtin : 'Core ] | 52 : 'TermGadget : [ 'Base : 'Builtin : 'Core ] | 54 : 'Lattice : [ ] | 55 : 'Check : [ 'Base : 'Builtin : 'Core : 'Lattice ] | 55 : 'Control : [ 'Base : 'Builtin : 'Core : 'Lattice ] | 56 : 'Cox : [ ] | 57 : 'Nest : [ ] | 59 : 'Global : [ 'Base : 'Cop : 'Core : 'Cox : 'Op ] |}

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
                                               8     'Abortable     [ 'Prelude : 'Text ]
                                               8     'Report        [ 'Prelude : 'Text ]
                                               
  12        'Koshucode.Baala.Base.Syntax       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               0     'Line          [ ]
                                               11    'Code          [ 'Abort : 'Prelude : 'Text ]
                                               11    'Tree          [ 'Abort : 'Prelude : 'Text ]
                                               12    'Infix         [ 'Prelude ]
                                               
  15        'Koshucode.Baala.Base.Token        /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               7     'AngleText     [ 'Prelude : 'Text ]
                                               7     'Token         [ 'Prelude : 'Text ]
                                               8     'Short         [ 'Prelude : 'Text ]
                                               14    'TokenLine     [ 'Abort : 'Prelude : 'Syntax : 'Text ]
                                               15    'TokenClause   [ 'Prelude : 'Syntax : 'Text ]
                                               15    'TokenTree     [ 'Abort : 'Prelude : 'Syntax : 'Text ]
                                               
  21        'Koshucode.Baala.Base.Data         /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               11    'Clock         [ 'Abort : 'Prelude : 'Text ]
                                               11    'Date          [ 'Abort : 'Prelude : 'Text ]
                                               11    'Decimal       [ 'Abort : 'Prelude ]
                                               11    'Para          [ 'Abort : 'Prelude ]
                                               12    'Time          [ 'Abort : 'Prelude : 'Text ]
                                               17    'Interp        [ 'Prelude : 'Text : 'Token ]
                                               17    'Type          [ 'Prelude : 'Text : 'Token ]
                                               18    'Head          [ 'Abort : 'Prelude : 'Text : 'Token ]
                                               19    'Rel           [ 'Prelude : 'Text ]
                                               20    'Judge         [ 'Abort : 'Prelude : 'Text : 'Token ]
                                               20    'Mono          [ 'Abort ]
                                               21    'Output        [ 'Abort : 'Prelude : 'Text : 'Token ]
                                               
  22        'Koshucode.Baala.Base              /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               3     'Prelude       [ ]
                                               6     'Text          [ ]
                                               9     'Abort         [ ]
                                               10    'Message       [ 'Abort : 'Prelude : 'Text ]
                                               13    'Syntax        [ ]
                                               16    'Token         [ ]
                                               22    'Data          [ ]
                                               
  28        'Koshucode.Baala.Core.Content      /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               25    'Class         [ 'Base ]
                                               25    'Cox           [ 'Base ]
                                               25    'Tree          [ 'Base ]
                                               26    'Cop           [ 'Base ]
                                               26    'Literal       [ 'Base ]
                                               27    'Build         [ 'Base ]
                                               28    'Run           [ 'Base ]
                                               
  29        'Koshucode.Baala.Core.Lexmap       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               25    'AttrPos       [ 'Base ]
                                               26    'Attr          [ 'Base ]
                                               26    'Slot          [ 'Base ]
                                               27    'AttrEd        [ 'Base ]
                                               27    'Lexmap        [ 'Base ]
                                               28    'LexmapTrees   [ 'Base ]
                                               29    'Construct     [ 'Base ]
                                               
  33        'Koshucode.Baala.Core.Relkit       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               31    'Relkit        [ 'Base : 'Lexmap ]
                                               32    'Construct     [ 'Base ]
                                               33    'Run           [ 'Base : 'Content ]
                                               
  37        'Koshucode.Baala.Core.Relmap       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               30    'Option        [ 'Base : 'Content ]
                                               35    'Relmap        [ 'Base : 'Lexmap : 'Relkit ]
                                               36    'Rop           [ 'Base : 'Lexmap ]
                                               36    'Specialize    [ 'Base : 'Lexmap : 'Relkit ]
                                               37    'Construct     [ 'Base : 'Lexmap : 'Relkit ]
                                               37    'Global        [ 'Base : 'Content ]
                                               
  40        'Koshucode.Baala.Core.Assert       /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               30    'RelTable      [ 'Base : 'Content ]
                                               35    'Dataset       [ 'Base : 'Content : 'Relkit ]
                                               39    'Assert        [ 'Base : 'Content : 'Lexmap : 'Relmap ]
                                               40    'Run           [ 'Base : 'Content : 'Lexmap : 'Relkit : 'Relmap ]
                                               
  44        'Koshucode.Baala.Core.Resource     /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               31    'Clause        [ 'Base : 'Content : 'Lexmap ]
                                               42    'Resource      [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relkit : 'Relmap ]
                                               43    'Include       [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relmap ]
                                               43    'Run           [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relmap ]
                                               44    'Quoter        [ 'Base : 'Lexmap ]
                                               44    'Read          [ 'Base : 'Content : 'Relmap ]
                                               
  45        'Koshucode.Baala.Core              /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               24    'Message       [ 'Base ]
                                               29    'Content       [ ]
                                               30    'Lexmap        [ ]
                                               34    'Relkit        [ ]
                                               38    'Relmap        [ ]
                                               41    'Assert        [ ]
                                               45    'Resource      [ ]
                                               
  48        'Koshucode.Baala.Op.Cop            /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               47    'Coxhand       [ 'Base : 'Core ]
                                               48    'Arith         [ 'Base : 'Core ]
                                               48    'List          [ 'Base : 'Core ]
                                               48    'Logic         [ 'Base : 'Core ]
                                               48    'Misc          [ 'Base : 'Core ]
                                               48    'Order         [ 'Base : 'Core ]
                                               48    'Time          [ 'Base : 'Core ]
                                               48    'Type          [ 'Base : 'Core ]
                                               
  48        'Koshucode.Baala.Type              /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               48    'Vanilla       [ 'Base : 'Core ]
                                               
  49        'Koshucode.Baala.Op.Builtin        /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               47    'Define        [ 'Base : 'Core ]
                                               48    'Rop           [ 'Base : 'Core ]
                                               48    'Term          [ 'Base ]
                                               49    'Get           [ 'Base : 'Core ]
                                               
  50        'Koshucode.Baala.Toolkit.Library   /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               0     'Version       [ ]
                                               24    'Exit          [ 'Base ]
                                               47    'Element       [ 'Base : 'Core ]
                                               47    'RDF           [ 'Base : 'Core ]
                                               47    'Run           [ 'Base : 'Core ]
                                               49    'Input         [ 'Base : 'Core ]
                                               50    'Change        [ 'Base ]
                                               
  51        'Koshucode.Baala.Toolkit.Main      /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               48    'KoshuFilter   [ 'Base : 'Core ]
                                               48    'KoshuMain     [ 'Base : 'Core ]
                                               49    'KoshuRdf      [ 'Base ]
                                               49    'KoshuSyntax   [ 'Base : 'Core ]
                                               51    'KoshuChange   [ ]
                                               
  53        'Koshucode.Baala.Op.Lattice        /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               51    'Tropashko     [ 'Base : 'Builtin : 'Core ]
                                               52    'Restrict      [ 'Base : 'Builtin : 'Core ]
                                               53    'Rop           [ 'Builtin : 'Core ]
                                               
  55        'Koshucode.Baala.Op.Cox            /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               51    'Get           [ 'Base : 'Builtin : 'Core ]
                                               52    'Accessor      [ 'Base : 'Builtin : 'Core ]
                                               52    'Calc          [ 'Base : 'Builtin : 'Core ]
                                               52    'Filter        [ 'Base : 'Builtin : 'Core ]
                                               52    'Gadget        [ 'Base : 'Builtin : 'Core ]
                                               52    'Range         [ 'Base : 'Builtin : 'Core ]
                                               55    'Empty         [ 'Base : 'Builtin : 'Core : 'Lattice ]
                                               
  56        'Koshucode.Baala.Op.Nest           /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               51    'Flow          [ 'Base : 'Builtin : 'Core ]
                                               52    'Confl         [ 'Base : 'Builtin : 'Core ]
                                               55    'Deriv         [ 'Base : 'Builtin : 'Core : 'Lattice ]
                                               56    'Rop           [ 'Builtin : 'Core ]
                                               
  58        'Koshucode.Baala                   /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               23    'Base          [ 'Abort : 'Data : 'Prelude : 'Syntax : 'Text : 'Token ]
                                               46    'Core          [ 'Assert : 'Content : 'Lexmap : 'Relkit : 'Relmap : 'Resource ]
                                               58    'Op            [ 'Builtin : 'Lattice : 'Nest ]
                                               
  59        'Koshucode.Baala.Op                /rank /base          /import-dir
                                               ----- -------------- ------------------------------------------------------------------
                                               24    'DepRank       [ 'Base ]
                                               47    'Message       [ 'Base : 'Core ]
                                               49    'Cop           [ ]
                                               49    'Quoter        [ 'Core ]
                                               50    'Builtin       [ ]
                                               51    'Gadget        [ 'Base : 'Builtin : 'Core ]
                                               51    'Meta          [ 'Base : 'Builtin : 'Core ]
                                               51    'Resource      [ 'Base : 'Builtin : 'Core ]
                                               51    'Source        [ 'Base : 'Builtin : 'Core ]
                                               51    'Term          [ 'Base : 'Builtin : 'Core ]
                                               52    'Peripheral    [ 'Base : 'Builtin : 'Core ]
                                               52    'TermGadget    [ 'Base : 'Builtin : 'Core ]
                                               54    'Lattice       [ ]
                                               55    'Check         [ 'Base : 'Builtin : 'Core : 'Lattice ]
                                               55    'Control       [ 'Base : 'Builtin : 'Core : 'Lattice ]
                                               56    'Cox           [ ]
                                               57    'Nest          [ ]
                                               59    'Global        [ 'Base : 'Cop : 'Core : 'Cox : 'Op ]
                                               

=== rel

**
**  SUMMARY
**      24 judges on DIR-RANK
**      24 judges in total
**

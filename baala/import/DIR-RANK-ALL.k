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

|-- DIR-RANK  /dir-rank 2  /dir 'Koshucode.Baala.Base.Prelude  /base-rank {| /rank : /base : /import-dir | 0 : 'Class : [ ] | 0 : 'Import : [ ] | 1 : 'Assoc : [ ] | 1 : 'List : [ ] | 1 : 'Pair : [ ] | 1 : 'Snip : [ ] | 2 : 'Order : [ ] |}
|-- DIR-RANK  /dir-rank 5  /dir 'Koshucode.Baala.Base.Text  /base-rank {| /rank : /base : /import-dir | 0 : 'Comment : [ ] | 0 : 'Unicode : [ ] | 4 : 'CodeName : [ 'Prelude ] | 4 : 'Http : [ 'Prelude ] | 4 : 'TextTable : [ 'Prelude ] | 4 : 'Utility : [ 'Prelude ] | 4 : 'Write : [ 'Prelude ] | 5 : 'CodePt : [ 'Prelude ] |}
|-- DIR-RANK  /dir-rank 8  /dir 'Koshucode.Baala.Base.Abort  /base-rank {| /rank : /base : /import-dir | 7 : 'Reason : [ 'Text ] | 8 : 'Abortable : [ 'Prelude : 'Text ] | 8 : 'Report : [ 'Prelude : 'Text ] |}
|-- DIR-RANK  /dir-rank 12  /dir 'Koshucode.Baala.Base.Syntax  /base-rank {| /rank : /base : /import-dir | 0 : 'Line : [ ] | 11 : 'Code : [ 'Abort : 'Prelude : 'Text ] | 11 : 'Tree : [ 'Abort : 'Prelude : 'Text ] | 12 : 'Infix : [ 'Prelude ] |}
|-- DIR-RANK  /dir-rank 15  /dir 'Koshucode.Baala.Base.Token  /base-rank {| /rank : /base : /import-dir | 7 : 'AngleText : [ 'Prelude : 'Text ] | 7 : 'Token : [ 'Prelude : 'Text ] | 8 : 'Short : [ 'Prelude : 'Text ] | 14 : 'TokenLine : [ 'Abort : 'Prelude : 'Syntax : 'Text ] | 15 : 'TokenClause : [ 'Prelude : 'Syntax : 'Text ] | 15 : 'TokenTree : [ 'Abort : 'Prelude : 'Syntax : 'Text ] |}

|-- DIR-RANK  /dir-rank 21  /dir 'Koshucode.Baala.Base.Data  /base-rank {| /rank : /base : /import-dir | 11 : 'Clock : [ 'Abort : 'Prelude : 'Text ] | 11 : 'Date : [ 'Abort : 'Prelude : 'Text ] | 11 : 'Decimal : [ 'Abort : 'Prelude ] | 12 : 'Time : [ 'Abort : 'Prelude : 'Text ] | 17 : 'Interp : [ 'Prelude : 'Text : 'Token ] | 17 : 'Type : [ 'Prelude : 'Text : 'Token ] | 18 : 'Head : [ 'Prelude : 'Text : 'Token ] | 19 : 'Rel : [ 'Prelude : 'Text ] | 20 : 'Judge : [ 'Abort : 'Prelude : 'Text : 'Token ] | 20 : 'Mono : [ 'Abort ] | 21 : 'Output : [ 'Prelude : 'Text : 'Token ] |}
|-- DIR-RANK  /dir-rank 22  /dir 'Koshucode.Baala.Base  /base-rank {| /rank : /base : /import-dir | 3 : 'Prelude : [ ] | 6 : 'Text : [ ] | 9 : 'Abort : [ ] | 10 : 'Message : [ 'Abort : 'Prelude : 'Text ] | 13 : 'Syntax : [ ] | 16 : 'Token : [ ] | 22 : 'Data : [ ] |}
|-- DIR-RANK  /dir-rank 28  /dir 'Koshucode.Baala.Core.Content  /base-rank {| /rank : /base : /import-dir | 25 : 'Class : [ 'Base ] | 25 : 'Cox : [ 'Base ] | 25 : 'Tree : [ 'Base ] | 26 : 'Cop : [ 'Base ] | 26 : 'Literal : [ 'Base ] | 27 : 'Build : [ 'Base ] | 28 : 'Run : [ 'Base ] |}
|-- DIR-RANK  /dir-rank 28  /dir 'Koshucode.Baala.Core.Lexmap  /base-rank {| /rank : /base : /import-dir | 25 : 'Attribute : [ 'Base ] | 26 : 'Slot : [ 'Base ] | 26 : 'Sorter : [ 'Base ] | 27 : 'Attrmap : [ 'Base ] | 28 : 'Lexmap : [ 'Base ] |}
|-- DIR-RANK  /dir-rank 34  /dir 'Koshucode.Baala.Core.Relmap  /base-rank {| /rank : /base : /import-dir | 30 : 'Relkit : [ 'Base : 'Lexmap ] | 31 : 'Relmap : [ 'Base : 'Lexmap ] | 32 : 'Rop : [ 'Base : 'Lexmap ] | 33 : 'Global : [ 'Base : 'Content : 'Lexmap ] | 34 : 'Construct : [ 'Base : 'Lexmap ] | 34 : 'Run : [ 'Base : 'Content ] | 34 : 'Specialize : [ 'Base : 'Lexmap ] |}

|-- DIR-RANK  /dir-rank 37  /dir 'Koshucode.Baala.Core.Assert  /base-rank {| /rank : /base : /import-dir | 30 : 'RelTable : [ 'Base : 'Content ] | 36 : 'Assert : [ 'Base : 'Content : 'Lexmap : 'Relmap ] | 36 : 'Dataset : [ 'Base : 'Content : 'Relmap ] | 37 : 'Run : [ 'Base : 'Content : 'Relmap ] |}
|-- DIR-RANK  /dir-rank 41  /dir 'Koshucode.Baala.Core.Resource  /base-rank {| /rank : /base : /import-dir | 30 : 'Clause : [ 'Base : 'Content : 'Lexmap ] | 39 : 'Resource : [ 'Assert : 'Base : 'Lexmap : 'Relmap ] | 40 : 'Include : [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relmap ] | 40 : 'Run : [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relmap ] | 41 : 'Quoter : [ 'Base : 'Lexmap ] | 41 : 'Read : [ 'Base : 'Content : 'Relmap ] |}
|-- DIR-RANK  /dir-rank 42  /dir 'Koshucode.Baala.Core  /base-rank {| /rank : /base : /import-dir | 24 : 'Message : [ 'Base ] | 29 : 'Content : [ ] | 29 : 'Lexmap : [ ] | 35 : 'Relmap : [ ] | 38 : 'Assert : [ ] | 42 : 'Resource : [ ] |}
|-- DIR-RANK  /dir-rank 45  /dir 'Koshucode.Baala.Op.Cop  /base-rank {| /rank : /base : /import-dir | 44 : 'Coxhand : [ 'Base : 'Core ] | 45 : 'Arith : [ 'Base : 'Core ] | 45 : 'List : [ 'Base : 'Core ] | 45 : 'Logic : [ 'Base : 'Core ] | 45 : 'Misc : [ 'Base : 'Core ] | 45 : 'Order : [ 'Base : 'Core ] | 45 : 'Time : [ 'Base : 'Core ] |}
|-- DIR-RANK  /dir-rank 45  /dir 'Koshucode.Baala.Type  /base-rank {| /rank : /base : /import-dir | 45 : 'Vanilla : [ 'Base : 'Core ] |}

|-- DIR-RANK  /dir-rank 46  /dir 'Koshucode.Baala.Op.Builtin  /base-rank {| /rank : /base : /import-dir | 44 : 'Define : [ 'Base : 'Core ] | 45 : 'Rop : [ 'Base : 'Core ] | 45 : 'Term : [ 'Base ] | 46 : 'Get : [ 'Base : 'Core ] |}
|-- DIR-RANK  /dir-rank 47  /dir 'Koshucode.Baala.Toolkit.Library  /base-rank {| /rank : /base : /import-dir | 0 : 'Version : [ ] | 24 : 'Exit : [ 'Base ] | 44 : 'Element : [ 'Base : 'Core ] | 44 : 'RDF : [ 'Base : 'Core ] | 44 : 'Run : [ 'Base : 'Core ] | 46 : 'Input : [ 'Base : 'Core ] | 47 : 'Change : [ 'Base ] |}
|-- DIR-RANK  /dir-rank 48  /dir 'Koshucode.Baala.Toolkit.Main  /base-rank {| /rank : /base : /import-dir | 45 : 'KoshuMain : [ 'Base : 'Core ] | 46 : 'KoshuRdf : [ 'Base ] | 46 : 'KoshuSyntax : [ 'Base : 'Core ] | 48 : 'KoshuChange : [ ] |}
|-- DIR-RANK  /dir-rank 50  /dir 'Koshucode.Baala.Op.Lattice  /base-rank {| /rank : /base : /import-dir | 48 : 'Tropashko : [ 'Base : 'Builtin : 'Core ] | 49 : 'Restrict : [ 'Base : 'Builtin : 'Core ] | 50 : 'Rop : [ 'Builtin : 'Core ] |}
|-- DIR-RANK  /dir-rank 52  /dir 'Koshucode.Baala.Op.Cox  /base-rank {| /rank : /base : /import-dir | 48 : 'Get : [ 'Base : 'Builtin : 'Core ] | 49 : 'Accessor : [ 'Base : 'Builtin : 'Core ] | 49 : 'Calc : [ 'Base : 'Builtin : 'Core ] | 49 : 'Filter : [ 'Base : 'Builtin : 'Core ] | 49 : 'Gadget : [ 'Base : 'Builtin : 'Core ] | 49 : 'Range : [ 'Base : 'Builtin : 'Core ] | 52 : 'Empty : [ 'Base : 'Builtin : 'Core : 'Lattice ] |}

|-- DIR-RANK  /dir-rank 53  /dir 'Koshucode.Baala.Op.Nest  /base-rank {| /rank : /base : /import-dir | 48 : 'Flow : [ 'Base : 'Builtin : 'Core ] | 49 : 'Confl : [ 'Base : 'Builtin : 'Core ] | 52 : 'Deriv : [ 'Base : 'Builtin : 'Core : 'Lattice ] | 53 : 'Rop : [ 'Builtin : 'Core ] |}
|-- DIR-RANK  /dir-rank 55  /dir 'Koshucode.Baala  /base-rank {| /rank : /base : /import-dir | 23 : 'Base : [ 'Abort : 'Data : 'Prelude : 'Syntax : 'Text : 'Token ] | 43 : 'Core : [ 'Assert : 'Content : 'Lexmap : 'Relmap : 'Resource ] | 55 : 'Op : [ 'Builtin : 'Lattice : 'Nest ] |}
|-- DIR-RANK  /dir-rank 56  /dir 'Koshucode.Baala.Op  /base-rank {| /rank : /base : /import-dir | 24 : 'DepRank : [ 'Base ] | 44 : 'Message : [ 'Base : 'Core ] | 46 : 'Cop : [ ] | 46 : 'Quoter : [ 'Core ] | 47 : 'Builtin : [ ] | 48 : 'Gadget : [ 'Base : 'Builtin : 'Core ] | 48 : 'Meta : [ 'Base : 'Builtin : 'Core ] | 48 : 'Resource : [ 'Base : 'Builtin : 'Core ] | 48 : 'Source : [ 'Base : 'Builtin : 'Core ] | 48 : 'Term : [ 'Base : 'Builtin : 'Core ] | 49 : 'Peripheral : [ 'Base : 'Builtin : 'Core ] | 49 : 'TermGadget : [ 'Base : 'Builtin : 'Core ] | 51 : 'Lattice : [ ] | 52 : 'Check : [ 'Base : 'Builtin : 'Core : 'Lattice ] | 52 : 'Control : [ 'Base : 'Builtin : 'Core : 'Lattice ] | 53 : 'Cox : [ ] | 54 : 'Nest : [ ] | 56 : 'Global : [ 'Base : 'Cop : 'Core : 'Cox : 'Op ] |}

*** 23 judges

**  TABLE : DIR-RANK
**
**    /dir-rank /dir                               /base-rank
**    --------- ---------------------------------- -------------------------------------------------------------------------------
**    2         'Koshucode.Baala.Base.Prelude      /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 0     'Class         [ ]
**                                                 0     'Import        [ ]
**                                                 1     'Assoc         [ ]
**                                                 1     'List          [ ]
**                                                 1     'Pair          [ ]
**                                                 1     'Snip          [ ]
**                                                 2     'Order         [ ]
**                                                 
**    5         'Koshucode.Baala.Base.Text         /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 0     'Comment       [ ]
**                                                 0     'Unicode       [ ]
**                                                 4     'CodeName      [ 'Prelude ]
**                                                 4     'Http          [ 'Prelude ]
**                                                 4     'TextTable     [ 'Prelude ]
**                                                 4     'Utility       [ 'Prelude ]
**                                                 4     'Write         [ 'Prelude ]
**                                                 5     'CodePt        [ 'Prelude ]
**                                                 
**    8         'Koshucode.Baala.Base.Abort        /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 7     'Reason        [ 'Text ]
**                                                 8     'Abortable     [ 'Prelude : 'Text ]
**                                                 8     'Report        [ 'Prelude : 'Text ]
**                                                 
**    12        'Koshucode.Baala.Base.Syntax       /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 0     'Line          [ ]
**                                                 11    'Code          [ 'Abort : 'Prelude : 'Text ]
**                                                 11    'Tree          [ 'Abort : 'Prelude : 'Text ]
**                                                 12    'Infix         [ 'Prelude ]
**                                                 
**    15        'Koshucode.Baala.Base.Token        /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 7     'AngleText     [ 'Prelude : 'Text ]
**                                                 7     'Token         [ 'Prelude : 'Text ]
**                                                 8     'Short         [ 'Prelude : 'Text ]
**                                                 14    'TokenLine     [ 'Abort : 'Prelude : 'Syntax : 'Text ]
**                                                 15    'TokenClause   [ 'Prelude : 'Syntax : 'Text ]
**                                                 15    'TokenTree     [ 'Abort : 'Prelude : 'Syntax : 'Text ]
**                                                 
**    21        'Koshucode.Baala.Base.Data         /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 11    'Clock         [ 'Abort : 'Prelude : 'Text ]
**                                                 11    'Date          [ 'Abort : 'Prelude : 'Text ]
**                                                 11    'Decimal       [ 'Abort : 'Prelude ]
**                                                 12    'Time          [ 'Abort : 'Prelude : 'Text ]
**                                                 17    'Interp        [ 'Prelude : 'Text : 'Token ]
**                                                 17    'Type          [ 'Prelude : 'Text : 'Token ]
**                                                 18    'Head          [ 'Prelude : 'Text : 'Token ]
**                                                 19    'Rel           [ 'Prelude : 'Text ]
**                                                 20    'Judge         [ 'Abort : 'Prelude : 'Text : 'Token ]
**                                                 20    'Mono          [ 'Abort ]
**                                                 21    'Output        [ 'Prelude : 'Text : 'Token ]
**                                                 
**    22        'Koshucode.Baala.Base              /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 3     'Prelude       [ ]
**                                                 6     'Text          [ ]
**                                                 9     'Abort         [ ]
**                                                 10    'Message       [ 'Abort : 'Prelude : 'Text ]
**                                                 13    'Syntax        [ ]
**                                                 16    'Token         [ ]
**                                                 22    'Data          [ ]
**                                                 
**    28        'Koshucode.Baala.Core.Content      /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 25    'Class         [ 'Base ]
**                                                 25    'Cox           [ 'Base ]
**                                                 25    'Tree          [ 'Base ]
**                                                 26    'Cop           [ 'Base ]
**                                                 26    'Literal       [ 'Base ]
**                                                 27    'Build         [ 'Base ]
**                                                 28    'Run           [ 'Base ]
**                                                 
**    28        'Koshucode.Baala.Core.Lexmap       /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 25    'Attribute     [ 'Base ]
**                                                 26    'Slot          [ 'Base ]
**                                                 26    'Sorter        [ 'Base ]
**                                                 27    'Attrmap       [ 'Base ]
**                                                 28    'Lexmap        [ 'Base ]
**                                                 
**    34        'Koshucode.Baala.Core.Relmap       /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 30    'Relkit        [ 'Base : 'Lexmap ]
**                                                 31    'Relmap        [ 'Base : 'Lexmap ]
**                                                 32    'Rop           [ 'Base : 'Lexmap ]
**                                                 33    'Global        [ 'Base : 'Content : 'Lexmap ]
**                                                 34    'Construct     [ 'Base : 'Lexmap ]
**                                                 34    'Run           [ 'Base : 'Content ]
**                                                 34    'Specialize    [ 'Base : 'Lexmap ]
**                                                 
**    37        'Koshucode.Baala.Core.Assert       /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 30    'RelTable      [ 'Base : 'Content ]
**                                                 36    'Assert        [ 'Base : 'Content : 'Lexmap : 'Relmap ]
**                                                 36    'Dataset       [ 'Base : 'Content : 'Relmap ]
**                                                 37    'Run           [ 'Base : 'Content : 'Relmap ]
**                                                 
**    41        'Koshucode.Baala.Core.Resource     /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 30    'Clause        [ 'Base : 'Content : 'Lexmap ]
**                                                 39    'Resource      [ 'Assert : 'Base : 'Lexmap : 'Relmap ]
**                                                 40    'Include       [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relmap ]
**                                                 40    'Run           [ 'Assert : 'Base : 'Content : 'Lexmap : 'Relmap ]
**                                                 41    'Quoter        [ 'Base : 'Lexmap ]
**                                                 41    'Read          [ 'Base : 'Content : 'Relmap ]
**                                                 
**    42        'Koshucode.Baala.Core              /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 24    'Message       [ 'Base ]
**                                                 29    'Content       [ ]
**                                                 29    'Lexmap        [ ]
**                                                 35    'Relmap        [ ]
**                                                 38    'Assert        [ ]
**                                                 42    'Resource      [ ]
**                                                 
**    45        'Koshucode.Baala.Op.Cop            /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 44    'Coxhand       [ 'Base : 'Core ]
**                                                 45    'Arith         [ 'Base : 'Core ]
**                                                 45    'List          [ 'Base : 'Core ]
**                                                 45    'Logic         [ 'Base : 'Core ]
**                                                 45    'Misc          [ 'Base : 'Core ]
**                                                 45    'Order         [ 'Base : 'Core ]
**                                                 45    'Time          [ 'Base : 'Core ]
**                                                 
**    45        'Koshucode.Baala.Type              /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 45    'Vanilla       [ 'Base : 'Core ]
**                                                 
**    46        'Koshucode.Baala.Op.Builtin        /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 44    'Define        [ 'Base : 'Core ]
**                                                 45    'Rop           [ 'Base : 'Core ]
**                                                 45    'Term          [ 'Base ]
**                                                 46    'Get           [ 'Base : 'Core ]
**                                                 
**    47        'Koshucode.Baala.Toolkit.Library   /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 0     'Version       [ ]
**                                                 24    'Exit          [ 'Base ]
**                                                 44    'Element       [ 'Base : 'Core ]
**                                                 44    'RDF           [ 'Base : 'Core ]
**                                                 44    'Run           [ 'Base : 'Core ]
**                                                 46    'Input         [ 'Base : 'Core ]
**                                                 47    'Change        [ 'Base ]
**                                                 
**    48        'Koshucode.Baala.Toolkit.Main      /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 45    'KoshuMain     [ 'Base : 'Core ]
**                                                 46    'KoshuRdf      [ 'Base ]
**                                                 46    'KoshuSyntax   [ 'Base : 'Core ]
**                                                 48    'KoshuChange   [ ]
**                                                 
**    50        'Koshucode.Baala.Op.Lattice        /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 48    'Tropashko     [ 'Base : 'Builtin : 'Core ]
**                                                 49    'Restrict      [ 'Base : 'Builtin : 'Core ]
**                                                 50    'Rop           [ 'Builtin : 'Core ]
**                                                 
**    52        'Koshucode.Baala.Op.Cox            /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 48    'Get           [ 'Base : 'Builtin : 'Core ]
**                                                 49    'Accessor      [ 'Base : 'Builtin : 'Core ]
**                                                 49    'Calc          [ 'Base : 'Builtin : 'Core ]
**                                                 49    'Filter        [ 'Base : 'Builtin : 'Core ]
**                                                 49    'Gadget        [ 'Base : 'Builtin : 'Core ]
**                                                 49    'Range         [ 'Base : 'Builtin : 'Core ]
**                                                 52    'Empty         [ 'Base : 'Builtin : 'Core : 'Lattice ]
**                                                 
**    53        'Koshucode.Baala.Op.Nest           /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 48    'Flow          [ 'Base : 'Builtin : 'Core ]
**                                                 49    'Confl         [ 'Base : 'Builtin : 'Core ]
**                                                 52    'Deriv         [ 'Base : 'Builtin : 'Core : 'Lattice ]
**                                                 53    'Rop           [ 'Builtin : 'Core ]
**                                                 
**    55        'Koshucode.Baala                   /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 23    'Base          [ 'Abort : 'Data : 'Prelude : 'Syntax : 'Text : 'Token ]
**                                                 43    'Core          [ 'Assert : 'Content : 'Lexmap : 'Relmap : 'Resource ]
**                                                 55    'Op            [ 'Builtin : 'Lattice : 'Nest ]
**                                                 
**    56        'Koshucode.Baala.Op                /rank /base          /import-dir
**                                                 ----- -------------- ----------------------------------------------------------
**                                                 24    'DepRank       [ 'Base ]
**                                                 44    'Message       [ 'Base : 'Core ]
**                                                 46    'Cop           [ ]
**                                                 46    'Quoter        [ 'Core ]
**                                                 47    'Builtin       [ ]
**                                                 48    'Gadget        [ 'Base : 'Builtin : 'Core ]
**                                                 48    'Meta          [ 'Base : 'Builtin : 'Core ]
**                                                 48    'Resource      [ 'Base : 'Builtin : 'Core ]
**                                                 48    'Source        [ 'Base : 'Builtin : 'Core ]
**                                                 48    'Term          [ 'Base : 'Builtin : 'Core ]
**                                                 49    'Peripheral    [ 'Base : 'Builtin : 'Core ]
**                                                 49    'TermGadget    [ 'Base : 'Builtin : 'Core ]
**                                                 51    'Lattice       [ ]
**                                                 52    'Check         [ 'Base : 'Builtin : 'Core : 'Lattice ]
**                                                 52    'Control       [ 'Base : 'Builtin : 'Core : 'Lattice ]
**                                                 53    'Cox           [ ]
**                                                 54    'Nest          [ ]
**                                                 56    'Global        [ 'Base : 'Cop : 'Core : 'Cox : 'Op ]
**                                                 

**
**  SUMMARY
**      23 judges on DIR-RANK
**      23 judges in total
**

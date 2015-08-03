** -*- koshu -*-
**
**  INPUT
**    sloc-all-list.k
**    base/SLOC.k
**    calculator/SLOC.k
**    content/SLOC.k
**    core/SLOC.k
**    operator/SLOC.k
**    toolkit/SLOC.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR  /package 'base  /code 14  /comment 4  /dir "base/Koshucode/Baala"
|-- DIR  /package 'base  /code 169  /comment 82  /dir "base/Koshucode/Baala/Base"
|-- DIR  /package 'base  /code 117  /comment 86  /dir "base/Koshucode/Baala/Base/Abort"
|-- DIR  /package 'base  /code 1 588  /comment 863  /dir "base/Koshucode/Baala/Base/Data"
|-- DIR  /package 'base  /code 526  /comment 485  /dir "base/Koshucode/Baala/Base/Prelude"

|-- DIR  /package 'base  /code 281  /comment 192  /dir "base/Koshucode/Baala/Base/Syntax"
|-- DIR  /package 'base  /code 559  /comment 306  /dir "base/Koshucode/Baala/Base/Text"
|-- DIR  /package 'base  /code 806  /comment 542  /dir "base/Koshucode/Baala/Base/Token"
|-- DIR  /package 'calculator  /code 142  /comment 107  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- DIR  /package 'calculator  /code 186  /comment 123  /dir "calculator/Koshucode/Baala/Toolkit/Main"

|-- DIR  /package 'content  /code 124  /comment 45  /dir "content/Koshucode/Baala/Op"
|-- DIR  /package 'content  /code 647  /comment 381  /dir "content/Koshucode/Baala/Op/Cop"
|-- DIR  /package 'content  /code 717  /comment 371  /dir "content/Koshucode/Baala/Op/Cox"
|-- DIR  /package 'content  /code 180  /comment 45  /dir "content/Koshucode/Baala/Type"
|-- DIR  /package 'core  /code 15  /comment 72  /dir "core/Koshucode/Baala"

|-- DIR  /package 'core  /code 358  /comment 234  /dir "core/Koshucode/Baala/Core"
|-- DIR  /package 'core  /code 259  /comment 129  /dir "core/Koshucode/Baala/Core/Assert"
|-- DIR  /package 'core  /code 1 004  /comment 564  /dir "core/Koshucode/Baala/Core/Content"
|-- DIR  /package 'core  /code 475  /comment 268  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- DIR  /package 'core  /code 211  /comment 110  /dir "core/Koshucode/Baala/Core/Relkit"

|-- DIR  /package 'core  /code 434  /comment 238  /dir "core/Koshucode/Baala/Core/Relmap"
|-- DIR  /package 'core  /code 600  /comment 269  /dir "core/Koshucode/Baala/Core/Resource"
|-- DIR  /package 'operator  /code 29  /comment 6  /dir "operator/Koshucode/Baala"
|-- DIR  /package 'operator  /code 1 193  /comment 737  /dir "operator/Koshucode/Baala/Op"
|-- DIR  /package 'operator  /code 227  /comment 152  /dir "operator/Koshucode/Baala/Op/Builtin"
*** 25

|-- DIR  /package 'operator  /code 181  /comment 174  /dir "operator/Koshucode/Baala/Op/Lattice"
|-- DIR  /package 'operator  /code 239  /comment 241  /dir "operator/Koshucode/Baala/Op/Nest"
|-- DIR  /package 'toolkit  /code 127  /comment 75  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- DIR  /package 'toolkit  /code 353  /comment 146  /dir "toolkit/Koshucode/Baala/Toolkit/Main"

*** 29 judges

|-- DIR-TOTAL  /package 'base  /code 4 060  /comment 2 560
|-- DIR-TOTAL  /package 'calculator  /code 328  /comment 230
|-- DIR-TOTAL  /package 'content  /code 1 668  /comment 842
|-- DIR-TOTAL  /package 'core  /code 3 356  /comment 1 884
|-- DIR-TOTAL  /package 'operator  /code 1 869  /comment 1 310

|-- DIR-TOTAL  /package 'toolkit  /code 480  /comment 221

*** 6 judges

|-- FILE  /package 'base  /code 14  /comment 4  /file 'Base.hs  /dir "base/Koshucode/Baala"
|-- FILE  /package 'base  /code 8  /comment 6  /file 'Abort.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 22  /comment 12  /file 'Abortable.hs  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 45  /comment 38  /file 'Reason.hs  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 50  /comment 36  /file 'Report.hs  /dir "base/Koshucode/Baala/Base/Abort"

|-- FILE  /package 'base  /code 29  /comment 5  /file 'Data.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 12  /comment 5  /file 'About.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 185  /comment 84  /file 'Clock.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 73  /comment 30  /file 'Date.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 169  /comment 66  /file 'Decimal.hs  /dir "base/Koshucode/Baala/Base/Data"

|-- FILE  /package 'base  /code 152  /comment 155  /file 'Head.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 35  /comment 12  /file 'Interp.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 119  /comment 73  /file 'Judge.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 30  /comment 23  /file 'Mono.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 193  /comment 73  /file 'Para.hs  /dir "base/Koshucode/Baala/Base/Data"

|-- FILE  /package 'base  /code 67  /comment 67  /file 'Rel.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 239  /comment 77  /file 'Result.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 179  /comment 107  /file 'Time.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 135  /comment 91  /file 'Type.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 72  /comment 53  /file 'Message.hs  /dir "base/Koshucode/Baala/Base"

|-- FILE  /package 'base  /code 16  /comment 3  /file 'Prelude.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 126  /comment 145  /file 'Assoc.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 33  /comment 20  /file 'Class.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 61  /comment 34  /file 'Import.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 122  /comment 126  /file 'List.hs  /dir "base/Koshucode/Baala/Base/Prelude"
*** 25

|-- FILE  /package 'base  /code 75  /comment 34  /file 'Order.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 35  /comment 12  /file 'Pair.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 74  /comment 114  /file 'Snip.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 10  /comment 6  /file 'Syntax.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 91  /comment 55  /file 'Code.hs  /dir "base/Koshucode/Baala/Base/Syntax"

|-- FILE  /package 'base  /code 68  /comment 50  /file 'Infix.hs  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 20  /comment 14  /file 'Line.hs  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 102  /comment 73  /file 'Tree.hs  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 18  /comment 3  /file 'Text.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 58  /comment 33  /file 'CodePt.hs  /dir "base/Koshucode/Baala/Base/Text"

|-- FILE  /package 'base  /code 36  /comment 24  /file 'Comment.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 76  /comment 14  /file 'Http.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 53  /comment 28  /file 'IOPoint.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 124  /comment 57  /file 'TextTable.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 65  /comment 14  /file 'Unicode.hs  /dir "base/Koshucode/Baala/Base/Text"

|-- FILE  /package 'base  /code 53  /comment 43  /file 'Utility.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 94  /comment 93  /file 'Write.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 16  /comment 6  /file 'Token.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 53  /comment 72  /file 'AngleText.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'base  /code 57  /comment 32  /file 'Short.hs  /dir "base/Koshucode/Baala/Base/Token"

|-- FILE  /package 'base  /code 32  /comment 33  /file 'Term.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'base  /code 253  /comment 100  /file 'Token.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'base  /code 36  /comment 14  /file 'TokenClause.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'base  /code 261  /comment 192  /file 'TokenLine.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'base  /code 114  /comment 99  /file 'TokenTree.hs  /dir "base/Koshucode/Baala/Base/Token"
*** 50

|-- FILE  /package 'calculator  /code 62  /comment 49  /file 'Element.hs  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 44  /comment 16  /file 'Exit.hs  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 26  /comment 34  /file 'Run.hs  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 10  /comment 8  /file 'Version.hs  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 71  /comment 14  /file 'KoshuFilter.hs  /dir "calculator/Koshucode/Baala/Toolkit/Main"

|-- FILE  /package 'calculator  /code 115  /comment 109  /file 'KoshuMain.hs  /dir "calculator/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'content  /code 16  /comment 5  /file 'Cop.hs  /dir "content/Koshucode/Baala/Op"
|-- FILE  /package 'content  /code 78  /comment 39  /file 'Arith.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 32  /comment 40  /file 'Coxhand.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 292  /comment 128  /file 'List.hs  /dir "content/Koshucode/Baala/Op/Cop"

|-- FILE  /package 'content  /code 52  /comment 36  /file 'Logic.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 72  /comment 50  /file 'Misc.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 50  /comment 34  /file 'Order.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 31  /comment 27  /file 'Time.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 40  /comment 27  /file 'Type.hs  /dir "content/Koshucode/Baala/Op/Cop"

|-- FILE  /package 'content  /code 16  /comment 6  /file 'Cox.hs  /dir "content/Koshucode/Baala/Op"
|-- FILE  /package 'content  /code 132  /comment 35  /file 'Accessor.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 174  /comment 85  /file 'Calc.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 53  /comment 31  /file 'Empty.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 53  /comment 41  /file 'Filter.hs  /dir "content/Koshucode/Baala/Op/Cox"

|-- FILE  /package 'content  /code 120  /comment 61  /file 'Gadget.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 81  /comment 46  /file 'Get.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 104  /comment 72  /file 'Range.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 81  /comment 23  /file 'Global.hs  /dir "content/Koshucode/Baala/Op"
|-- FILE  /package 'content  /code 11  /comment 11  /file 'Quoter.hs  /dir "content/Koshucode/Baala/Op"
*** 75

|-- FILE  /package 'content  /code 180  /comment 45  /file 'Vanilla.hs  /dir "content/Koshucode/Baala/Type"
|-- FILE  /package 'core  /code 15  /comment 72  /file 'Core.hs  /dir "core/Koshucode/Baala"
|-- FILE  /package 'core  /code 10  /comment 6  /file 'Assert.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 33  /comment 30  /file 'Assert.hs  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 36  /comment 25  /file 'Dataset.hs  /dir "core/Koshucode/Baala/Core/Assert"

|-- FILE  /package 'core  /code 53  /comment 29  /file 'RelTable.hs  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 137  /comment 45  /file 'Run.hs  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 16  /comment 5  /file 'Content.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 167  /comment 45  /file 'Build.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 181  /comment 98  /file 'Class.hs  /dir "core/Koshucode/Baala/Core/Content"

|-- FILE  /package 'core  /code 67  /comment 55  /file 'Cop.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 105  /comment 85  /file 'Cox.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 157  /comment 158  /file 'Literal.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 154  /comment 48  /file 'Run.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 173  /comment 75  /file 'Tree.hs  /dir "core/Koshucode/Baala/Core/Content"

|-- FILE  /package 'core  /code 16  /comment 3  /file 'Lexmap.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 71  /comment 46  /file 'Attr.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 91  /comment 37  /file 'AttrEd.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 78  /comment 37  /file 'AttrPos.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 106  /comment 81  /file 'Construct.hs  /dir "core/Koshucode/Baala/Core/Lexmap"

|-- FILE  /package 'core  /code 54  /comment 26  /file 'Lexmap.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 38  /comment 26  /file 'LexmapTrees.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 37  /comment 15  /file 'Slot.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 279  /comment 131  /file 'Message.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 8  /comment 5  /file 'Relkit.hs  /dir "core/Koshucode/Baala/Core"
*** 100

|-- FILE  /package 'core  /code 63  /comment 46  /file 'Construct.hs  /dir "core/Koshucode/Baala/Core/Relkit"
|-- FILE  /package 'core  /code 59  /comment 38  /file 'Relkit.hs  /dir "core/Koshucode/Baala/Core/Relkit"
|-- FILE  /package 'core  /code 89  /comment 26  /file 'Run.hs  /dir "core/Koshucode/Baala/Core/Relkit"
|-- FILE  /package 'core  /code 14  /comment 55  /file 'Relmap.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 56  /comment 77  /file 'Construct.hs  /dir "core/Koshucode/Baala/Core/Relmap"

|-- FILE  /package 'core  /code 98  /comment 43  /file 'Global.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 62  /comment 20  /file 'Option.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 92  /comment 52  /file 'Relmap.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 31  /comment 26  /file 'Rop.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 95  /comment 20  /file 'Specialize.hs  /dir "core/Koshucode/Baala/Core/Relmap"

|-- FILE  /package 'core  /code 15  /comment 29  /file 'Resource.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 169  /comment 114  /file 'Clause.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 115  /comment 51  /file 'Include.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 52  /comment 21  /file 'Quoter.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 90  /comment 30  /file 'Read.hs  /dir "core/Koshucode/Baala/Core/Resource"

|-- FILE  /package 'core  /code 87  /comment 37  /file 'Resource.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 87  /comment 16  /file 'Run.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'operator  /code 29  /comment 6  /file 'Op.hs  /dir "operator/Koshucode/Baala"
|-- FILE  /package 'operator  /code 10  /comment 6  /file 'Builtin.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 49  /comment 13  /file 'Define.hs  /dir "operator/Koshucode/Baala/Op/Builtin"

|-- FILE  /package 'operator  /code 116  /comment 74  /file 'Get.hs  /dir "operator/Koshucode/Baala/Op/Builtin"
|-- FILE  /package 'operator  /code 22  /comment 39  /file 'Rop.hs  /dir "operator/Koshucode/Baala/Op/Builtin"
|-- FILE  /package 'operator  /code 40  /comment 26  /file 'Term.hs  /dir "operator/Koshucode/Baala/Op/Builtin"
|-- FILE  /package 'operator  /code 81  /comment 49  /file 'Check.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 93  /comment 48  /file 'Control.hs  /dir "operator/Koshucode/Baala/Op"
*** 125

|-- FILE  /package 'operator  /code 49  /comment 23  /file 'DepRank.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 176  /comment 100  /file 'Gadget.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 8  /comment 26  /file 'Lattice.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 92  /comment 53  /file 'Restrict.hs  /dir "operator/Koshucode/Baala/Op/Lattice"
|-- FILE  /package 'operator  /code 18  /comment 21  /file 'Rop.hs  /dir "operator/Koshucode/Baala/Op/Lattice"

|-- FILE  /package 'operator  /code 71  /comment 100  /file 'Tropashko.hs  /dir "operator/Koshucode/Baala/Op/Lattice"
|-- FILE  /package 'operator  /code 70  /comment 43  /file 'Message.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 171  /comment 84  /file 'Meta.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 10  /comment 6  /file 'Nest.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 92  /comment 86  /file 'Confl.hs  /dir "operator/Koshucode/Baala/Op/Nest"

|-- FILE  /package 'operator  /code 58  /comment 59  /file 'Deriv.hs  /dir "operator/Koshucode/Baala/Op/Nest"
|-- FILE  /package 'operator  /code 64  /comment 47  /file 'Flow.hs  /dir "operator/Koshucode/Baala/Op/Nest"
|-- FILE  /package 'operator  /code 25  /comment 49  /file 'Rop.hs  /dir "operator/Koshucode/Baala/Op/Nest"
|-- FILE  /package 'operator  /code 197  /comment 107  /file 'Peripheral.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 64  /comment 36  /file 'Resource.hs  /dir "operator/Koshucode/Baala/Op"

|-- FILE  /package 'operator  /code 48  /comment 73  /file 'Source.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 140  /comment 83  /file 'Term.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 76  /comment 53  /file 'TermGadget.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'toolkit  /code 67  /comment 46  /file 'Change.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'toolkit  /code 27  /comment 12  /file 'Input.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Library"

|-- FILE  /package 'toolkit  /code 33  /comment 17  /file 'RDF.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'toolkit  /code 73  /comment 38  /file 'KoshuChange.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'toolkit  /code 109  /comment 44  /file 'KoshuRdf.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'toolkit  /code 171  /comment 64  /file 'KoshuSyntax.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Main"

*** 149 judges

|-- FILE-TOTAL  /package 'base  /code 4 060  /comment 2 560
|-- FILE-TOTAL  /package 'calculator  /code 328  /comment 230
|-- FILE-TOTAL  /package 'content  /code 1 668  /comment 842
|-- FILE-TOTAL  /package 'core  /code 3 356  /comment 1 884
|-- FILE-TOTAL  /package 'operator  /code 1 869  /comment 1 310

|-- FILE-TOTAL  /package 'toolkit  /code 480  /comment 221

*** 6 judges

|-- FILE-LONG  /rank 1  /package 'content  /code 292  /file 'List.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE-LONG  /rank 2  /package 'core  /code 279  /file 'Message.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE-LONG  /rank 3  /package 'base  /code 261  /file 'TokenLine.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE-LONG  /rank 4  /package 'base  /code 253  /file 'Token.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE-LONG  /rank 5  /package 'base  /code 239  /file 'Result.hs  /dir "base/Koshucode/Baala/Base/Data"

|-- FILE-LONG  /rank 6  /package 'operator  /code 197  /file 'Peripheral.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE-LONG  /rank 7  /package 'base  /code 193  /file 'Para.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE-LONG  /rank 8  /package 'base  /code 185  /file 'Clock.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE-LONG  /rank 9  /package 'core  /code 181  /file 'Class.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE-LONG  /rank 10  /package 'content  /code 180  /file 'Vanilla.hs  /dir "content/Koshucode/Baala/Type"

*** 10 judges

**
**  SUMMARY
**      29 judges on DIR
**       6 judges on DIR-TOTAL
**     149 judges on FILE
**      10 judges on FILE-LONG
**       6 judges on FILE-TOTAL
**     200 judges in total
**

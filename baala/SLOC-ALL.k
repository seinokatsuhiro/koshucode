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

|-- DIR  /package 'base  /code 14  /comment 4  /dir "base/Koshucode/Baala"
|-- DIR  /package 'base  /code 165  /comment 82  /dir "base/Koshucode/Baala/Base"
|-- DIR  /package 'base  /code 98  /comment 56  /dir "base/Koshucode/Baala/Base/Abort"
|-- DIR  /package 'base  /code 1 347  /comment 779  /dir "base/Koshucode/Baala/Base/Data"
|-- DIR  /package 'base  /code 512  /comment 474  /dir "base/Koshucode/Baala/Base/Prelude"

|-- DIR  /package 'base  /code 264  /comment 189  /dir "base/Koshucode/Baala/Base/Syntax"
|-- DIR  /package 'base  /code 518  /comment 290  /dir "base/Koshucode/Baala/Base/Text"
|-- DIR  /package 'base  /code 704  /comment 505  /dir "base/Koshucode/Baala/Base/Token"
|-- DIR  /package 'calculator  /code 138  /comment 111  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- DIR  /package 'calculator  /code 104  /comment 107  /dir "calculator/Koshucode/Baala/Toolkit/Main"

|-- DIR  /package 'content  /code 118  /comment 45  /dir "content/Koshucode/Baala/Op"
|-- DIR  /package 'content  /code 518  /comment 328  /dir "content/Koshucode/Baala/Op/Cop"
|-- DIR  /package 'content  /code 719  /comment 376  /dir "content/Koshucode/Baala/Op/Cox"
|-- DIR  /package 'content  /code 170  /comment 42  /dir "content/Koshucode/Baala/Type"
|-- DIR  /package 'core  /code 13  /comment 72  /dir "core/Koshucode/Baala"

|-- DIR  /package 'core  /code 326  /comment 224  /dir "core/Koshucode/Baala/Core"
|-- DIR  /package 'core  /code 242  /comment 126  /dir "core/Koshucode/Baala/Core/Assert"
|-- DIR  /package 'core  /code 969  /comment 522  /dir "core/Koshucode/Baala/Core/Content"
|-- DIR  /package 'core  /code 446  /comment 252  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- DIR  /package 'core  /code 559  /comment 284  /dir "core/Koshucode/Baala/Core/Relmap"

|-- DIR  /package 'core  /code 503  /comment 255  /dir "core/Koshucode/Baala/Core/Resource"
|-- DIR  /package 'operator  /code 29  /comment 6  /dir "operator/Koshucode/Baala"
|-- DIR  /package 'operator  /code 976  /comment 658  /dir "operator/Koshucode/Baala/Op"
|-- DIR  /package 'operator  /code 216  /comment 147  /dir "operator/Koshucode/Baala/Op/Builtin"
|-- DIR  /package 'operator  /code 159  /comment 165  /dir "operator/Koshucode/Baala/Op/Lattice"
*** 25

|-- DIR  /package 'operator  /code 248  /comment 244  /dir "operator/Koshucode/Baala/Op/Nest"
|-- DIR  /package 'toolkit  /code 127  /comment 75  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- DIR  /package 'toolkit  /code 355  /comment 147  /dir "toolkit/Koshucode/Baala/Toolkit/Main"

*** 28 judges

|-- DIR-TOTAL  /package 'base  /code 3 622  /comment 2 379
|-- DIR-TOTAL  /package 'calculator  /code 242  /comment 218
|-- DIR-TOTAL  /package 'content  /code 1 525  /comment 791
|-- DIR-TOTAL  /package 'core  /code 3 058  /comment 1 735
|-- DIR-TOTAL  /package 'operator  /code 1 628  /comment 1 220

|-- DIR-TOTAL  /package 'toolkit  /code 482  /comment 222

*** 6 judges

|-- FILE  /package 'base  /code 14  /comment 4  /file 'Base.hs  /dir "base/Koshucode/Baala"
|-- FILE  /package 'base  /code 8  /comment 6  /file 'Abort.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 22  /comment 12  /file 'Abortable.hs  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 26  /comment 22  /file 'Reason.hs  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 50  /comment 22  /file 'Report.hs  /dir "base/Koshucode/Baala/Base/Abort"

|-- FILE  /package 'base  /code 27  /comment 5  /file 'Data.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 185  /comment 84  /file 'Clock.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 73  /comment 30  /file 'Date.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 169  /comment 66  /file 'Decimal.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 85  /comment 137  /file 'Head.hs  /dir "base/Koshucode/Baala/Base/Data"

|-- FILE  /package 'base  /code 35  /comment 12  /file 'Interp.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 112  /comment 64  /file 'Judge.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 30  /comment 14  /file 'Mono.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 117  /comment 45  /file 'Output.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 182  /comment 71  /file 'Para.hs  /dir "base/Koshucode/Baala/Base/Data"

|-- FILE  /package 'base  /code 45  /comment 58  /file 'Rel.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 179  /comment 107  /file 'Time.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 135  /comment 91  /file 'Type.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE  /package 'base  /code 72  /comment 53  /file 'Message.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 16  /comment 3  /file 'Prelude.hs  /dir "base/Koshucode/Baala/Base"

|-- FILE  /package 'base  /code 123  /comment 144  /file 'Assoc.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 32  /comment 20  /file 'Class.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 59  /comment 34  /file 'Import.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 120  /comment 123  /file 'List.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 75  /comment 34  /file 'Order.hs  /dir "base/Koshucode/Baala/Base/Prelude"
*** 25

|-- FILE  /package 'base  /code 35  /comment 12  /file 'Pair.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 68  /comment 107  /file 'Snip.hs  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 10  /comment 6  /file 'Syntax.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 82  /comment 53  /file 'Code.hs  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 68  /comment 50  /file 'Infix.hs  /dir "base/Koshucode/Baala/Base/Syntax"

|-- FILE  /package 'base  /code 20  /comment 14  /file 'Line.hs  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 94  /comment 72  /file 'Tree.hs  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 18  /comment 3  /file 'Text.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 49  /comment 27  /file 'CodeName.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 58  /comment 33  /file 'CodePt.hs  /dir "base/Koshucode/Baala/Base/Text"

|-- FILE  /package 'base  /code 36  /comment 24  /file 'Comment.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 73  /comment 13  /file 'Http.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 124  /comment 57  /file 'TextTable.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 65  /comment 14  /file 'Unicode.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 47  /comment 41  /file 'Utility.hs  /dir "base/Koshucode/Baala/Base/Text"

|-- FILE  /package 'base  /code 66  /comment 81  /file 'Write.hs  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 14  /comment 6  /file 'Token.hs  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 53  /comment 72  /file 'AngleText.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'base  /code 57  /comment 32  /file 'Short.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'base  /code 216  /comment 104  /file 'Token.hs  /dir "base/Koshucode/Baala/Base/Token"

|-- FILE  /package 'base  /code 36  /comment 14  /file 'TokenClause.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'base  /code 232  /comment 188  /file 'TokenLine.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'base  /code 110  /comment 95  /file 'TokenTree.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE  /package 'calculator  /code 53  /comment 49  /file 'Element.hs  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 44  /comment 16  /file 'Exit.hs  /dir "calculator/Koshucode/Baala/Toolkit/Library"
*** 50

|-- FILE  /package 'calculator  /code 31  /comment 38  /file 'Run.hs  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 10  /comment 8  /file 'Version.hs  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 104  /comment 107  /file 'KoshuMain.hs  /dir "calculator/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'content  /code 14  /comment 5  /file 'Cop.hs  /dir "content/Koshucode/Baala/Op"
|-- FILE  /package 'content  /code 78  /comment 39  /file 'Arith.hs  /dir "content/Koshucode/Baala/Op/Cop"

|-- FILE  /package 'content  /code 32  /comment 40  /file 'Coxhand.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 203  /comment 102  /file 'List.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 52  /comment 36  /file 'Logic.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 72  /comment 50  /file 'Misc.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 50  /comment 34  /file 'Order.hs  /dir "content/Koshucode/Baala/Op/Cop"

|-- FILE  /package 'content  /code 31  /comment 27  /file 'Time.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE  /package 'content  /code 16  /comment 6  /file 'Cox.hs  /dir "content/Koshucode/Baala/Op"
|-- FILE  /package 'content  /code 132  /comment 35  /file 'Accessor.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 174  /comment 85  /file 'Calc.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 55  /comment 36  /file 'Empty.hs  /dir "content/Koshucode/Baala/Op/Cox"

|-- FILE  /package 'content  /code 53  /comment 41  /file 'Filter.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 120  /comment 61  /file 'Gadget.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 81  /comment 46  /file 'Get.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 104  /comment 72  /file 'Range.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE  /package 'content  /code 77  /comment 23  /file 'Global.hs  /dir "content/Koshucode/Baala/Op"

|-- FILE  /package 'content  /code 11  /comment 11  /file 'Quoter.hs  /dir "content/Koshucode/Baala/Op"
|-- FILE  /package 'content  /code 170  /comment 42  /file 'Vanilla.hs  /dir "content/Koshucode/Baala/Type"
|-- FILE  /package 'core  /code 13  /comment 72  /file 'Core.hs  /dir "core/Koshucode/Baala"
|-- FILE  /package 'core  /code 10  /comment 6  /file 'Assert.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 41  /comment 32  /file 'Assert.hs  /dir "core/Koshucode/Baala/Core/Assert"
*** 75

|-- FILE  /package 'core  /code 36  /comment 25  /file 'Dataset.hs  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 53  /comment 29  /file 'RelTable.hs  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 112  /comment 40  /file 'Run.hs  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 16  /comment 5  /file 'Content.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 164  /comment 45  /file 'Build.hs  /dir "core/Koshucode/Baala/Core/Content"

|-- FILE  /package 'core  /code 165  /comment 91  /file 'Class.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 67  /comment 55  /file 'Cop.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 105  /comment 85  /file 'Cox.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 145  /comment 124  /file 'Literal.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 154  /comment 48  /file 'Run.hs  /dir "core/Koshucode/Baala/Core/Content"

|-- FILE  /package 'core  /code 169  /comment 74  /file 'Tree.hs  /dir "core/Koshucode/Baala/Core/Content"
|-- FILE  /package 'core  /code 14  /comment 3  /file 'Lexmap.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 71  /comment 46  /file 'Attr.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 93  /comment 36  /file 'AttrPos.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 73  /comment 32  /file 'Attrmap.hs  /dir "core/Koshucode/Baala/Core/Lexmap"

|-- FILE  /package 'core  /code 116  /comment 97  /file 'Construct.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 56  /comment 26  /file 'Lexmap.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 37  /comment 15  /file 'Slot.hs  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 255  /comment 126  /file 'Message.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 16  /comment 55  /file 'Relmap.hs  /dir "core/Koshucode/Baala/Core"

|-- FILE  /package 'core  /code 71  /comment 75  /file 'Construct.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 91  /comment 42  /file 'Global.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 116  /comment 56  /file 'Relkit.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 77  /comment 41  /file 'Relmap.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 30  /comment 23  /file 'Rop.hs  /dir "core/Koshucode/Baala/Core/Relmap"
*** 100

|-- FILE  /package 'core  /code 89  /comment 28  /file 'Run.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 85  /comment 19  /file 'Specialize.hs  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 15  /comment 29  /file 'Resource.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 158  /comment 114  /file 'Clause.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 87  /comment 48  /file 'Include.hs  /dir "core/Koshucode/Baala/Core/Resource"

|-- FILE  /package 'core  /code 52  /comment 21  /file 'Quoter.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 74  /comment 25  /file 'Read.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 69  /comment 34  /file 'Resource.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 63  /comment 13  /file 'Run.hs  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'operator  /code 29  /comment 6  /file 'Op.hs  /dir "operator/Koshucode/Baala"

|-- FILE  /package 'operator  /code 10  /comment 6  /file 'Builtin.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 46  /comment 12  /file 'Define.hs  /dir "operator/Koshucode/Baala/Op/Builtin"
|-- FILE  /package 'operator  /code 116  /comment 74  /file 'Get.hs  /dir "operator/Koshucode/Baala/Op/Builtin"
|-- FILE  /package 'operator  /code 22  /comment 39  /file 'Rop.hs  /dir "operator/Koshucode/Baala/Op/Builtin"
|-- FILE  /package 'operator  /code 32  /comment 22  /file 'Term.hs  /dir "operator/Koshucode/Baala/Op/Builtin"

|-- FILE  /package 'operator  /code 85  /comment 50  /file 'Check.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 93  /comment 48  /file 'Control.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 49  /comment 23  /file 'DepRank.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 62  /comment 61  /file 'Gadget.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 8  /comment 26  /file 'Lattice.hs  /dir "operator/Koshucode/Baala/Op"

|-- FILE  /package 'operator  /code 64  /comment 38  /file 'Restrict.hs  /dir "operator/Koshucode/Baala/Op/Lattice"
|-- FILE  /package 'operator  /code 16  /comment 21  /file 'Rop.hs  /dir "operator/Koshucode/Baala/Op/Lattice"
|-- FILE  /package 'operator  /code 79  /comment 106  /file 'Tropashko.hs  /dir "operator/Koshucode/Baala/Op/Lattice"
|-- FILE  /package 'operator  /code 70  /comment 43  /file 'Message.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 171  /comment 84  /file 'Meta.hs  /dir "operator/Koshucode/Baala/Op"
*** 125

|-- FILE  /package 'operator  /code 10  /comment 6  /file 'Nest.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 101  /comment 89  /file 'Confl.hs  /dir "operator/Koshucode/Baala/Op/Nest"
|-- FILE  /package 'operator  /code 58  /comment 59  /file 'Deriv.hs  /dir "operator/Koshucode/Baala/Op/Nest"
|-- FILE  /package 'operator  /code 65  /comment 47  /file 'Flow.hs  /dir "operator/Koshucode/Baala/Op/Nest"
|-- FILE  /package 'operator  /code 24  /comment 49  /file 'Rop.hs  /dir "operator/Koshucode/Baala/Op/Nest"

|-- FILE  /package 'operator  /code 126  /comment 83  /file 'Peripheral.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 64  /comment 36  /file 'Resource.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 48  /comment 73  /file 'Source.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 103  /comment 66  /file 'Term.hs  /dir "operator/Koshucode/Baala/Op"
|-- FILE  /package 'operator  /code 77  /comment 53  /file 'TermGadget.hs  /dir "operator/Koshucode/Baala/Op"

|-- FILE  /package 'toolkit  /code 67  /comment 46  /file 'Change.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'toolkit  /code 27  /comment 12  /file 'Input.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'toolkit  /code 33  /comment 17  /file 'RDF.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'toolkit  /code 73  /comment 38  /file 'KoshuChange.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'toolkit  /code 109  /comment 44  /file 'KoshuRdf.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Main"

|-- FILE  /package 'toolkit  /code 173  /comment 65  /file 'KoshuSyntax.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Main"

*** 141 judges

|-- FILE-TOTAL  /package 'base  /code 3 622  /comment 2 379
|-- FILE-TOTAL  /package 'calculator  /code 242  /comment 218
|-- FILE-TOTAL  /package 'content  /code 1 525  /comment 791
|-- FILE-TOTAL  /package 'core  /code 3 058  /comment 1 735
|-- FILE-TOTAL  /package 'operator  /code 1 628  /comment 1 220

|-- FILE-TOTAL  /package 'toolkit  /code 482  /comment 222

*** 6 judges

|-- FILE-LONG  /rank 1  /code 255  /package 'core  /file 'Message.hs  /dir "core/Koshucode/Baala/Core"
|-- FILE-LONG  /rank 2  /code 232  /package 'base  /file 'TokenLine.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE-LONG  /rank 3  /code 216  /package 'base  /file 'Token.hs  /dir "base/Koshucode/Baala/Base/Token"
|-- FILE-LONG  /rank 4  /code 203  /package 'content  /file 'List.hs  /dir "content/Koshucode/Baala/Op/Cop"
|-- FILE-LONG  /rank 5  /code 185  /package 'base  /file 'Clock.hs  /dir "base/Koshucode/Baala/Base/Data"

|-- FILE-LONG  /rank 6  /code 182  /package 'base  /file 'Para.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE-LONG  /rank 7  /code 179  /package 'base  /file 'Time.hs  /dir "base/Koshucode/Baala/Base/Data"
|-- FILE-LONG  /rank 8  /code 174  /package 'content  /file 'Calc.hs  /dir "content/Koshucode/Baala/Op/Cox"
|-- FILE-LONG  /rank 9  /code 173  /package 'toolkit  /file 'KoshuSyntax.hs  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- FILE-LONG  /rank 10  /code 171  /package 'operator  /file 'Meta.hs  /dir "operator/Koshucode/Baala/Op"

*** 10 judges

**
**  SUMMARY
**      28 judges on DIR
**       6 judges on DIR-TOTAL
**     141 judges on FILE
**      10 judges on FILE-LONG
**       6 judges on FILE-TOTAL
**     191 judges in total
**

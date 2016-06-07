** -*- koshu -*-
**
**  INPUT
**    sloc-all-calc.k
**    base/data/SLOC.k
**    calculator/data/SLOC.k
**    cop/data/SLOC.k
**    core/data/SLOC.k
**    data/data/SLOC.k
**    rop-cox/data/SLOC.k
**    rop-flat/data/SLOC.k
**    rop-nested/data/SLOC.k
**    syntax/data/SLOC.k
**    toolkit/data/SLOC.k
**    writer/data/SLOC.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR  /package 'base  /code 12  /comment 4  /dir "base/Koshucode/Baala"
|-- DIR  /package 'base  /code 70  /comment 25  /dir "base/Koshucode/Baala/Base"
|-- DIR  /package 'base  /code 130  /comment 98  /dir "base/Koshucode/Baala/Base/Abort"
|-- DIR  /package 'base  /code 537  /comment 600  /dir "base/Koshucode/Baala/Base/List"
|-- DIR  /package 'base  /code 200  /comment 167  /dir "base/Koshucode/Baala/Base/Prelude"

|-- DIR  /package 'base  /code 338  /comment 226  /dir "base/Koshucode/Baala/Base/Syntax"
|-- DIR  /package 'base  /code 875  /comment 568  /dir "base/Koshucode/Baala/Base/Text"
|-- DIR  /package 'calculator  /code 235  /comment 182  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- DIR  /package 'calculator  /code 229  /comment 125  /dir "calculator/Koshucode/Baala/Toolkit/Main"
|-- DIR  /package 'cop  /code 18  /comment 6  /dir "cop/Koshucode/Baala"

|-- DIR  /package 'cop  /code 1 006  /comment 653  /dir "cop/Koshucode/Baala/Cop"
|-- DIR  /package 'core  /code 13  /comment 72  /dir "core/Koshucode/Baala"
|-- DIR  /package 'core  /code 75  /comment 103  /dir "core/Koshucode/Baala/Core"
|-- DIR  /package 'core  /code 289  /comment 138  /dir "core/Koshucode/Baala/Core/Assert"
|-- DIR  /package 'core  /code 215  /comment 150  /dir "core/Koshucode/Baala/Core/Lexmap"

|-- DIR  /package 'core  /code 229  /comment 123  /dir "core/Koshucode/Baala/Core/Relkit"
|-- DIR  /package 'core  /code 619  /comment 315  /dir "core/Koshucode/Baala/Core/Relmap"
|-- DIR  /package 'core  /code 700  /comment 393  /dir "core/Koshucode/Baala/Core/Resource"
|-- DIR  /package 'data  /code 9  /comment 3  /dir "data/Koshucode/Baala"
|-- DIR  /package 'data  /code 52  /comment 17  /dir "data/Koshucode/Baala/Data"

|-- DIR  /package 'data  /code 586  /comment 276  /dir "data/Koshucode/Baala/Data/Church"
|-- DIR  /package 'data  /code 861  /comment 508  /dir "data/Koshucode/Baala/Data/Content"
|-- DIR  /package 'data  /code 337  /comment 205  /dir "data/Koshucode/Baala/Data/Type"
|-- DIR  /package 'data  /code 479  /comment 390  /dir "data/Koshucode/Baala/Data/Type/Decimal"
|-- DIR  /package 'data  /code 250  /comment 246  /dir "data/Koshucode/Baala/Data/Type/Rel"
*** 25

|-- DIR  /package 'data  /code 439  /comment 235  /dir "data/Koshucode/Baala/Data/Type/Time"
|-- DIR  /package 'rop-cox  /code 18  /comment 6  /dir "rop-cox/Koshucode/Baala/Rop"
|-- DIR  /package 'rop-cox  /code 926  /comment 468  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- DIR  /package 'rop-flat  /code 38  /comment 12  /dir "rop-flat/Koshucode/Baala/Rop"
|-- DIR  /package 'rop-flat  /code 229  /comment 168  /dir "rop-flat/Koshucode/Baala/Rop/Base"

|-- DIR  /package 'rop-flat  /code 1 327  /comment 798  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- DIR  /package 'rop-flat  /code 214  /comment 203  /dir "rop-flat/Koshucode/Baala/Rop/Flat/Lattice"
|-- DIR  /package 'rop-nested  /code 10  /comment 6  /dir "rop-nested/Koshucode/Baala/Rop"
|-- DIR  /package 'rop-nested  /code 266  /comment 247  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- DIR  /package 'syntax  /code 12  /comment 4  /dir "syntax/Koshucode/Baala"

|-- DIR  /package 'syntax  /code 58  /comment 28  /dir "syntax/Koshucode/Baala/Syntax"
|-- DIR  /package 'syntax  /code 361  /comment 245  /dir "syntax/Koshucode/Baala/Syntax/Attr"
|-- DIR  /package 'syntax  /code 345  /comment 276  /dir "syntax/Koshucode/Baala/Syntax/Para"
|-- DIR  /package 'syntax  /code 272  /comment 258  /dir "syntax/Koshucode/Baala/Syntax/Symbol"
|-- DIR  /package 'syntax  /code 157  /comment 144  /dir "syntax/Koshucode/Baala/Syntax/TTree"

|-- DIR  /package 'syntax  /code 572  /comment 419  /dir "syntax/Koshucode/Baala/Syntax/Token"
|-- DIR  /package 'toolkit  /code 126  /comment 74  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- DIR  /package 'toolkit  /code 357  /comment 146  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- DIR  /package 'writer  /code 12  /comment 3  /dir "writer/Koshucode/Baala"
|-- DIR  /package 'writer  /code 340  /comment 105  /dir "writer/Koshucode/Baala/Writer"

*** 45 judges

|-- DIR-TOTAL  /package 'base  /code 2 162  /comment 1 688
|-- DIR-TOTAL  /package 'calculator  /code 464  /comment 307
|-- DIR-TOTAL  /package 'cop  /code 1 024  /comment 659
|-- DIR-TOTAL  /package 'core  /code 2 140  /comment 1 294
|-- DIR-TOTAL  /package 'data  /code 3 013  /comment 1 880

|-- DIR-TOTAL  /package 'rop-cox  /code 944  /comment 474
|-- DIR-TOTAL  /package 'rop-flat  /code 1 808  /comment 1 181
|-- DIR-TOTAL  /package 'rop-nested  /code 276  /comment 253
|-- DIR-TOTAL  /package 'syntax  /code 1 777  /comment 1 374
|-- DIR-TOTAL  /package 'toolkit  /code 483  /comment 220

|-- DIR-TOTAL  /package 'writer  /code 352  /comment 108

*** 11 judges

|-- FILE  /package 'base  /code 12  /comment 4  /file "Base.hs"  /dir "base/Koshucode/Baala"
|-- FILE  /package 'base  /code 8  /comment 6  /file "Abort.hs"  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 22  /comment 12  /file "Abortable.hs"  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 12  /comment 10  /file "Message.hs"  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 46  /comment 40  /file "Reason.hs"  /dir "base/Koshucode/Baala/Base/Abort"

|-- FILE  /package 'base  /code 50  /comment 36  /file "Report.hs"  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 14  /comment 3  /file "List.hs"  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 142  /comment 156  /file "Assoc.hs"  /dir "base/Koshucode/Baala/Base/List"
|-- FILE  /package 'base  /code 54  /comment 59  /file "Dots.hs"  /dir "base/Koshucode/Baala/Base/List"
|-- FILE  /package 'base  /code 126  /comment 134  /file "List.hs"  /dir "base/Koshucode/Baala/Base/List"

|-- FILE  /package 'base  /code 78  /comment 40  /file "Order.hs"  /dir "base/Koshucode/Baala/Base/List"
|-- FILE  /package 'base  /code 73  /comment 119  /file "Snip.hs"  /dir "base/Koshucode/Baala/Base/List"
|-- FILE  /package 'base  /code 64  /comment 92  /file "Split.hs"  /dir "base/Koshucode/Baala/Base/List"
|-- FILE  /package 'base  /code 6  /comment 4  /file "Message.hs"  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 10  /comment 3  /file "Prelude.hs"  /dir "base/Koshucode/Baala/Base"

|-- FILE  /package 'base  /code 38  /comment 66  /file "Case.hs"  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 33  /comment 20  /file "Class.hs"  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 83  /comment 56  /file "Import.hs"  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 46  /comment 25  /file "Pair.hs"  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 10  /comment 6  /file "Syntax.hs"  /dir "base/Koshucode/Baala/Base"

|-- FILE  /package 'base  /code 25  /comment 20  /file "Clause.hs"  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 68  /comment 50  /file "Infix.hs"  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 127  /comment 76  /file "Line.hs"  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 15  /comment 10  /file "Message.hs"  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 103  /comment 70  /file "Tree.hs"  /dir "base/Koshucode/Baala/Base/Syntax"
*** 25

|-- FILE  /package 'base  /code 22  /comment 3  /file "Text.hs"  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 57  /comment 33  /file "CodePt.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 36  /comment 24  /file "Comment.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 81  /comment 15  /file "Http.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 80  /comment 45  /file "IOPoint.hs"  /dir "base/Koshucode/Baala/Base/Text"

|-- FILE  /package 'base  /code 36  /comment 10  /file "LineBreak.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 201  /comment 204  /file "MixText.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 125  /comment 57  /file "TextTable.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 103  /comment 40  /file "Unicode.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 59  /comment 45  /file "Utility.hs"  /dir "base/Koshucode/Baala/Base/Text"

|-- FILE  /package 'base  /code 97  /comment 95  /file "Write.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'calculator  /code 64  /comment 49  /file "Element.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 40  /comment 15  /file "Exit.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 22  /comment 10  /file "Global.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 28  /comment 34  /file "Run.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"

|-- FILE  /package 'calculator  /code 71  /comment 66  /file "SimpleOption.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 10  /comment 8  /file "Version.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 72  /comment 14  /file "KoshuFilter.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'calculator  /code 157  /comment 111  /file "KoshuMain.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'cop  /code 18  /comment 6  /file "Cop.hs"  /dir "cop/Koshucode/Baala"

|-- FILE  /package 'cop  /code 195  /comment 88  /file "Arith.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 64  /comment 19  /file "Bundle.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 33  /comment 40  /file "Coxhand.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 196  /comment 109  /file "List.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 54  /comment 37  /file "Logic.hs"  /dir "cop/Koshucode/Baala/Cop"
*** 50

|-- FILE  /package 'cop  /code 4  /comment 4  /file "Message.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 81  /comment 55  /file "Misc.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 50  /comment 34  /file "Order.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 40  /comment 75  /file "Replace.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 219  /comment 138  /file "Text.hs"  /dir "cop/Koshucode/Baala/Cop"

|-- FILE  /package 'cop  /code 31  /comment 27  /file "Time.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 39  /comment 27  /file "Type.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'core  /code 13  /comment 72  /file "Core.hs"  /dir "core/Koshucode/Baala"
|-- FILE  /package 'core  /code 10  /comment 6  /file "Assert.hs"  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 34  /comment 30  /file "Assert.hs"  /dir "core/Koshucode/Baala/Core/Assert"

|-- FILE  /package 'core  /code 36  /comment 25  /file "Dataset.hs"  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 26  /comment 9  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 54  /comment 29  /file "RelTable.hs"  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 139  /comment 45  /file "Run.hs"  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 8  /comment 4  /file "Lexmap.hs"  /dir "core/Koshucode/Baala/Core"

|-- FILE  /package 'core  /code 110  /comment 87  /file "Construct.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 52  /comment 26  /file "Lexmap.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 34  /comment 25  /file "LexmapTrees.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 19  /comment 12  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 16  /comment 4  /file "Message.hs"  /dir "core/Koshucode/Baala/Core"

|-- FILE  /package 'core  /code 8  /comment 5  /file "Relkit.hs"  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 65  /comment 46  /file "Construct.hs"  /dir "core/Koshucode/Baala/Core/Relkit"
|-- FILE  /package 'core  /code 12  /comment 8  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Relkit"
|-- FILE  /package 'core  /code 61  /comment 43  /file "Relkit.hs"  /dir "core/Koshucode/Baala/Core/Relkit"
|-- FILE  /package 'core  /code 91  /comment 26  /file "Run.hs"  /dir "core/Koshucode/Baala/Core/Relkit"
*** 75

|-- FILE  /package 'core  /code 16  /comment 55  /file "Relmap.hs"  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 61  /comment 77  /file "Construct.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 115  /comment 52  /file "Global.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 44  /comment 16  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 64  /comment 20  /file "Option.hs"  /dir "core/Koshucode/Baala/Core/Relmap"

|-- FILE  /package 'core  /code 94  /comment 52  /file "Relmap.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 111  /comment 45  /file "Result.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 32  /comment 26  /file "Rop.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 98  /comment 27  /file "Specialize.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 17  /comment 29  /file "Resource.hs"  /dir "core/Koshucode/Baala/Core"

|-- FILE  /package 'core  /code 11  /comment 6  /file "About.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 170  /comment 114  /file "Clause.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 16  /comment 22  /file "Concrete.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 122  /comment 53  /file "Include.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 35  /comment 21  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Resource"

|-- FILE  /package 'core  /code 31  /comment 50  /file "Queue.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 100  /comment 44  /file "Read.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 102  /comment 59  /file "Resource.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 113  /comment 24  /file "Run.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'data  /code 9  /comment 3  /file "Data.hs"  /dir "data/Koshucode/Baala"

|-- FILE  /package 'data  /code 10  /comment 3  /file "Church.hs"  /dir "data/Koshucode/Baala/Data"
|-- FILE  /package 'data  /code 169  /comment 47  /file "Build.hs"  /dir "data/Koshucode/Baala/Data/Church"
|-- FILE  /package 'data  /code 68  /comment 55  /file "Cop.hs"  /dir "data/Koshucode/Baala/Data/Church"
|-- FILE  /package 'data  /code 110  /comment 86  /file "Cox.hs"  /dir "data/Koshucode/Baala/Data/Church"
|-- FILE  /package 'data  /code 76  /comment 37  /file "Message.hs"  /dir "data/Koshucode/Baala/Data/Church"
*** 100

|-- FILE  /package 'data  /code 163  /comment 51  /file "Run.hs"  /dir "data/Koshucode/Baala/Data/Church"
|-- FILE  /package 'data  /code 16  /comment 5  /file "Content.hs"  /dir "data/Koshucode/Baala/Data"
|-- FILE  /package 'data  /code 187  /comment 47  /file "BaalaC.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 67  /comment 53  /file "Complex.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 168  /comment 158  /file "Decode.hs"  /dir "data/Koshucode/Baala/Data/Content"

|-- FILE  /package 'data  /code 51  /comment 28  /file "Message.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 87  /comment 64  /file "Simple.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 28  /comment 22  /file "Singleton.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 176  /comment 75  /file "Tree.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 97  /comment 61  /file "Utility.hs"  /dir "data/Koshucode/Baala/Data/Content"

|-- FILE  /package 'data  /code 12  /comment 4  /file "Message.hs"  /dir "data/Koshucode/Baala/Data"
|-- FILE  /package 'data  /code 14  /comment 5  /file "Type.hs"  /dir "data/Koshucode/Baala/Data"
|-- FILE  /package 'data  /code 12  /comment 4  /file "Decimal.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 54  /comment 41  /file "BinaryAb.hs"  /dir "data/Koshucode/Baala/Data/Type/Decimal"
|-- FILE  /package 'data  /code 129  /comment 87  /file "Coder.hs"  /dir "data/Koshucode/Baala/Data/Type/Decimal"

|-- FILE  /package 'data  /code 84  /comment 89  /file "Decimal.hs"  /dir "data/Koshucode/Baala/Data/Type/Decimal"
|-- FILE  /package 'data  /code 133  /comment 99  /file "Fraction.hs"  /dir "data/Koshucode/Baala/Data/Type/Decimal"
|-- FILE  /package 'data  /code 79  /comment 74  /file "Rational.hs"  /dir "data/Koshucode/Baala/Data/Type/Decimal"
|-- FILE  /package 'data  /code 34  /comment 12  /file "Interp.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 117  /comment 73  /file "Judge.hs"  /dir "data/Koshucode/Baala/Data/Type"

|-- FILE  /package 'data  /code 22  /comment 15  /file "Message.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 8  /comment 5  /file "Rel.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 150  /comment 155  /file "Head.hs"  /dir "data/Koshucode/Baala/Data/Type/Rel"
|-- FILE  /package 'data  /code 30  /comment 23  /file "Mono.hs"  /dir "data/Koshucode/Baala/Data/Type/Rel"
|-- FILE  /package 'data  /code 70  /comment 68  /file "Rel.hs"  /dir "data/Koshucode/Baala/Data/Type/Rel"
*** 125

|-- FILE  /package 'data  /code 8  /comment 5  /file "Time.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 183  /comment 84  /file "Clock.hs"  /dir "data/Koshucode/Baala/Data/Type/Time"
|-- FILE  /package 'data  /code 73  /comment 43  /file "Date.hs"  /dir "data/Koshucode/Baala/Data/Type/Time"
|-- FILE  /package 'data  /code 183  /comment 108  /file "Time.hs"  /dir "data/Koshucode/Baala/Data/Type/Time"
|-- FILE  /package 'data  /code 136  /comment 91  /file "Type.hs"  /dir "data/Koshucode/Baala/Data/Type"

|-- FILE  /package 'rop-cox  /code 18  /comment 6  /file "Cox.hs"  /dir "rop-cox/Koshucode/Baala/Rop"
|-- FILE  /package 'rop-cox  /code 134  /comment 35  /file "Accessor.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 18  /comment 6  /file "Bundle.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 176  /comment 85  /file "Calc.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 71  /comment 47  /file "Empty.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"

|-- FILE  /package 'rop-cox  /code 54  /comment 41  /file "Filter.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 178  /comment 85  /file "Gadget.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 102  /comment 47  /file "GeoDatumJp.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 83  /comment 46  /file "Get.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 4  /comment 4  /file "Message.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"

|-- FILE  /package 'rop-cox  /code 106  /comment 72  /file "Range.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-flat  /code 10  /comment 6  /file "Base.hs"  /dir "rop-flat/Koshucode/Baala/Rop"
|-- FILE  /package 'rop-flat  /code 18  /comment 12  /file "Define.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"
|-- FILE  /package 'rop-flat  /code 123  /comment 75  /file "Get.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"
|-- FILE  /package 'rop-flat  /code 16  /comment 10  /file "Message.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"

|-- FILE  /package 'rop-flat  /code 24  /comment 42  /file "Rop.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"
|-- FILE  /package 'rop-flat  /code 48  /comment 29  /file "Term.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"
|-- FILE  /package 'rop-flat  /code 28  /comment 6  /file "Flat.hs"  /dir "rop-flat/Koshucode/Baala/Rop"
|-- FILE  /package 'rop-flat  /code 32  /comment 5  /file "Bundle.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 82  /comment 49  /file "Check.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
*** 150

|-- FILE  /package 'rop-flat  /code 99  /comment 49  /file "Control.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 176  /comment 96  /file "Elem.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 184  /comment 103  /file "Gadget.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 8  /comment 26  /file "Lattice.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 102  /comment 72  /file "Restrict.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat/Lattice"

|-- FILE  /package 'rop-flat  /code 18  /comment 21  /file "Rop.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat/Lattice"
|-- FILE  /package 'rop-flat  /code 94  /comment 110  /file "Tropashko.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat/Lattice"
|-- FILE  /package 'rop-flat  /code 66  /comment 37  /file "Message.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 173  /comment 84  /file "Meta.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 64  /comment 32  /file "Order.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"

|-- FILE  /package 'rop-flat  /code 91  /comment 54  /file "Peripheral.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 56  /comment 35  /file "PoScale.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 66  /comment 36  /file "Resource.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 50  /comment 73  /file "Source.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 103  /comment 66  /file "Term.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"

|-- FILE  /package 'rop-flat  /code 77  /comment 53  /file "TermGadget.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-nested  /code 10  /comment 6  /file "Nest.hs"  /dir "rop-nested/Koshucode/Baala/Rop"
|-- FILE  /package 'rop-nested  /code 99  /comment 86  /file "Confl.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- FILE  /package 'rop-nested  /code 60  /comment 59  /file "Deriv.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- FILE  /package 'rop-nested  /code 66  /comment 47  /file "Flow.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"

|-- FILE  /package 'rop-nested  /code 12  /comment 6  /file "Message.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- FILE  /package 'rop-nested  /code 29  /comment 49  /file "Rop.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- FILE  /package 'syntax  /code 12  /comment 4  /file "Syntax.hs"  /dir "syntax/Koshucode/Baala"
|-- FILE  /package 'syntax  /code 12  /comment 4  /file "Attr.hs"  /dir "syntax/Koshucode/Baala/Syntax"
|-- FILE  /package 'syntax  /code 62  /comment 47  /file "Attr.hs"  /dir "syntax/Koshucode/Baala/Syntax/Attr"
*** 175

|-- FILE  /package 'syntax  /code 94  /comment 37  /file "AttrEd.hs"  /dir "syntax/Koshucode/Baala/Syntax/Attr"
|-- FILE  /package 'syntax  /code 24  /comment 17  /file "AttrName.hs"  /dir "syntax/Koshucode/Baala/Syntax/Attr"
|-- FILE  /package 'syntax  /code 68  /comment 32  /file "Message.hs"  /dir "syntax/Koshucode/Baala/Syntax/Attr"
|-- FILE  /package 'syntax  /code 72  /comment 95  /file "Parse.hs"  /dir "syntax/Koshucode/Baala/Syntax/Attr"
|-- FILE  /package 'syntax  /code 41  /comment 17  /file "Slot.hs"  /dir "syntax/Koshucode/Baala/Syntax/Attr"

|-- FILE  /package 'syntax  /code 8  /comment 3  /file "Message.hs"  /dir "syntax/Koshucode/Baala/Syntax"
|-- FILE  /package 'syntax  /code 8  /comment 4  /file "Para.hs"  /dir "syntax/Koshucode/Baala/Syntax"
|-- FILE  /package 'syntax  /code 58  /comment 39  /file "Get.hs"  /dir "syntax/Koshucode/Baala/Syntax/Para"
|-- FILE  /package 'syntax  /code 92  /comment 68  /file "Para.hs"  /dir "syntax/Koshucode/Baala/Syntax/Para"
|-- FILE  /package 'syntax  /code 195  /comment 169  /file "ParaSpec.hs"  /dir "syntax/Koshucode/Baala/Syntax/Para"

|-- FILE  /package 'syntax  /code 10  /comment 5  /file "Symbol.hs"  /dir "syntax/Koshucode/Baala/Syntax"
|-- FILE  /package 'syntax  /code 52  /comment 72  /file "AngleText.hs"  /dir "syntax/Koshucode/Baala/Syntax/Symbol"
|-- FILE  /package 'syntax  /code 10  /comment 8  /file "Message.hs"  /dir "syntax/Koshucode/Baala/Syntax/Symbol"
|-- FILE  /package 'syntax  /code 129  /comment 112  /file "Next.hs"  /dir "syntax/Koshucode/Baala/Syntax/Symbol"
|-- FILE  /package 'syntax  /code 44  /comment 32  /file "Short.hs"  /dir "syntax/Koshucode/Baala/Syntax/Symbol"

|-- FILE  /package 'syntax  /code 37  /comment 34  /file "Term.hs"  /dir "syntax/Koshucode/Baala/Syntax/Symbol"
|-- FILE  /package 'syntax  /code 8  /comment 6  /file "TTree.hs"  /dir "syntax/Koshucode/Baala/Syntax"
|-- FILE  /package 'syntax  /code 66  /comment 40  /file "Bracket.hs"  /dir "syntax/Koshucode/Baala/Syntax/TTree"
|-- FILE  /package 'syntax  /code 53  /comment 66  /file "Parse.hs"  /dir "syntax/Koshucode/Baala/Syntax/TTree"
|-- FILE  /package 'syntax  /code 38  /comment 38  /file "TokenTree.hs"  /dir "syntax/Koshucode/Baala/Syntax/TTree"

|-- FILE  /package 'syntax  /code 12  /comment 6  /file "Token.hs"  /dir "syntax/Koshucode/Baala/Syntax"
|-- FILE  /package 'syntax  /code 16  /comment 11  /file "Message.hs"  /dir "syntax/Koshucode/Baala/Syntax/Token"
|-- FILE  /package 'syntax  /code 24  /comment 47  /file "Pattern.hs"  /dir "syntax/Koshucode/Baala/Syntax/Token"
|-- FILE  /package 'syntax  /code 132  /comment 75  /file "Token.hs"  /dir "syntax/Koshucode/Baala/Syntax/Token"
|-- FILE  /package 'syntax  /code 35  /comment 14  /file "TokenClause.hs"  /dir "syntax/Koshucode/Baala/Syntax/Token"
*** 200

|-- FILE  /package 'syntax  /code 268  /comment 218  /file "TokenLine.hs"  /dir "syntax/Koshucode/Baala/Syntax/Token"
|-- FILE  /package 'syntax  /code 97  /comment 54  /file "Utility.hs"  /dir "syntax/Koshucode/Baala/Syntax/Token"
|-- FILE  /package 'toolkit  /code 69  /comment 46  /file "Change.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'toolkit  /code 25  /comment 11  /file "Input.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'toolkit  /code 32  /comment 17  /file "RDF.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Library"

|-- FILE  /package 'toolkit  /code 74  /comment 38  /file "KoshuChange.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'toolkit  /code 111  /comment 44  /file "KoshuRdf.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'toolkit  /code 172  /comment 64  /file "KoshuSyntax.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'writer  /code 12  /comment 3  /file "Writer.hs"  /dir "writer/Koshucode/Baala"
|-- FILE  /package 'writer  /code 67  /comment 17  /file "Csv.hs"  /dir "writer/Koshucode/Baala/Writer"

|-- FILE  /package 'writer  /code 26  /comment 9  /file "Html.hs"  /dir "writer/Koshucode/Baala/Writer"
|-- FILE  /package 'writer  /code 94  /comment 25  /file "Json.hs"  /dir "writer/Koshucode/Baala/Writer"
|-- FILE  /package 'writer  /code 66  /comment 32  /file "Judge.hs"  /dir "writer/Koshucode/Baala/Writer"
|-- FILE  /package 'writer  /code 87  /comment 22  /file "Koshu.hs"  /dir "writer/Koshucode/Baala/Writer"

*** 214 judges

|-- FILE-TOTAL  /package 'base  /code 2 162  /comment 1 688
|-- FILE-TOTAL  /package 'calculator  /code 464  /comment 307
|-- FILE-TOTAL  /package 'cop  /code 1 024  /comment 659
|-- FILE-TOTAL  /package 'core  /code 2 140  /comment 1 294
|-- FILE-TOTAL  /package 'data  /code 3 013  /comment 1 880

|-- FILE-TOTAL  /package 'rop-cox  /code 944  /comment 474
|-- FILE-TOTAL  /package 'rop-flat  /code 1 808  /comment 1 181
|-- FILE-TOTAL  /package 'rop-nested  /code 276  /comment 253
|-- FILE-TOTAL  /package 'syntax  /code 1 777  /comment 1 374
|-- FILE-TOTAL  /package 'toolkit  /code 483  /comment 220

|-- FILE-TOTAL  /package 'writer  /code 352  /comment 108

*** 11 judges

|-- FILE-LONG  /rank 1  /package 'syntax  /code 268  /file "TokenLine.hs"  /dir "syntax/Koshucode/Baala/Syntax/Token"
|-- FILE-LONG  /rank 2  /package 'cop  /code 219  /file "Text.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE-LONG  /rank 3  /package 'base  /code 201  /file "MixText.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE-LONG  /rank 4  /package 'cop  /code 196  /file "List.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE-LONG  /rank 5  /package 'cop  /code 195  /file "Arith.hs"  /dir "cop/Koshucode/Baala/Cop"

|-- FILE-LONG  /rank 5  /package 'syntax  /code 195  /file "ParaSpec.hs"  /dir "syntax/Koshucode/Baala/Syntax/Para"
|-- FILE-LONG  /rank 7  /package 'data  /code 187  /file "BaalaC.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE-LONG  /rank 8  /package 'rop-flat  /code 184  /file "Gadget.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE-LONG  /rank 9  /package 'data  /code 183  /file "Clock.hs"  /dir "data/Koshucode/Baala/Data/Type/Time"
|-- FILE-LONG  /rank 9  /package 'data  /code 183  /file "Time.hs"  /dir "data/Koshucode/Baala/Data/Type/Time"

*** 10 judges

**
**  SUMMARY
**      45 judges on DIR
**      11 judges on DIR-TOTAL
**     214 judges on FILE
**      10 judges on FILE-LONG
**      11 judges on FILE-TOTAL
**     291 judges in total
**

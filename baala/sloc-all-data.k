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
**    toolkit/data/SLOC.k
**    writer/data/SLOC.k
**
**  OUTPUT
**    <stdout>
**

|-- DIR  /package 'base  /code 10  /comment 4  /dir "base/Koshucode/Baala"
|-- DIR  /package 'base  /code 58  /comment 22  /dir "base/Koshucode/Baala/Base"
|-- DIR  /package 'base  /code 128  /comment 96  /dir "base/Koshucode/Baala/Base/Abort"
|-- DIR  /package 'base  /code 563  /comment 528  /dir "base/Koshucode/Baala/Base/Prelude"
|-- DIR  /package 'base  /code 302  /comment 212  /dir "base/Koshucode/Baala/Base/Syntax"

|-- DIR  /package 'base  /code 616  /comment 341  /dir "base/Koshucode/Baala/Base/Text"
|-- DIR  /package 'calculator  /code 233  /comment 182  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- DIR  /package 'calculator  /code 224  /comment 125  /dir "calculator/Koshucode/Baala/Toolkit/Main"
|-- DIR  /package 'cop  /code 18  /comment 6  /dir "cop/Koshucode/Baala"
|-- DIR  /package 'cop  /code 830  /comment 459  /dir "cop/Koshucode/Baala/Cop"

|-- DIR  /package 'core  /code 13  /comment 72  /dir "core/Koshucode/Baala"
|-- DIR  /package 'core  /code 79  /comment 102  /dir "core/Koshucode/Baala/Core"
|-- DIR  /package 'core  /code 282  /comment 138  /dir "core/Koshucode/Baala/Core/Assert"
|-- DIR  /package 'core  /code 563  /comment 325  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- DIR  /package 'core  /code 226  /comment 118  /dir "core/Koshucode/Baala/Core/Relkit"

|-- DIR  /package 'core  /code 587  /comment 294  /dir "core/Koshucode/Baala/Core/Relmap"
|-- DIR  /package 'core  /code 595  /comment 276  /dir "core/Koshucode/Baala/Core/Resource"
|-- DIR  /package 'data  /code 11  /comment 3  /dir "data/Koshucode/Baala"
|-- DIR  /package 'data  /code 79  /comment 23  /dir "data/Koshucode/Baala/Data"
|-- DIR  /package 'data  /code 586  /comment 276  /dir "data/Koshucode/Baala/Data/Church"

|-- DIR  /package 'data  /code 812  /comment 434  /dir "data/Koshucode/Baala/Data/Content"
|-- DIR  /package 'data  /code 968  /comment 691  /dir "data/Koshucode/Baala/Data/Token"
|-- DIR  /package 'data  /code 1 483  /comment 877  /dir "data/Koshucode/Baala/Data/Type"
|-- DIR  /package 'rop-cox  /code 18  /comment 6  /dir "rop-cox/Koshucode/Baala/Rop"
|-- DIR  /package 'rop-cox  /code 904  /comment 452  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
*** 25

|-- DIR  /package 'rop-flat  /code 38  /comment 12  /dir "rop-flat/Koshucode/Baala/Rop"
|-- DIR  /package 'rop-flat  /code 254  /comment 166  /dir "rop-flat/Koshucode/Baala/Rop/Base"
|-- DIR  /package 'rop-flat  /code 1 237  /comment 752  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- DIR  /package 'rop-flat  /code 183  /comment 174  /dir "rop-flat/Koshucode/Baala/Rop/Flat/Lattice"
|-- DIR  /package 'rop-nested  /code 10  /comment 6  /dir "rop-nested/Koshucode/Baala/Rop"

|-- DIR  /package 'rop-nested  /code 253  /comment 247  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- DIR  /package 'toolkit  /code 126  /comment 74  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- DIR  /package 'toolkit  /code 355  /comment 146  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- DIR  /package 'writer  /code 12  /comment 3  /dir "writer/Koshucode/Baala"
|-- DIR  /package 'writer  /code 337  /comment 105  /dir "writer/Koshucode/Baala/Writer"

*** 35 judges

|-- DIR-TOTAL  /package 'base  /code 1 677  /comment 1 203
|-- DIR-TOTAL  /package 'calculator  /code 457  /comment 307
|-- DIR-TOTAL  /package 'cop  /code 848  /comment 465
|-- DIR-TOTAL  /package 'core  /code 2 345  /comment 1 325
|-- DIR-TOTAL  /package 'data  /code 3 939  /comment 2 304

|-- DIR-TOTAL  /package 'rop-cox  /code 922  /comment 458
|-- DIR-TOTAL  /package 'rop-flat  /code 1 712  /comment 1 104
|-- DIR-TOTAL  /package 'rop-nested  /code 263  /comment 253
|-- DIR-TOTAL  /package 'toolkit  /code 481  /comment 220
|-- DIR-TOTAL  /package 'writer  /code 349  /comment 108

*** 10 judges

|-- FILE  /package 'base  /code 10  /comment 4  /file "Base.hs"  /dir "base/Koshucode/Baala"
|-- FILE  /package 'base  /code 8  /comment 6  /file "Abort.hs"  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 22  /comment 12  /file "Abortable.hs"  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 12  /comment 10  /file "Message.hs"  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 45  /comment 38  /file "Reason.hs"  /dir "base/Koshucode/Baala/Base/Abort"

|-- FILE  /package 'base  /code 49  /comment 36  /file "Report.hs"  /dir "base/Koshucode/Baala/Base/Abort"
|-- FILE  /package 'base  /code 6  /comment 4  /file "Message.hs"  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 16  /comment 3  /file "Prelude.hs"  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 131  /comment 155  /file "Assoc.hs"  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 33  /comment 20  /file "Class.hs"  /dir "base/Koshucode/Baala/Base/Prelude"

|-- FILE  /package 'base  /code 74  /comment 47  /file "Import.hs"  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 125  /comment 127  /file "List.hs"  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 80  /comment 40  /file "Order.hs"  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 46  /comment 25  /file "Pair.hs"  /dir "base/Koshucode/Baala/Base/Prelude"
|-- FILE  /package 'base  /code 74  /comment 114  /file "Snip.hs"  /dir "base/Koshucode/Baala/Base/Prelude"

|-- FILE  /package 'base  /code 10  /comment 6  /file "Syntax.hs"  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 24  /comment 20  /file "Clause.hs"  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 68  /comment 50  /file "Infix.hs"  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 93  /comment 62  /file "Line.hs"  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 15  /comment 10  /file "Message.hs"  /dir "base/Koshucode/Baala/Base/Syntax"

|-- FILE  /package 'base  /code 102  /comment 70  /file "Tree.hs"  /dir "base/Koshucode/Baala/Base/Syntax"
|-- FILE  /package 'base  /code 18  /comment 3  /file "Text.hs"  /dir "base/Koshucode/Baala/Base"
|-- FILE  /package 'base  /code 58  /comment 33  /file "CodePt.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 36  /comment 24  /file "Comment.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 82  /comment 15  /file "Http.hs"  /dir "base/Koshucode/Baala/Base/Text"
*** 25

|-- FILE  /package 'base  /code 64  /comment 34  /file "IOPoint.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 124  /comment 57  /file "TextTable.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 103  /comment 40  /file "Unicode.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 53  /comment 43  /file "Utility.hs"  /dir "base/Koshucode/Baala/Base/Text"
|-- FILE  /package 'base  /code 96  /comment 95  /file "Write.hs"  /dir "base/Koshucode/Baala/Base/Text"

|-- FILE  /package 'calculator  /code 63  /comment 49  /file "Element.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 40  /comment 15  /file "Exit.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 21  /comment 10  /file "Global.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 28  /comment 34  /file "Run.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 71  /comment 66  /file "SimpleOption.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"

|-- FILE  /package 'calculator  /code 10  /comment 8  /file "Version.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'calculator  /code 72  /comment 14  /file "KoshuFilter.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'calculator  /code 152  /comment 111  /file "KoshuMain.hs"  /dir "calculator/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'cop  /code 18  /comment 6  /file "Cop.hs"  /dir "cop/Koshucode/Baala"
|-- FILE  /package 'cop  /code 94  /comment 45  /file "Arith.hs"  /dir "cop/Koshucode/Baala/Cop"

|-- FILE  /package 'cop  /code 63  /comment 19  /file "Bundle.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 32  /comment 40  /file "Coxhand.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 220  /comment 109  /file "List.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 54  /comment 37  /file "Logic.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 4  /comment 4  /file "Message.hs"  /dir "cop/Koshucode/Baala/Cop"

|-- FILE  /package 'cop  /code 72  /comment 50  /file "Misc.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 50  /comment 34  /file "Order.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 155  /comment 65  /file "Text.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 31  /comment 27  /file "Time.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE  /package 'cop  /code 55  /comment 29  /file "Type.hs"  /dir "cop/Koshucode/Baala/Cop"
*** 50

|-- FILE  /package 'core  /code 13  /comment 72  /file "Core.hs"  /dir "core/Koshucode/Baala"
|-- FILE  /package 'core  /code 10  /comment 6  /file "Assert.hs"  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 33  /comment 30  /file "Assert.hs"  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 36  /comment 25  /file "Dataset.hs"  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 22  /comment 9  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Assert"

|-- FILE  /package 'core  /code 53  /comment 29  /file "RelTable.hs"  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 138  /comment 45  /file "Run.hs"  /dir "core/Koshucode/Baala/Core/Assert"
|-- FILE  /package 'core  /code 16  /comment 3  /file "Lexmap.hs"  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 76  /comment 67  /file "Attr.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 93  /comment 37  /file "AttrEd.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"

|-- FILE  /package 'core  /code 79  /comment 37  /file "AttrPos.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 108  /comment 81  /file "Construct.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 55  /comment 26  /file "Lexmap.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 36  /comment 25  /file "LexmapTrees.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 78  /comment 37  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"

|-- FILE  /package 'core  /code 38  /comment 15  /file "Slot.hs"  /dir "core/Koshucode/Baala/Core/Lexmap"
|-- FILE  /package 'core  /code 16  /comment 4  /file "Message.hs"  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 8  /comment 5  /file "Relkit.hs"  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 64  /comment 46  /file "Construct.hs"  /dir "core/Koshucode/Baala/Core/Relkit"
|-- FILE  /package 'core  /code 12  /comment 8  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Relkit"

|-- FILE  /package 'core  /code 60  /comment 38  /file "Relkit.hs"  /dir "core/Koshucode/Baala/Core/Relkit"
|-- FILE  /package 'core  /code 90  /comment 26  /file "Run.hs"  /dir "core/Koshucode/Baala/Core/Relkit"
|-- FILE  /package 'core  /code 16  /comment 55  /file "Relmap.hs"  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 58  /comment 77  /file "Construct.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 99  /comment 43  /file "Global.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
*** 75

|-- FILE  /package 'core  /code 35  /comment 11  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 63  /comment 20  /file "Option.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 93  /comment 52  /file "Relmap.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 111  /comment 45  /file "Result.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 31  /comment 26  /file "Rop.hs"  /dir "core/Koshucode/Baala/Core/Relmap"

|-- FILE  /package 'core  /code 97  /comment 20  /file "Specialize.hs"  /dir "core/Koshucode/Baala/Core/Relmap"
|-- FILE  /package 'core  /code 13  /comment 29  /file "Resource.hs"  /dir "core/Koshucode/Baala/Core"
|-- FILE  /package 'core  /code 170  /comment 114  /file "Clause.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 116  /comment 51  /file "Include.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 35  /comment 21  /file "Message.hs"  /dir "core/Koshucode/Baala/Core/Resource"

|-- FILE  /package 'core  /code 91  /comment 30  /file "Read.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 96  /comment 44  /file "Resource.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'core  /code 87  /comment 16  /file "Run.hs"  /dir "core/Koshucode/Baala/Core/Resource"
|-- FILE  /package 'data  /code 11  /comment 3  /file "Data.hs"  /dir "data/Koshucode/Baala"
|-- FILE  /package 'data  /code 10  /comment 3  /file "Church.hs"  /dir "data/Koshucode/Baala/Data"

|-- FILE  /package 'data  /code 169  /comment 47  /file "Build.hs"  /dir "data/Koshucode/Baala/Data/Church"
|-- FILE  /package 'data  /code 68  /comment 55  /file "Cop.hs"  /dir "data/Koshucode/Baala/Data/Church"
|-- FILE  /package 'data  /code 110  /comment 86  /file "Cox.hs"  /dir "data/Koshucode/Baala/Data/Church"
|-- FILE  /package 'data  /code 76  /comment 37  /file "Message.hs"  /dir "data/Koshucode/Baala/Data/Church"
|-- FILE  /package 'data  /code 163  /comment 51  /file "Run.hs"  /dir "data/Koshucode/Baala/Data/Church"

|-- FILE  /package 'data  /code 10  /comment 5  /file "Content.hs"  /dir "data/Koshucode/Baala/Data"
|-- FILE  /package 'data  /code 204  /comment 43  /file "BaalaC.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 211  /comment 124  /file "Class.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 165  /comment 158  /file "Decode.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 56  /comment 34  /file "Message.hs"  /dir "data/Koshucode/Baala/Data/Content"
*** 100

|-- FILE  /package 'data  /code 176  /comment 75  /file "Tree.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE  /package 'data  /code 12  /comment 4  /file "Message.hs"  /dir "data/Koshucode/Baala/Data"
|-- FILE  /package 'data  /code 18  /comment 6  /file "Token.hs"  /dir "data/Koshucode/Baala/Data"
|-- FILE  /package 'data  /code 52  /comment 72  /file "AngleText.hs"  /dir "data/Koshucode/Baala/Data/Token"
|-- FILE  /package 'data  /code 22  /comment 15  /file "Message.hs"  /dir "data/Koshucode/Baala/Data/Token"

|-- FILE  /package 'data  /code 129  /comment 112  /file "Next.hs"  /dir "data/Koshucode/Baala/Data/Token"
|-- FILE  /package 'data  /code 44  /comment 32  /file "Short.hs"  /dir "data/Koshucode/Baala/Data/Token"
|-- FILE  /package 'data  /code 37  /comment 34  /file "Term.hs"  /dir "data/Koshucode/Baala/Data/Token"
|-- FILE  /package 'data  /code 252  /comment 100  /file "Token.hs"  /dir "data/Koshucode/Baala/Data/Token"
|-- FILE  /package 'data  /code 34  /comment 14  /file "TokenClause.hs"  /dir "data/Koshucode/Baala/Data/Token"

|-- FILE  /package 'data  /code 264  /comment 217  /file "TokenLine.hs"  /dir "data/Koshucode/Baala/Data/Token"
|-- FILE  /package 'data  /code 134  /comment 95  /file "TokenTree.hs"  /dir "data/Koshucode/Baala/Data/Token"
|-- FILE  /package 'data  /code 29  /comment 5  /file "Type.hs"  /dir "data/Koshucode/Baala/Data"
|-- FILE  /package 'data  /code 11  /comment 5  /file "About.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 93  /comment 41  /file "Arithmetic.hs"  /dir "data/Koshucode/Baala/Data/Type"

|-- FILE  /package 'data  /code 183  /comment 84  /file "Clock.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 73  /comment 43  /file "Date.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 153  /comment 79  /file "Decimal.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 39  /comment 9  /file "Decimal2.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 150  /comment 155  /file "Head.hs"  /dir "data/Koshucode/Baala/Data/Type"

|-- FILE  /package 'data  /code 34  /comment 12  /file "Interp.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 117  /comment 73  /file "Judge.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 19  /comment 13  /file "Message.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 30  /comment 23  /file "Mono.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 192  /comment 73  /file "Para.hs"  /dir "data/Koshucode/Baala/Data/Type"
*** 125

|-- FILE  /package 'data  /code 70  /comment 68  /file "Rel.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 183  /comment 108  /file "Time.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'data  /code 136  /comment 91  /file "Type.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE  /package 'rop-cox  /code 18  /comment 6  /file "Cox.hs"  /dir "rop-cox/Koshucode/Baala/Rop"
|-- FILE  /package 'rop-cox  /code 133  /comment 35  /file "Accessor.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"

|-- FILE  /package 'rop-cox  /code 18  /comment 6  /file "Bundle.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 175  /comment 85  /file "Calc.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 54  /comment 31  /file "Empty.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 54  /comment 41  /file "Filter.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 177  /comment 85  /file "Gadget.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"

|-- FILE  /package 'rop-cox  /code 102  /comment 47  /file "GeoDatumJp.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 82  /comment 46  /file "Get.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 4  /comment 4  /file "Message.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-cox  /code 105  /comment 72  /file "Range.hs"  /dir "rop-cox/Koshucode/Baala/Rop/Cox"
|-- FILE  /package 'rop-flat  /code 10  /comment 6  /file "Base.hs"  /dir "rop-flat/Koshucode/Baala/Rop"

|-- FILE  /package 'rop-flat  /code 49  /comment 13  /file "Define.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"
|-- FILE  /package 'rop-flat  /code 125  /comment 76  /file "Get.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"
|-- FILE  /package 'rop-flat  /code 11  /comment 9  /file "Message.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"
|-- FILE  /package 'rop-flat  /code 22  /comment 39  /file "Rop.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"
|-- FILE  /package 'rop-flat  /code 47  /comment 29  /file "Term.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Base"

|-- FILE  /package 'rop-flat  /code 28  /comment 6  /file "Flat.hs"  /dir "rop-flat/Koshucode/Baala/Rop"
|-- FILE  /package 'rop-flat  /code 30  /comment 5  /file "Bundle.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 82  /comment 49  /file "Check.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 94  /comment 48  /file "Control.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 183  /comment 103  /file "Gadget.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
*** 150

|-- FILE  /package 'rop-flat  /code 8  /comment 26  /file "Lattice.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 93  /comment 53  /file "Restrict.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat/Lattice"
|-- FILE  /package 'rop-flat  /code 18  /comment 21  /file "Rop.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat/Lattice"
|-- FILE  /package 'rop-flat  /code 72  /comment 100  /file "Tropashko.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat/Lattice"
|-- FILE  /package 'rop-flat  /code 59  /comment 35  /file "Message.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"

|-- FILE  /package 'rop-flat  /code 172  /comment 84  /file "Meta.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 63  /comment 32  /file "Order.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 198  /comment 107  /file "Peripheral.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 56  /comment 35  /file "PoScale.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 65  /comment 36  /file "Resource.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"

|-- FILE  /package 'rop-flat  /code 49  /comment 73  /file "Source.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 102  /comment 66  /file "Term.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-flat  /code 76  /comment 53  /file "TermGadget.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE  /package 'rop-nested  /code 10  /comment 6  /file "Nest.hs"  /dir "rop-nested/Koshucode/Baala/Rop"
|-- FILE  /package 'rop-nested  /code 93  /comment 86  /file "Confl.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"

|-- FILE  /package 'rop-nested  /code 58  /comment 59  /file "Deriv.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- FILE  /package 'rop-nested  /code 65  /comment 47  /file "Flow.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- FILE  /package 'rop-nested  /code 11  /comment 6  /file "Message.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- FILE  /package 'rop-nested  /code 26  /comment 49  /file "Rop.hs"  /dir "rop-nested/Koshucode/Baala/Rop/Nest"
|-- FILE  /package 'toolkit  /code 69  /comment 46  /file "Change.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Library"

|-- FILE  /package 'toolkit  /code 25  /comment 11  /file "Input.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'toolkit  /code 32  /comment 17  /file "RDF.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Library"
|-- FILE  /package 'toolkit  /code 74  /comment 38  /file "KoshuChange.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'toolkit  /code 110  /comment 44  /file "KoshuRdf.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
|-- FILE  /package 'toolkit  /code 171  /comment 64  /file "KoshuSyntax.hs"  /dir "toolkit/Koshucode/Baala/Toolkit/Main"
*** 175

|-- FILE  /package 'writer  /code 12  /comment 3  /file "Writer.hs"  /dir "writer/Koshucode/Baala"
|-- FILE  /package 'writer  /code 66  /comment 17  /file "Csv.hs"  /dir "writer/Koshucode/Baala/Writer"
|-- FILE  /package 'writer  /code 26  /comment 9  /file "Html.hs"  /dir "writer/Koshucode/Baala/Writer"
|-- FILE  /package 'writer  /code 93  /comment 25  /file "Json.hs"  /dir "writer/Koshucode/Baala/Writer"
|-- FILE  /package 'writer  /code 66  /comment 32  /file "Judge.hs"  /dir "writer/Koshucode/Baala/Writer"

|-- FILE  /package 'writer  /code 86  /comment 22  /file "Koshu.hs"  /dir "writer/Koshucode/Baala/Writer"

*** 181 judges

|-- FILE-TOTAL  /package 'base  /code 1 677  /comment 1 203
|-- FILE-TOTAL  /package 'calculator  /code 457  /comment 307
|-- FILE-TOTAL  /package 'cop  /code 848  /comment 465
|-- FILE-TOTAL  /package 'core  /code 2 345  /comment 1 325
|-- FILE-TOTAL  /package 'data  /code 3 939  /comment 2 304

|-- FILE-TOTAL  /package 'rop-cox  /code 922  /comment 458
|-- FILE-TOTAL  /package 'rop-flat  /code 1 712  /comment 1 104
|-- FILE-TOTAL  /package 'rop-nested  /code 263  /comment 253
|-- FILE-TOTAL  /package 'toolkit  /code 481  /comment 220
|-- FILE-TOTAL  /package 'writer  /code 349  /comment 108

*** 10 judges

|-- FILE-LONG  /rank 1  /package 'data  /code 264  /file "TokenLine.hs"  /dir "data/Koshucode/Baala/Data/Token"
|-- FILE-LONG  /rank 2  /package 'data  /code 252  /file "Token.hs"  /dir "data/Koshucode/Baala/Data/Token"
|-- FILE-LONG  /rank 3  /package 'cop  /code 220  /file "List.hs"  /dir "cop/Koshucode/Baala/Cop"
|-- FILE-LONG  /rank 4  /package 'data  /code 211  /file "Class.hs"  /dir "data/Koshucode/Baala/Data/Content"
|-- FILE-LONG  /rank 5  /package 'data  /code 204  /file "BaalaC.hs"  /dir "data/Koshucode/Baala/Data/Content"

|-- FILE-LONG  /rank 6  /package 'rop-flat  /code 198  /file "Peripheral.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"
|-- FILE-LONG  /rank 7  /package 'data  /code 192  /file "Para.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE-LONG  /rank 8  /package 'data  /code 183  /file "Clock.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE-LONG  /rank 8  /package 'data  /code 183  /file "Time.hs"  /dir "data/Koshucode/Baala/Data/Type"
|-- FILE-LONG  /rank 8  /package 'rop-flat  /code 183  /file "Gadget.hs"  /dir "rop-flat/Koshucode/Baala/Rop/Flat"

*** 10 judges

**
**  SUMMARY
**      35 judges on DIR
**      10 judges on DIR-TOTAL
**     181 judges on FILE
**      10 judges on FILE-LONG
**      10 judges on FILE-TOTAL
**     246 judges in total
**

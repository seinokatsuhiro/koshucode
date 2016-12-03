{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Running relational calculation.

module Koshucode.Baala.Core.Assert.Run
  ( runAssertJudges,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Data                  as D
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Relkit           as C
import qualified Koshucode.Baala.Core.Relmap           as C
import qualified Koshucode.Baala.Core.Assert.Assert    as C
import qualified Koshucode.Baala.Core.Assert.RelTable  as C
import qualified Koshucode.Baala.Data.Message          as Msg
import qualified Koshucode.Baala.Core.Assert.Message   as Msg


-- ----------------------  Assert

-- | Calculate assertion list.
runAssertJudges :: (Ord c, D.CContent c, D.SelectRel h, C.GetGlobal h)
  => h c -> C.Option c -> C.ShortAsserts' h c -> B.Ab (C.ShortResultChunks c)
runAssertJudges hook opt a =
    do chunks <- runAssertDataset hook opt a
       Right $ a { S.shortBody = chunks }

-- | Calculate assertion list.
runAssertDataset :: forall h. forall c. (Ord c, D.CContent c, D.SelectRel h, C.GetGlobal h)
  => h c -> C.Option c -> C.ShortAsserts' h c -> B.Ab [C.ResultChunk c]
runAssertDataset hook option (S.Short _ sh ass) =
    Right . concat =<< mapM each ass
    where
      each (C.Assert _ _ _ _ _ Nothing _) = B.bug "runAssertDataset"
      each a@(C.Assert _ typ pat _ opt (Just relmap) libs) =
          Msg.abAssert [a] $ do
            r1 <- runRelmapViaRelkit hook libs relmap D.reldee
            let judgeOf showEmpty = assert showEmpty typ
            optionProcess sh judgeOf pat option opt r1

      assert :: (D.CEmpty c) => Bool -> D.AssertType -> D.JudgeOf c
      assert True  q p = D.assertAs q p
      assert False q p = D.assertAs q p . D.omitEmpty

runRelmapViaRelkit :: (Ord c, D.CRel c, D.SelectRel h, C.GetGlobal h)
  => h c -> C.RelmapLinkTable' h c
  -> C.Relmap' h c -> B.AbMap (D.Rel c)
runRelmapViaRelkit hook links r (D.Rel he1 bo1) =
    do (kdef, C.Relkit hi2' ho2' f2') <- C.relmapSpecialize hook links [] (Just he1) r
       let C.Relkit _ mhe2 f2 = C.relkitLink kdef $ C.Relkit hi2' ho2' f2'
       he2 <- just "unknown relhead" mhe2
       bo2 <- C.relkitRun hook [] f2 bo1
       Right $ D.Rel he2 bo2
    where
      just :: String -> Maybe a -> B.Ab a
      just _ (Just h) = Right h
      just s Nothing  = Msg.adlib s


-- ---------------------------------  Options

optionType :: S.ParaSpec String
optionType = S.paraSpec $ S.paraMin 0 . S.paraOpt
             [ "empty"     -- show empty filler
             , "forward"   -- move terms to front
             , "backward"  -- move terms to rear
             , "lexical"   -- make terms lexical order
             , "order"     -- sort list of judges by content
             , "align"     -- align terms vertically
             , "table"     -- output relation in tabular format
             ]

optionProcess :: (Ord c, D.CRel c, B.MixTransEncode c)
    => [S.ShortDef] -> (Bool -> D.JudgeOf c) -> D.JudgeClass
    -> C.Option c -> C.TTreePara
    -> D.Rel c -> B.Ab [C.ResultChunk c]
optionProcess sh judgeOf pat option opt r1 =
    do case S.paraMatch optionType opt of
         Right _  -> Right ()
         Left u   -> Msg.unkOption u
       showEmpty <- S.paraGetSwitch opt "empty"
       r2 <- optionRelmapResource option r1
       r3 <- optionRelmapAssert   opt    r2
       let js  = D.judgesFromRel (judgeOf showEmpty) pat r3
       comment <- optionComment sh pat opt r3
       Right [ C.ResultJudge js
             , C.ResultRel pat r3
             , C.ResultNote comment ]

optionRelmapResource :: (Ord c, D.CRel c) => C.Option c -> B.AbMap (D.Rel c)
optionRelmapResource option r1 =
    Right r1 >>= call optionOrder (C.optionBool "order" option)
    where call f True  r2 = f [] r2
          call _ False r2 = Right r2

optionRelmapAssert :: (Ord c, D.CRel c) => C.TTreePara -> B.AbMap (D.Rel c)
optionRelmapAssert opt r1 =
    Right r1 >>= call optionForward  "forward"
             >>= call optionBackward "backward"
             >>= call optionLexical  "lexical"
             >>= call optionOrder    "order"
    where call f name r2 = case S.paraGet opt name of
                             Right args -> f args r2
                             Left _     -> Right r2

optionComment :: (D.CRel c, B.MixTransEncode c) =>
    [S.ShortDef] -> D.JudgeClass -> C.TTreePara -> D.Rel c -> B.Ab [String]
optionComment sh p opt r =
    do optTable <- S.paraGetSwitch opt "table"
       case optTable of
         True  -> Right $ title : "" : table
         False -> Right []
    where
      title = "TABLE : " ++ p
      table = ("  " ++) `map` C.relTableLines sh r


-- ---------------------------------  Option "forward" and "backward"

optionForward, optionBackward :: (Ord c) => [S.TTree] -> B.AbMap (D.Rel c)
optionForward  = optionToward True
optionBackward = optionToward False

-- | Apply forward and backward option.
optionToward :: (Ord c) => Bool -> [S.TTree] -> B.AbMap (D.Rel c)
optionToward dir opt2 r1 =
    do ns <- D.treesFlatNames opt2
       towardRel dir ns r1

towardRel :: (Ord c) => Bool -> [S.TermName] -> B.AbMap (D.Rel c)
towardRel dir ns (D.Rel he1 bo1)
    | D.newTermsExist pk  = Msg.unkTerm (D.newTerms pk) he1
    | otherwise           = Right $ D.Rel he2 bo2
    where
      pk    = D.termPicker ns he1
      he2   = D.towardTerms dir pk `D.headMap` he1
      bo2   = D.towardTerms dir pk `map` bo1

-- | lexical option.
optionLexical :: (Ord c) => [S.TTree] -> B.AbMap (D.Rel c)
optionLexical _ = lexicalOrderRel

lexicalOrderRel :: (Ord c) => B.AbMap (D.Rel c)
lexicalOrderRel rel@(D.Rel he1 _) = towardRel True ns' rel where
    ns' = B.sort $ D.getTermNames he1


-- ---------------------------------  Option "order"

optionOrder :: (Ord c, D.CRel c) => [S.TTree] ->  B.AbMap (D.Rel c)
optionOrder _ r1 = Right $ relSortDeep r1

relSortDeep :: (Ord c, D.CRel c) => O.Map (D.Rel c)
relSortDeep = relApply f where
    f (D.Rel he bo) = D.Rel he $ B.sort bo

relApply :: (D.CRel c) => O.Map (O.Map (D.Rel c))
relApply f (D.Rel he bo) = f $ D.Rel he $ B.map2 nest bo where
    nest c | D.isRel c = D.pRel $ relApply f $ D.gRel c
           | otherwise = c


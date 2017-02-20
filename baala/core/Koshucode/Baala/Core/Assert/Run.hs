{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Running relational calculation.

module Koshucode.Baala.Core.Assert.Run
  ( runAssertJudges,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type                  as T
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
runAssertJudges :: (Ord c, D.CContent c, T.SelectRel h, C.GetGlobal' h)
  => h c -> C.Option c -> C.ShortAsserts' h c -> B.Ab (C.ShortResultChunks c)
runAssertJudges hook opt a =
    do chunks <- runAssertDataset hook opt a
       Right $ a { S.shortBody = chunks }

-- | Calculate assertion list.
runAssertDataset :: forall h. forall c. (Ord c, D.CContent c, T.SelectRel h, C.GetGlobal' h)
  => h c -> C.Option c -> C.ShortAsserts' h c -> B.Ab [C.ResultChunk c]
runAssertDataset hook option (S.Short _ sh ass) =
    Right . concat O.# mapM each ass
    where
      each (C.Assert _ _ _ _ _ Nothing _) = B.bug "runAssertDataset"
      each a@(C.Assert _ typ pat _ opt (Just relmap) libs) =
          Msg.abAssert [a] $ do
            r1 <- runRelmapViaRelkit hook libs relmap T.reldee
            let judgeOf showEmpty = assert showEmpty typ
            optionProcess sh judgeOf pat option opt r1

      assert :: (D.CEmpty c) => Bool -> T.AssertType -> T.JudgeOf c
      assert True  q p = T.assertAs q p
      assert False q p = T.assertAs q p . D.cutEmpty

runRelmapViaRelkit :: (D.CContent c, T.SelectRel h, C.GetGlobal' h)
  => h c -> C.RelmapLinkTable' h c
  -> C.Relmap' h c -> B.AbMap (T.Rel c)
runRelmapViaRelkit hook links r (T.Rel he1 bo1) =
    do (kdef, C.Relkit hi2' ho2' f2') <- C.relmapSpecialize hook links [] (Just he1) r
       let C.Relkit _ mhe2 f2 = C.relkitLink kdef $ C.Relkit hi2' ho2' f2'
       he2 <- just "unknown relhead" mhe2
       bo2 <- C.relkitRun hook [] f2 bo1
       Right $ T.Rel he2 bo2
    where
      just :: String -> Maybe a -> B.Ab a
      just _ (Just h) = Right h
      just s Nothing  = Msg.adlib s


-- ---------------------------------  Options

optionType :: S.ParaSpec S.Chars
optionType = S.paraSpec $ S.paraMin 0 . S.paraOpt
             [ "empty"     -- show empty filler
             , "forward"   -- move terms to front
             , "backward"  -- move terms to rear
             , "lexical"   -- make terms lexical order
             , "order"     -- sort list of judges by content
             , "align"     -- align terms vertically
             , "table"     -- output relation in tabular format
             ]

optionProcess :: (Ord c, D.CRel c, B.MixEncode c)
    => [S.ShortDef String] -> (Bool -> T.JudgeOf c) -> S.JudgeClass
    -> C.Option c -> C.TTreePara S.Chars
    -> T.Rel c -> B.Ab [C.ResultChunk c]
optionProcess sh judgeOf pat option opt r1 =
    do case S.paraMatch optionType opt of
         Right _  -> Right ()
         Left u   -> Msg.unkOption (O.tString <$> u)
       showEmpty <- S.paraGetSwitch opt "empty"
       r2 <- optionRelmapResource option r1
       r3 <- optionRelmapAssert   opt    r2
       let js  = T.judgesFromRel (judgeOf showEmpty) pat r3
       comment <- optionComment sh pat opt r3
       -- TODO: unify judge, rel, note
       Right [ C.ResultJudge js
             , C.ResultRel pat r3
             , C.ResultNote comment ]

optionRelmapResource :: (Ord c, D.CRel c) => C.Option c -> B.AbMap (T.Rel c)
optionRelmapResource option r1 =
    Right r1 >>= call optionOrder (C.optionBool "order" option)
    where call f True  r2 = f [] r2
          call _ False r2 = Right r2

optionRelmapAssert :: (Ord c, D.CRel c) => C.TTreePara S.Chars -> B.AbMap (T.Rel c)
optionRelmapAssert opt r1 =
    Right r1 >>= call optionForward  "forward"
             >>= call optionBackward "backward"
             >>= call optionLexical  "lexical"
             >>= call optionOrder    "order"
    where call f name r2 = case S.paraGet opt name of
                             Right args -> f args r2
                             Left _     -> Right r2

optionComment :: (D.CRel c, B.MixEncode c) =>
    [S.ShortDef String] -> S.JudgeClass -> C.TTreePara S.Chars -> T.Rel c -> B.Ab [String]
optionComment sh p opt r =
    do optTable <- S.paraGetSwitch opt "table"
       case optTable of
         True  -> Right $ title : "" : table
         False -> Right []
    where
      title = "TABLE : " ++ p
      table = ("  " ++) `map` C.relTableLines sh r


-- ---------------------------------  Option "forward" and "backward"

optionForward, optionBackward :: (Ord c) => [S.Tree] -> B.AbMap (T.Rel c)
optionForward  = optionToward True
optionBackward = optionToward False

-- | Apply forward and backward option.
optionToward :: (Ord c) => Bool -> [S.Tree] -> B.AbMap (T.Rel c)
optionToward dir opt2 r1 =
    do ns <- D.treesFlatNames opt2
       towardRel dir ns r1

towardRel :: (Ord c) => Bool -> [S.TermName] -> B.AbMap (T.Rel c)
towardRel dir ns (T.Rel he1 bo1)
    | T.newTermsExist pk  = Msg.newTerm pk he1
    | otherwise           = Right $ T.Rel he2 bo2
    where
      pk    = T.termPicker ns he1
      he2   = T.towardTerms dir pk `T.headMap` he1
      bo2   = T.towardTerms dir pk `map` bo1

-- | lexical option.
optionLexical :: (Ord c) => [S.Tree] -> B.AbMap (T.Rel c)
optionLexical _ = lexicalOrderRel

lexicalOrderRel :: (Ord c) => B.AbMap (T.Rel c)
lexicalOrderRel rel@(T.Rel he1 _) = towardRel True ns' rel where
    ns' = O.sort $ T.getTermNames he1


-- ---------------------------------  Option "order"

optionOrder :: (Ord c, D.CRel c) => [S.Tree] ->  B.AbMap (T.Rel c)
optionOrder _ r1 = Right $ relSortDeep r1

relSortDeep :: (Ord c, D.CRel c) => O.Map (T.Rel c)
relSortDeep = relApply f where
    f (T.Rel he bo) = T.Rel he $ O.sort bo

relApply :: (D.CRel c) => O.Map (O.Map (T.Rel c))
relApply f (T.Rel he bo) = f $ T.Rel he (nest O.<$$> bo) where
    nest c | D.isRel c = D.pRel $ relApply f $ D.gRel c
           | otherwise = c


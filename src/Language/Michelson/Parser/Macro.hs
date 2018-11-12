module Language.Michelson.Parser.Macro where

{- compare macros:
    * cmp{eq}
    * if{eq}
    * ifcmp{eq}
-}

-- fail

{- assertion macros:
  * assert
  * assert_{eq}
  * assert_none
  * assert_some
  * assert_left
  * assert_right
-}

{- syntactic "conveniences"
   * DII+P code
   * DUU+P
   * [PAIR]
   * UNPAIR
   * C[AD]R
   * IF_SOME
   * SET_C[AD]R
   * MAP_C[AD]R
-}

-- Macros

--macro :: Parser M.ISeq
--macro =

--cmp :: Parser M.ISeq
--cmp = do
--  string "CMP"
--  a <- cmp_op
--  return $ M.ISeq $ Seq.fromList [M.COMPARE (M.Nv Nothing), a]
--
--ifcmp :: Parser M.ISeq


{-# Language TypeSynonymInstances, FlexibleInstances #-}
{-# Language ScopedTypeVariables #-}

module Lib
    ( someFunc
    ) where

import Data.Tree
import Data.Tree.Lens
import Control.Zipper
import Control.Lens
import Data.Word
import Test.QuickCheck hiding (within)
import Control.Monad
import Text.PrettyPrint.ANSI.Leijen
import qualified Text.PrettyPrint.ANSI.Leijen as Pret

type MyTree = Tree Word8

{--
class ToExplore a where
  data Direction a :: * -> *
  focus ::
  moveFocus ::
  display ::
--}

{--
-- | Neat 2-dimensional drawing of a tree.
drawTree :: Tree String -> String
drawTree  = unlines . draw

-- | Neat 2-dimensional drawing of a forest.
drawForest :: Forest String -> String
drawForest  = unlines . map drawTree

draw :: Tree String -> [String]
draw (Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)
--}

instance Pretty Word8 where
  pretty = text . show

instance Pretty a => Pretty (Tree a) where
  pretty (Node r ts) = pretty r
                       Pret.<+>
                       (align . vsep . map pretty $ ts)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    n <- getSize
    n <- choose (0, n `quot` 5)
    liftM2 Node arbitrary (vectorOf n arbitrary)

someFunc :: IO ()
someFunc = do
--  t <- unfoldTreeM_BF (\b->pure (b,[b+1, b+2])) (0 :: Word8)
  -- let t = unfoldTree (\b->(b,[b-1, b+1])) 0
  (t :: MyTree) <- generate $ resize 10 arbitrary
  putStrLn
    -- $ take 1000
    $ drawTree
    $ fmap show t
  -- putDoc $ pretty t
  let z = zipper t
        & focus
        {--
        & fromWithin traverse
        -- & downward
        & rightmost
        & focus
        & fromWithin traverse
        & rightmost
        --}
  print z
  --putDoc $ pretty
    -- $ view focus $ z

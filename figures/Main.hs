-- Copyright 2017 Google Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs, TemplateHaskell #-}

import Diagrams.Prelude hiding (Empty)
import Diagrams.TwoD.Layout.Tree
import Diagrams.TwoD.Layout.Grid
import Diagrams.TwoD.Text
import Diagrams.Backend.Rasterific.CmdLine
import Test.Feat
import System.Random
import Control.Monad
import qualified Control.Monad.Random.Class as R
import qualified Control.Monad.Random as R
import qualified Data.Map as M
import Debug.Trace

deriveEnumerable ''BTree

type BT = BTree ()
type BTT = BTree (BTree ())
type LBT = BTree Integer

newtype Nat = Nat { nat :: Integer }
  deriving Show

instance Enumerable Nat where
  enumerate = consts [unary (Nat . (+1) . nat), nullary (Nat 0)]

drawDecoratedTree :: BTree (Diagram B) -> Diagram B
drawDecoratedTree tree =
  maybe mempty (renderTree id (~~))
  (symmLayoutBin' (with & slHSep .~ 4 & slVSep .~ 4) tree)
  # lw veryThin

decorateBt :: BT -> BTree (Diagram B)
decorateBt (BNode _ l r) =
  BNode (circle 1 # fc white) (decorateBt l) (decorateBt r)
decorateBt Empty = BNode (circle 0.2 # fc black) Empty Empty

decorateBtt :: BTT -> BTree (Diagram B)
decorateBtt (BNode t l r) =
  BNode (td <> nd) (decorateBtt l) (decorateBtt r)
    where td = scale 0.15 (centerXY (drawTree t))
          nd = circle 1.5 # fc white
decorateBtt Empty = BNode (circle 0.2 # fc black) Empty Empty

decorateLBt :: LBT -> BTree (Diagram B)
decorateLBt t = decorateLBt' t
  where
    n = sum t
    colorFor 0 = white
    colorFor i = darken (1 - (fromIntegral i / fromIntegral n)) white
    decorateLBt' (BNode i l r) = BNode img (decorateLBt' l) (decorateLBt' r)
      where
        img = circle 1 # fc (colorFor i)
    decorateLBt' Empty = BNode (circle 0.2 # fc black) Empty Empty

drawTree t = drawDecoratedTree (decorateBt t)

drawTreeOfTrees t = drawDecoratedTree (decorateBtt t)

drawLTree t = drawDecoratedTree (decorateLBt t)

treeGrid :: (Int, [BT]) -> Diagram B
treeGrid (n, trees) = text (show n) # fc black # scale 2 # withBox 2 # alignBR
                   <> (gridCat $ map (frame 3 . drawTree) trees) # withBox 3 # alignBR

treeGrid' :: [BT] -> Diagram B
treeGrid' trees =  (gridCat $ map (frame 3 . drawTree) trees) # withBox 3 # alignBR

treeGrid'S :: Int -> [BT] -> Diagram B
treeGrid'S d trees =  (gridCat $ map draw trees) # withBox 3 # alignBR
  where draw tree = (if saturated tree then lc red else id) (frame 3 $ drawTree tree)
        saturated = saturated' d
        saturated' 1 _ = True
        saturated' n (BNode _ l r) = saturated' (n-1) l && saturated' (n-1) r
        saturated' _ _ = False


treeGrid'With :: Int -> [Diagram B] -> Diagram B
treeGrid'With numcols trees = (gridCat' numcols $ map (frame 3) trees) # withBox 3 # alignBR

withBox :: Double -> Diagram B -> Diagram B
withBox padding d = boundingRect (frame padding d) # lw veryThin <> d

nonEmptyParts :: [[BT]]
nonEmptyParts = filter (not . null) $ map snd $ values

diagram :: Int -> Diagram B
diagram n = (hsep 3 $ map (alignT . treeGrid) $ zip [0..] $ take n nonEmptyParts)
            # frame 1
            # bg white

treesUpToDepth :: Int -> [BTree ()]
treesUpToDepth 0 = [Empty]
treesUpToDepth d = Empty : [BNode () t1 t2 | t1 <- subTrees, t2 <- subTrees]
  where subTrees = treesUpToDepth (d-1)

smallTrees :: Int -> Diagram B
smallTrees d = treeGrid' (treesUpToDepth d) # frame 3 # bg white

smallTreesOfDepth :: Int -> Diagram B
smallTreesOfDepth d = treeGrid'S d (filter ((==(d+1)) . depth) $ treesUpToDepth d) # frame 3 # bg white
  where depth Empty = 1
        depth (BNode _ l r) = 1 + max (depth l) (depth r)

randomTreeMaxDepth :: Int -> IO (BTree ())
randomTreeMaxDepth 0 = return Empty
randomTreeMaxDepth d = do
  n <- randomRIO (0, 5) :: IO Int
  if n == 0
    then return Empty
    else do
      l <- randomTreeMaxDepth (d-1)
      r <- randomTreeMaxDepth (d-1)
      return (BNode () l r)

randomTreeQc :: Int -> IO (BTree ())
randomTreeQc 0 = return Empty
randomTreeQc n = do
  i <- randomRIO (0, 5) :: IO Int
  if i == 0
    then return Empty
    else do
      l <- randomTreeQc (n `div` 2)
      r <- randomTreeQc (n `div` 2)
      return (BNode () l r)

randomTreeGood :: Int -> IO (BTree ())
randomTreeGood 0 = return Empty
randomTreeGood n = do
  i <- randomRIO (0, n - 1) :: IO Int
  l <- randomTreeGood i
  r <- randomTreeGood (n - 1 - i)
  return (BNode () l r)

randomTreesMaxDepth :: Int -> Int -> IO (Diagram B)
randomTreesMaxDepth depth n = do
  trees <- replicateM n (randomTreeMaxDepth depth)
  return $ treeGrid'With 4 (map drawTree trees) # frame 3 # bg white

randomTreesQc :: Int -> Int -> IO (Diagram B)
randomTreesQc s n = do
  trees <- replicateM n (randomTreeQc s)
  return $ treeGrid'With 4 (map drawTree trees) # frame 3 # bg white

randomTreesGood :: Int -> Int -> IO (Diagram B)
randomTreesGood s n = do
  trees <- replicateM n (randomTreeGood s)
  return $ treeGrid'With 5 (map drawTree trees) # frame 3 # bg white

randomLbTreesBad :: R.MonadRandom m => Integer -> Int -> m (Diagram B)
randomLbTreesBad s n = do
  trees <- replicateM n (randomLbtBad s)
  return $ treeGrid'With 4 (map drawLTree trees) # frame 3 # bg white

randomLbtGood :: R.MonadRandom m => Int -> m LBT
randomLbtGood s = do
  let cardinal = fst ((values :: [(Integer, [BTree Nat])]) !! s)
  index <- R.getRandomR (0, cardinal-1)
  return $ fmap nat (select s index)

randomLbTreesGood :: R.MonadRandom m => Int -> Int -> m (Diagram B)
randomLbTreesGood s n = do
  trees <- replicateM n (randomLbtGood s)
  return $ treeGrid'With 4 (map drawLTree trees) # frame 3 # bg white

sizesByDepth' :: Int -> LBT -> [(Int, Integer)]
sizesByDepth' _ Empty = []
sizesByDepth' d (BNode s l r) = (d,s) : (sizesByDepth' (d+1) l) ++ (sizesByDepth' (d+1) r)

sizesByDepth :: [LBT] -> [(Int, Integer, Int)]
sizesByDepth lbts = map unpack (M.toList sizeMap)
  where
    unpack ((depth, size), count) = (depth, size, count)
    sizeMap = M.unionsWith (+) sizeMaps
    sizeMaps = map (flip M.singleton 1) sizes
    sizes = concatMap (sizesByDepth' 0) lbts

randomSizesByDepth :: R.MonadRandom m => Int -> Int -> m [(Int, Integer, Int)]
randomSizesByDepth treeSize numTrees = do
  trees <- replicateM numTrees (randomLbtGood treeSize)
  return (sizesByDepth trees)

exampleTree :: Diagram B
exampleTree = drawTree tree # withBox 3 # bg white
  where
    tree =
      BNode ()
        (BNode ()
          Empty
          (BNode () Empty Empty))
        (BNode ()
          (BNode () Empty Empty)
          (BNode () Empty Empty))

treeOfTrees :: BTree (BTree ())
treeOfTrees = select 79 (39533034221095268 * 2 `div` 3)

exampleTreeOfTrees :: Diagram B
exampleTreeOfTrees = drawTreeOfTrees treeOfTrees # bg white

sums :: Integer -> Integer -> [[Integer]]
sums n 1 = [[n]]
sums n k = [p:tail | p <- [0..n], tail <- sums (n-p) (k-1)]

splitSum :: R.MonadRandom m => Integer -> Integer -> m [Integer]
splitSum n k = R.uniform (sums n k)

randomLbtBad :: R.MonadRandom m => Integer -> m LBT
randomLbtBad 0 = return Empty
randomLbtBad n = do
  [k,l,r] <- splitSum (n-1) 3
  left <- randomLbtBad l
  right <- randomLbtBad r
  return (BNode k left right)

main = do
  rtrees <- randomTreesMaxDepth 5 28
  rtreesqc <- randomTreesQc 30 28
  rtreesgood <- randomTreesGood 30 20
  lbtBad <- R.evalRandIO (randomLbTreesBad 20 8)
  lbtGood <- R.evalRandIO (randomLbTreesGood 20 8)
  --lbtSizesGood <- R.evalRandIO (randomSizesByDepth 100 10000)
  --writeFile "/tmp/size_by_depth_good" (show lbtSizesGood)
  mainWith [
    ("trees0", diagram 1),
    ("trees1", diagram 2),
    ("trees2", diagram 3),
    ("trees3", diagram 4),
    ("trees4", diagram 5),
    ("trees6", diagram 6),
    ("smalltrees", smallTrees 3),
    ("smalltreesofdepth", smallTreesOfDepth 3),
    ("randomtreesmaxdepth", rtrees),
    ("randomtreesgood", rtreesgood),
    ("randomtreesqc", rtreesqc),
    ("exampleTree", exampleTree),
    ("treeOfTrees", exampleTreeOfTrees),
    ("randomLTreesBad", lbtBad),
    ("randomLTreesGood", lbtGood)]

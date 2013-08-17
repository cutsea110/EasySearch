module EasySearch where

import Control.Monad

type Graph = ([Int], [(Int,Int)])

-- graph with loop
gr0 :: Graph
gr0 = ( [1..9], -- nodes
        [(1,2),(1,3),(1,4),(2,5),(3,5),(3,6),(4,7),(5,1),(6,7),(6,8),(7,9),(8,4)] -- arrows
      )

find_next :: Int -> Graph -> [Int]
find_next k gr = map snd $ filter ((==k) . fst) (snd gr)

type From = Int
type To = Int

-- depth first search
-- dfs :: Graph -> Int -> Int -> Maybe [Int]
-- dfs :: Graph -> From -> To -> [[Int]]
dfs :: MonadPlus m => Graph -> From -> To -> m [Int]
dfs gr p0 p1 = dfs_aux gr p0 p1 []

dfs_aux :: MonadPlus m => Graph -> From -> To -> [Int] -> m [Int]
dfs_aux gr p0 p1 path
  | p0 == p1 = return (reverse (p1:path))
  | otherwise = msum $ map (\p -> dfs_aux gr p p1 (p0:path)) [ x | x <- find_next p0 gr, not (x `elem` path) ]


-- breadth first search
bfs :: MonadPlus m => Graph -> From -> To -> m [Int]
bfs gr p0 p1 = bfs_aux gr p1 [ x:[p0] | x <- find_next p0 gr]

type Queue = [[Int]]

bfs_aux :: MonadPlus m => Graph -> To -> Queue -> m [Int]
bfs_aux _ _ [] = fail "no path"
bfs_aux gr p1 (path:queue)
  | p0 == p1 = (return (reverse path)) `mplus` (bfs_aux gr p1 queue)
  | otherwise = bfs_aux gr p1 (queue ++ [ x:path | x <- find_next p0 gr, not (x `elem` path)])
  where p0 = head path

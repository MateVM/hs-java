
module Java.ClassPath.Common where

import Data.List
import Data.String.Utils (split)

import Java.ClassPath.Types

mapF ::  (t -> a) -> [Tree t] -> [Tree a]
mapF fn forest = map (mapT fn) forest

mapFM fn forest = mapM (mapTM fn) forest

mapTM fn (Directory dir forest) = Directory dir `fmap` mapFM fn forest
mapTM fn (File a) = File `fmap` fn a

mapT ::  (t -> a) -> Tree t -> Tree a
mapT fn (Directory dir forest) = Directory dir (mapF fn forest)
mapT fn (File a) = File (fn a)

buildTree :: [FilePath] -> [Tree FilePath]
buildTree strs =
  let build :: [[String]] -> [Tree FilePath]
      build [[name]] = [File name]
      build ss = map node $ groupBy eq (sort ss)

      node [] = error "Impossible: groupBy give an empty group!"
      node ([]:l) = node l
      node l | all (null . tail) l = File (head $ head l)
             | otherwise           = Directory (head $ head l) (build $ map tail l)

      ls = map (split "/") strs

      eq [] []       = True
      eq (x:_) (y:_) = x == y

  in  build ls

merge :: [Tree CPEntry] -> [Tree CPEntry]
merge [] = []
merge [t1,t2] = merge1 [t1] t2
merge (t:ts) = foldl merge1 [t] ts
  
merge1 :: [Tree CPEntry] -> Tree CPEntry -> [Tree CPEntry]
merge1 [] x = [x]
merge1 (x@(File e): es) y@(File e') | e == e'   = x: es
                                    | otherwise = x: merge1 es y
merge1 (d@(Directory _ _):es) f@(File _) = d: merge1 es f
merge1 (x@(Directory dir f):es) y@(Directory dir' f')
  | dir == dir' = Directory dir (merge $ f ++ f'): es 
  | otherwise   = x: merge1 es y


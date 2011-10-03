
module Java.ClassPath where

import Control.Monad
import Control.Monad.State
import System.Directory
import System.FilePath
import System.FilePath.Glob
import Data.Function (on)
import Data.List
import Data.String.Utils (split)

import JVM.ClassFile
import JVM.Converter

data Tree a =
    Directory FilePath [Tree a]
  | File a
  deriving (Eq)

instance Show a => Show (Tree a) where
  show (Directory dir forest) = dir ++ "/{" ++ intercalate ", " (map show forest) ++ "}"
  show (File a) = show a

data CPEntry =
    NotLoaded FilePath
  | Loaded FilePath (Class Direct)
  | NotLoadedJAR FilePath FilePath
  | LoadedJAR FilePath (Class Direct)
  deriving (Eq)

instance Show CPEntry where
  show (NotLoaded path) = "<Not loaded file: " ++ path ++ ">"
  show (Loaded path cls) = "<Loaded from " ++ path ++ ": " ++ toString (thisClass cls) ++ ">"
  show (NotLoadedJAR jar path) = "<Not loaded JAR: " ++ jar ++ ": " ++ path ++ ">"
  show (LoadedJAR path cls) = "<Read JAR: " ++ path ++ ": " ++ toString (thisClass cls) ++ ">"

type ClassPath a = StateT [Tree CPEntry] IO a

-- | For given list of glob masks, return list of matching files
glob :: FilePath -> [FilePath] -> IO [FilePath]
glob dir patterns = do
  (matches, _) <- globDir (map compile patterns) dir
  return $ concat matches

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

appendPath :: FilePath -> [Tree CPEntry] -> [Tree CPEntry]
appendPath path forest = merge $ forest ++ (mapF NotLoaded $ buildTree [path])

merge :: [Tree CPEntry] -> [Tree CPEntry]
merge [] = []
merge (t:ts) = foldl merge1 [t] ts
  
merge1 :: [Tree CPEntry] -> Tree CPEntry -> [Tree CPEntry]
merge1 [] x = [x]
merge1 (x@(File e): es) y@(File e') | e == e'   = x: es
                                    | otherwise = x: merge1 es y
merge1 (d@(Directory _ _):es) f@(File _) = d: merge1 es f
merge1 (x@(Directory dir f):es) y@(Directory dir' f')
  | dir == dir' = Directory dir (merge $ f ++ f'): es 
  | otherwise   = x: merge1 es y

addDirectory :: FilePath -> ClassPath ()
addDirectory dir = do
  files <- liftIO $ glob dir ["*.class"]
  cp <- get
  let cp' = foldr appendPath cp files
  put cp'

runClassPath :: ClassPath () -> IO [Tree CPEntry]
runClassPath m = execStateT m []


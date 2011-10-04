
module Java.ClassPath.Types where

import Control.Monad.State
import Data.List

import JVM.ClassFile

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


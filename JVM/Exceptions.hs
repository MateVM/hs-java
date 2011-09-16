{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}
module JVM.Exceptions where

import Control.Monad.Exception
import qualified Data.ByteString.Lazy as B

import JVM.Types

data NoItemInPool = forall a. Show a => NoItemInPool a
  deriving (Typeable)

instance Exception NoItemInPool

instance Show NoItemInPool where
  show (NoItemInPool s) = "Internal error: no such item in pool: <" ++ show s ++ ">"

force :: String -> EM AnyException a -> a
force s x =
  case tryEM x of
    Right result -> result
    Left  exc    -> error $ "Exception at " ++ s ++ ": " ++ show exc

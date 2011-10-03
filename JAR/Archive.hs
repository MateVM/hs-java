
module JAR.Archive where

import Control.Monad.Trans
import qualified Control.Monad.State as St
import qualified Codec.Archive.LibZip as Zip
import Data.Binary
import Data.String.Utils (split)
import qualified Data.ByteString.Lazy as B

import Java.ClassPath
import JVM.ClassFile
import JVM.Converter

readJAR :: FilePath -> IO [Tree CPEntry]
readJAR jarfile = do
  files <- Zip.withArchive [] jarfile $ Zip.fileNames []
  return $ mapF (NotLoadedJAR jarfile) (buildTree files)

readFromJAR :: FilePath -> FilePath -> IO (Class Direct)
readFromJAR jarfile path = do
  content <- Zip.withArchive [] jarfile $ Zip.fileContents [] path
  let bstr = B.pack content
  return $ classFile2Direct (decode bstr)

addJAR :: FilePath -> ClassPath ()
addJAR jarfile = do
  classes <- liftIO $ readJAR jarfile
  cp <- St.get
  let cp' = merge $ cp ++ classes
  St.put cp'

loadClass :: String -> ClassPath ()
loadClass path = do
    cp <- St.get
    cp' <- liftIO $ mapM (load xs) cp
    St.put cp'
  where
    xs = split "/" path

    load :: [String] -> Tree CPEntry -> IO (Tree CPEntry)
    load [] t = return t
    load (p:ps) t@(Directory dir forest)
      | p == dir  = Directory dir `fmap` mapM (load ps) forest
      | otherwise = return t
    load [p] t@(File (NotLoaded f))
      | (p ++ ".class") == f = do
                               cls <- parseClassFile (path ++ ".class")
                               return (File $ Loaded path cls)
      | otherwise = return t
    load [p] t@(File (NotLoadedJAR jarfile f))
      | (p ++ ".class") == f = do
                               cls <- readFromJAR jarfile (path ++ ".class")
                               return (File $ LoadedJAR jarfile cls)
      | otherwise = return t
    load ps (File _) = fail $ "Found file when expecting directory! " ++ show ps


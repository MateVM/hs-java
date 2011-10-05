-- | This module defines functions to read Java JAR files.
module Java.JAR.Archive where

import qualified Codec.Archive.LibZip as Zip
import Data.Binary
import Data.List
import qualified Data.ByteString.Lazy as B

import Java.ClassPath.Types
import Java.ClassPath.Common
import JVM.ClassFile
import JVM.Converter

readJAREntry :: (Enum a) => FilePath -> String -> IO (Maybe [a])
readJAREntry jarfile path = do
  Zip.catchZipError (Just `fmap` (Zip.withArchive [] jarfile $ Zip.fileContents [] path))
                    (\_ -> return Nothing)

-- | Read all entires from JAR file
readAllJAR :: FilePath -> IO [Tree CPEntry]
readAllJAR jarfile = do
    files <- Zip.withArchive [] jarfile $ Zip.fileNames []
    return $ mapF (NotLoadedJAR jarfile) (buildTree $ filter good files)
  where
    good file = ".class" `isSuffixOf` file

-- | Read one class from JAR file
readFromJAR :: FilePath -> FilePath -> IO (Class Direct)
readFromJAR jarfile path = do
  content <- Zip.withArchive [] jarfile $ Zip.fileContents [] path
  let bstr = B.pack content
  return $ classFile2Direct (decode bstr)



module Java.JAR.Archive where

import Control.Monad.Trans
import qualified Control.Monad.State as St
import qualified Codec.Archive.LibZip as Zip
import Data.Binary
import qualified Data.ByteString.Lazy as B

import Java.ClassPath.Types
import Java.ClassPath.Common
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


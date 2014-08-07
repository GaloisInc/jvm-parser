{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Language.JVM.JarReader
Copyright   : Galois, Inc. 2012-2014
License     : BSD3
Maintainer  : atomb@galois.com
Stability   : stable
Portability : non-portable

A quick-n-dirty reader for JAR files.  MANY keep-it-simple concessions have
been made regarding the zip structure of jars (e.g. no ZIP64, Deflate-type
compression only, etc., etc.).  Please don't mistake this module for any kind
of fully-fledged unzip implementation!

Info on the zip file format can be found at:
<http://www.pkware.com/documents/casestudies/APPNOTE.TXT>
-}
module Language.JVM.JarReader
  ( JarReader(..)
  , addJar
  , dumpJarReader
  , loadClassFromJar
  , newJarReader
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy       (ByteString, isSuffixOf, hGet)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Map                   (Map)
import Data.Word
import System.IO

import qualified Codec.Compression.Zlib.Raw as Zlib
import qualified Control.Exception          as CE
import qualified Data.List                  as L
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Map                   as M

import Language.JVM.Parser

-- import Debug.Trace

-- | Datatype representing a collection of .jar archives.
--   Classfiles can be loaded from a JarReader using 'loadClassFromJar'.
newtype JarReader = JR
  { unJR :: Map ByteString (FilePath, DirEnt) }
  deriving (Show)

-- | Print all the directory entries known to this JarReader
--   onto the console
dumpJarReader :: JarReader -> IO ()
dumpJarReader jr = mapM_ putStrLn (map unpack . M.keys $ unJR jr)

emptyJarReader :: JarReader
emptyJarReader = JR (M.empty)

-- | Load a class from the given JarReader.
loadClassFromJar
   :: String          -- ^ Class file name to load
   -> JarReader
   -> IO (Maybe Class)
loadClassFromJar clNm jr = do
  let k = pack $ clNm ++ (if ".class" `L.isSuffixOf` clNm then "" else ".class")
  case M.lookup k (unJR jr) of
    Nothing            -> return Nothing
    Just (jarFn, dent) -> withBinaryFile jarFn ReadMode $ \h -> do
      let parseBytes (n::Int) p = runGet p <$> hGet h (fromIntegral n)
      hSeek h AbsoluteSeek $ fromIntegral $ dentRelOff dent

      sig <- parseBytes 4 getWord32le
      CE.assert (sig == lclHdrSig) $ return ()

      -- Skip header bytes (26 bytes of redundant/irrelevant/unused data), plus
      -- the variable-size fields (filename and "extra")
      hSeek h RelativeSeek $ fromIntegral $
        26 + dentFilenameLen dent + dentXtraFldLen dent

      let uncompSz = fromIntegral $ dentUncompSz dent
          compSz   = fromIntegral $ dentCompSz dent

      bytes <- case dentCompMethod dent of
                 0x0 -> hGet h uncompSz
                 0x8 -> Zlib.decompress <$> hGet h compSz
                 _   -> compTypeNotSupported
      CE.assert (LBS.length bytes == fromIntegral uncompSz) $ return ()
      let cl = runGet getClass bytes
      cl `seq` return (Just cl)

-- | Create a new `JarReader` from the given collection of .jar archives
newJarReader :: [FilePath] -> IO JarReader
newJarReader = foldM addJar emptyJarReader

-- | Add a .jar archive to a `JarReader`
addJar :: JarReader -> FilePath -> IO JarReader
addJar jr fn = do
  h <- openBinaryFile fn ReadMode
  let
    parseBytes n p        = runGet p <$> hGet h (fromIntegral n)
    seekFromEnd           = hSeek h SeekFromEnd . negate {- Grr! -} . fromIntegral
    seekFromStart         = hSeek h AbsoluteSeek . fromIntegral
    findDirEnd (off::Int) = do
      seekFromEnd off
      s <- parseBytes (4::Int) getWord32le
      if s == dirEndSig then return off else findDirEnd (off + 1)

  seekFromEnd =<< findDirEnd 4
  sz <- hFileSize h
  dend <- runGet dirEnd <$> hGet h (fromIntegral sz)

  -- no support for a "multidisk" archive
  CE.assert (dendDiskEntryCnt dend == dendEntryCnt dend) $ return ()
  CE.assert (dendDiskNum dend == dendStartDiskNum dend) $ return ()

  seekFromStart (dendDirStartOff dend)
  JR
    . (`M.union` unJR jr) -- left biased: use newer jar's value on collision
    . M.fromList
    . map ((dentFilename &&& (,) fn) . checkBitFlagAndSizes)
    . filter (isSuffixOf (pack ".class") . dentFilename)
    <$> let numHdrs = fromIntegral (dendEntryCnt dend) in do
        parseBytes (dendDirSize dend) (replicateM numHdrs dirEnt)

-- | NB: We expect that the given class files do not have the (un)compressed
-- sizes stored in the data descriptor segment.
checkBitFlagAndSizes :: DirEnt -> DirEnt
checkBitFlagAndSizes d =
  if (dentBitFlag d .&. 0x8 == 0
      || (dentCompSz d > 0 && dentUncompSz d > 0))
    then d
    else ddSizesNotSupported

-- A central directory entry
data DirEnt = DirEnt
  { dentBitFlag     :: !Word16
  , dentCompMethod  :: !Word16
  , dentCompSz      :: !Word32
  , dentUncompSz    :: !Word32
  , dentRelOff      :: !Word32
  , dentFilenameLen :: !Word16
  , dentXtraFldLen  :: !Word16
  , dentFilename    :: !ByteString
  }
  deriving (Show)

dirEnt :: Get DirEnt
dirEnt = do
  sig <- getWord32le
  CE.assert (sig == dirEntSig) $ return ()
  skip 2 -- version made by
  skip 2 -- version needed to extract
  bitFlag  <- getWord16le
  compMeth <- getWord16le
  -- "deflate" type compression or uncompressed only
  when (compMeth /= 0x0 && compMeth /= 0x8) compTypeNotSupported
  skip 2 -- last mod file time
  skip 2 -- last mod file date
  skip 4 -- crc-32
  compSz   <- getWord32le
  uncompSz <- getWord32le
  fnameLen <- getWord16le
  xfldLen  <- getWord16le
  cmntLen  <- fromIntegral <$> getWord16le
  skip 2 -- disk number start
  skip 2 -- internal file attributes
  skip 4 -- external file attributes
  relOff <- getWord32le
  fname  <- getLazyByteString (fromIntegral fnameLen)
--   when (bitFlag .&. 0x8 /= 0 && (uncompSz == 0 || compSz == 0)) $ do
--      trace ("viol: " ++ show fname ++ ", uncompSz = " ++ show uncompSz) $ return ()
  skip (fromIntegral xfldLen) -- extra field
  skip cmntLen -- file comment

  return $ DirEnt bitFlag compMeth compSz uncompSz relOff fnameLen xfldLen fname

-- | The trailer of the central directory section
data DirEnd = DirEnd
  { dendDiskNum      :: !Word16
  , dendStartDiskNum :: !Word16
  , dendDiskEntryCnt :: !Word16
  , dendEntryCnt     :: !Word16
  , dendDirSize      :: !Word32
  , dendDirStartOff  :: !Word32
  , _dendComment      :: !(Maybe ByteString)
  }
  deriving (Show)

dirEnd :: Get DirEnd
dirEnd = do
  sig <- getWord32le
  CE.assert (sig == dirEndSig) $ return ()
  DirEnd
    <$> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord16le
    <*> getWord32le
    <*> getWord32le
    <*> do clen <- getWord16le
           if clen > 0
             then Just <$> getRemainingLazyByteString
             else return Nothing

dirEndSig :: Word32
dirEndSig = 0x06054b50

dirEntSig :: Word32
dirEntSig = 0x02014b50

lclHdrSig :: Word32
lclHdrSig = 0x04034b50

compTypeNotSupported :: a
compTypeNotSupported =
  error $ "JAR processing error: data descriptor sizes encoding unsupported"

ddSizesNotSupported :: a
ddSizesNotSupported =
  error $ "JAR processing error: data descriptor sizes encoding unsupported"

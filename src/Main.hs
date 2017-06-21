module Main where

import qualified Data.ByteString     as Bs
import           Data.Maybe
import           Data.Monoid         ((<>))
import           Data.Word           (Word8)
import           Options.Applicative
import           Text.Printf
import System.IO
import System.Exit

data Sample = Sample
  { patchName :: String
  , dstName   :: String
  , offset    :: Maybe Int
  , tgtSize   :: Maybe Int
  , fillVal   :: Maybe Word8}


offs:: Parser (Maybe Int)
offs= optional $ option auto
            ( long "offset"
           <> short 'o'
           <> metavar "INT"
           <> help "Offset to write patch to" )

size :: Parser (Maybe Int)
size= optional $ option auto
            ( long "size"
           <> short 's'
           <> metavar "INT"
           <> help "Abort, if patch size is larger than this size" )
fill :: Parser (Maybe Word8)
fill = optional $ option auto
            ( long "fill"
           <> short 'f'
           <> metavar "BYTE"
           <> help "Align up to target size with this byte" )

sample :: Parser Sample
sample = Sample
     <$> argument str
          ( metavar "FILE"
         <> help "Patch file name" )
     <*> argument str
          ( metavar "FILE"
         <> help "Destination file name" )
     <*> offs <*> size <*> fill

patch :: Bs.ByteString -> Bs.ByteString -> Int -> String -> String -> IO()
patch patch dst offs patchName dstName = do

  let
    start = Bs.take offs dst
    end = Bs.drop  (offs+Bs.length patch) dst
  Bs.writeFile dstName $ Bs.concat [start, patch, end]
  putStrLn $ printf "%s successfully written at 0x%X" patchName offs


{--
Insert binary file into another file at given offset

Usage:
insertBin patch.bin target.bin [-o offset][-s size] [-f fillValue]

offset - default 0
size - target size of patch, if patch.bin is greater, insertBin returns error, if greater allign to size with fillValue
fillValue - if patch is greater, aligns patch.bin with given value. Default 0. If size is not specified, don't align
--}
main :: IO ()
main = do
  let
    opts = info (helper <*> sample) (fullDesc
     <> progDesc "Write binary file over in another file at given offset"
     <> header "hInsertBin - Simple binary patching utility v 0.1" )
  (Sample patchName dstName offs tgtSize fill)  <- execParser opts
  patchFile <- Bs.readFile patchName
  dstFile <- Bs.readFile dstName
  let
    o = fromMaybe 0 offs
    f = fromMaybe 0 fill
    ts = fromJust tgtSize
    dummyLen = ts - Bs.length patchFile
    dummy = Bs.replicate dummyLen f
    patchLen = Bs.length patchFile
  if isNothing tgtSize
    then patch patchFile dstFile o patchName dstName
    else if patchLen > ts
      then do
        hPutStrLn stderr $ printf "%s size 0x%X > specified 0x%X. ABORTED!" patchName patchLen ts
        exitFailure
      else do
        patch (patchFile `Bs.append` dummy) dstFile o patchName dstName
        putStrLn $ printf "0x%X bytes are free" dummyLen

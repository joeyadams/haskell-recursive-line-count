{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module CountLines (
    countLines,
    Entry(..),
    EntryType(..),
) where

import Prelude hiding (catch)

import Control.Arrow        ((>>>))
import Control.Exception
import Control.Monad.Reader
import Data.Int             (Int64)
import Data.Maybe           (catMaybes)
import Data.Tree
import GHC.Exts             (groupWith)
import System.FilePath
import System.IO

import qualified Data.ByteString.Lazy as L

data Entry
    = Entry
        { entryType         :: EntryType
        , entryName         :: String
        , entryPath         :: FilePath
        , entryLineCount    :: !Int64
        }

instance Show Entry where
    showsPrec d Entry{..}
        = showParen (d > 10)
        $ showString "Entry "
        . showsPrec 11 entryType
        . showString " "
        . showsPrec 11 entryName
        . showString " "
        . showsPrec 11 entryLineCount

data EntryType = File | Directory
    deriving (Eq, Show)

logError :: String -> IO ()
logError = hPutStrLn stderr

logIOError :: IOError -> IO ()
logIOError = logError . show

-- | File path separated into entry names using 'splitDirectories'.
type Path = [FileName]

type FileName = FilePath

-- | Reader monad with context that prepends parent elements to a path,
--   thus indicating the current directory of our traversal.
type CountLinesM = ReaderT (Path -> Path) IO

countLines :: [FilePath] -> IO (Forest Entry, Int64)
countLines = map splitDirectories
         >>> filter (not . null)
         >>> countLinesForest
         >>> (`runReaderT` id)

getFullPath :: Path -> CountLinesM FilePath
getFullPath path = fmap (\f -> joinPath $ f path) ask

-- | Count lines in a single file.
countLinesFile :: FileName -> CountLinesM (Maybe Entry)
countLinesFile name = do
    full_path <- getFullPath [name]
    liftIO $ (fmap (Just . Entry File name full_path) $
              L.readFile full_path >>= evaluate . L.count 10)
           `catch` \e -> do
                logIOError e
                return Nothing

-- | None of the paths given may be empty.
countLinesForest :: [Path] -> CountLinesM (Forest Entry, Int64)
countLinesForest paths = do
    forest <- fmap catMaybes $
              forM (groupWith head paths) $ \g ->
                  -- g :: [Path] is a list of paths that all
                  -- have the same root FileName.
                  countLinesTree (head $ head g) (map tail g)
    let !total = sum $ map (entryLineCount . rootLabel) forest
    return (forest, total)

countLinesTree :: FileName -> [Path] -> CountLinesM (Maybe (Tree Entry))
countLinesTree _        []      = error "countLinesTree: Empty path list"
countLinesTree filename [[]]    = fmap (fmap (\entry -> Node entry []))
                                $ countLinesFile filename
countLinesTree dirname children = do
    (child_nodes, total) <- local (. (dirname :))
                          $ countLinesForest children
    dirpath <- getFullPath [dirname]
    return $ Just $ Node (Entry Directory dirname dirpath total) child_nodes

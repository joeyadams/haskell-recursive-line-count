{-# LANGUAGE BangPatterns #-}
import Prelude hiding (catch)

import Control.Arrow        ((>>>))
import Control.Exception
import Control.Monad.Reader
import Data.Maybe           (catMaybes)
import Data.Tree
import GHC.Exts             (groupWith)
import System.FilePath
import System.IO

data Entry = Entry EntryType FilePath !Int
    deriving Show

data EntryType = File | Directory
    deriving Show

entryLineCount :: Entry -> Int
entryLineCount (Entry _ _ n) = n

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

getFullPath :: Path -> CountLinesM FilePath
getFullPath path = fmap (\f -> joinPath $ f path) ask

-- | Count lines in a single file.
countLines :: FileName -> CountLinesM (Maybe Entry)
countLines name = do
    full_path <- getFullPath [name]
    liftIO $ (fmap (Just . Entry File name) $
              readFile full_path >>= evaluate . length . lines)
           `catch` \e -> do
                logIOError e
                return Nothing

-- | None of the paths given may be empty.
countLinesForest :: [Path] -> CountLinesM (Forest Entry)
countLinesForest paths =
    fmap catMaybes $
    forM (groupWith head paths) $ \g ->
        -- g :: [Path] is a list of paths that all have the same root FileName.
        countLinesTree (head $ head g) (map tail g)

countLinesTree :: FileName -> [Path] -> CountLinesM (Maybe (Tree Entry))
countLinesTree _        []   = error "countLinesTree: Empty path list"
countLinesTree filename [[]] = fmap (fmap (\entry -> Node entry []))
                             $ countLines filename
countLinesTree dirname children = do
    child_nodes <- local (. (dirname :)) $ countLinesForest children
    let !total = sum $ map (entryLineCount . rootLabel) child_nodes
    return $ Just $ Node (Entry Directory dirname total) child_nodes

main :: IO ()
main = do
    paths <- fmap (lines >>> map splitDirectories >>> filter (not . null))
           $ getContents
    forest <- runReaderT (countLinesForest paths) id
    putStr $ drawForest $ map (fmap show) forest
{-
    countLines "/home/joey" >>= print
    countLines "Main.hs" >>= print
    countLines "" >>= print
    countLines "a" >>= print
-}

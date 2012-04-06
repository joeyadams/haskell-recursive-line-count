{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Control.Monad        (forM_)
import CountLines
import Data.Int             (Int64)
import Data.List            (sortBy)
import Data.Function        (on)
import Data.Tree
import Graphics.UI.Gtk
    hiding (on)
import System.IO            (hClose)
import System.Process

import qualified Graphics.UI.Gtk as Gtk

editFile :: FilePath -> Maybe Int64 -> IO ()
editFile path lineno = do
    let args = case lineno of
                   Nothing -> []
                   Just n  -> ['+' : show n]
    (Just stdin, Just stdout, Just stderr, _) <-
        createProcess (proc "gvim" (path : args))
            { std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    mapM_ hClose [stdin, stdout, stderr]

sortTree :: (a -> a -> Ordering) -> Tree a -> Tree a
sortTree cmp (Node root forest) = Node root (sortForest cmp forest)

sortForest :: (a -> a -> Ordering) -> Forest a -> Forest a
sortForest cmp xs = map (sortTree cmp) $ sortBy (cmp `on` rootLabel) xs

data BottomRowHandlers
    = BottomRowHandlers
        { onRandomLine :: IO ()
        }

createBottomRow :: BottomRowHandlers -> IO Alignment
createBottomRow BottomRowHandlers{..} = do
    randomLineButton <- buttonNewWithLabel "Jump to random line"
    onClicked randomLineButton onRandomLine

    align <- alignmentNew 0.5 0.0 0.0 0.0
    containerAdd align randomLineButton

    return align

main :: IO ()
main = do
    (forest_by_name, total) <- fmap lines getContents >>= countLines
    let forest = sortForest (flip compare `on` entryLineCount) forest_by_name

    -- Display the tree on the console
    --      putStr $ drawForest $ map (fmap show) forest
    --      putStrLn $ "Total line count: " ++ show total

    -- The following is based on TreeDemo.hs in the
    -- Gtk2Hs source distribution.

    initGUI

    win <- windowNew
    windowSetDefaultSize win 500 500
    onDestroy win mainQuit

    vbox <- vBoxNew False 0
    label <- labelNew $ Just $ "Total line count: " ++ show total
    miscSetAlignment label 0.0 0.5
    miscSetPadding label 5 0
    boxPackStart vbox label PackNatural 5
    containerAdd win vbox

    model <- treeStoreNew forest
    view  <- treeViewNewWithModel model

    treeViewSetHeadersVisible view True

    colFileName      <- treeViewColumnNew
    colNumberOfLines <- treeViewColumnNew

    forM_ [colFileName, colNumberOfLines] $ \col -> do
        treeViewColumnSetResizable  col True
        treeViewColumnSetExpand     col True

    treeViewColumnSetTitle colFileName      "Name"
    treeViewColumnSetTitle colNumberOfLines "Line count"

    rendererFileName      <- cellRendererTextNew
    rendererNumberOfLines <- cellRendererTextNew

    cellLayoutPackStart colFileName         rendererFileName        True
    cellLayoutPackStart colNumberOfLines    rendererNumberOfLines   True

    cellLayoutSetAttributes colFileName rendererFileName model $ \Entry{..} ->
        [ cellText := entryName ]
    cellLayoutSetAttributes colNumberOfLines rendererNumberOfLines model $ \Entry{..} ->
        [ cellText := show entryLineCount ]

    treeViewAppendColumn view colFileName
    treeViewAppendColumn view colNumberOfLines

    Gtk.on view rowActivated $ \path _ -> do
        Entry{..} <- treeStoreGetValue model path
        if entryType == File
            then editFile entryPath Nothing
            else do
                e <- treeViewRowExpanded view path
                _ <- if e
                    then treeViewCollapseRow view path
                    else treeViewExpandRow view path False
                return ()

    scroll <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic

    containerAdd scroll view
    boxPackStart vbox scroll PackGrow 0

    bottomRow <- createBottomRow BottomRowHandlers
        { onRandomLine = do
            dialog <- messageDialogNew (Just win)
                                       [DialogDestroyWithParent]
                                       MessageError
                                       ButtonsOk
                                       "Not implemented yet"
            _ <- dialogRun dialog
            widgetDestroy dialog
            return ()
        }
    boxPackStart vbox bottomRow PackNatural 0

    widgetShowAll win
    mainGUI

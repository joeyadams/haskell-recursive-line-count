{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Control.Monad        (forM_)
import CountLines
import Graphics.UI.Gtk
import System.IO            (hClose)
import System.Process

editFile :: FilePath -> IO ()
editFile path = do
    (Just stdin, Just stdout, Just stderr, _) <-
        createProcess (proc "gvim" [path])
            { std_in  = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    mapM_ hClose [stdin, stdout, stderr]

main :: IO ()
main = do
    (forest, total) <- fmap lines getContents >>= countLines

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

    on view rowActivated $ \path _ -> do
        Entry{..} <- treeStoreGetValue model path
        if entryType == File
            then editFile entryPath
            else do
                e <- treeViewRowExpanded view path
                _ <- if e
                    then treeViewCollapseRow view path
                    else treeViewExpandRow view path False
                return ()

    scroll <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic

    containerAdd scroll view
    containerAdd vbox scroll

    widgetShowAll win
    mainGUI

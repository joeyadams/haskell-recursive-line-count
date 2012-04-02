{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Control.Monad    (forM_, when)
import CountLines
import Graphics.UI.Gtk

editFile :: FilePath -> IO ()
editFile path = print path

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

    model <- treeStoreNew forest
    view  <- treeViewNewWithModel model

    treeViewSetHeadersVisible view True

    colFileName      <- treeViewColumnNew
    colNumberOfLines <- treeViewColumnNew

    forM_ [colFileName, colNumberOfLines] $ \col -> do
        treeViewColumnSetResizable  col True
        treeViewColumnSetExpand     col True

    treeViewColumnSetTitle colFileName      "Name"
    treeViewColumnSetTitle colNumberOfLines "# Lines"

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
        when (entryType == File) $
            editFile entryPath

    scroll <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic

    containerAdd scroll view
    containerAdd win scroll

    widgetShowAll win
    mainGUI

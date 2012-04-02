{-# LANGUAGE RecordWildCards #-}
import CountLines
import Graphics.UI.Gtk

main :: IO ()
main = do
    (forest, total) <- fmap lines getContents >>= countLines

    -- Display the tree on the console
    --      putStr $ drawForest $ map (fmap show) forest
    --      putStrLn $ "Total line count: " ++ show total

    -- The following is based on TreeDemo.hs in the
    -- Gtk2Hs source distribution.

    _ <- initGUI

    win <- windowNew
    _ <- onDestroy win mainQuit

    model <- treeStoreNew forest
    view  <- treeViewNewWithModel model

    treeViewSetHeadersVisible view True

    colFileName      <- treeViewColumnNew
    colNumberOfLines <- treeViewColumnNew

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

    _ <- treeViewAppendColumn view colFileName
    _ <- treeViewAppendColumn view colNumberOfLines

    containerAdd win view

    widgetShowAll win
    mainGUI

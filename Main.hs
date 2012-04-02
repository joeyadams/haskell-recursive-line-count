import CountLines
import Data.Tree

main :: IO ()
main = do
    (forest, total) <- fmap lines getContents >>= countLines
    putStr $ drawForest $ map (fmap show) forest
    putStrLn $ "Total line count: " ++ show total

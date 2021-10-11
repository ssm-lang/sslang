import           Front.Ast                      ( Program )
import           Front.Parser                   ( parse )
import           Front.Scanner                  ( runAlex )

parseStr :: String -> Either String Program
parseStr s = runAlex s parse

main :: IO ()
main = do
  putStrLn "Parser test"
  print $ parseStr $ unlines
    [ "main(clk : Ref Int) ="
    , "  loop"
    , "    wait clk"
    , "    wait clk"
    , "    wait clk"
    ]

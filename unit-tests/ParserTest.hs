module Main where

import Front.Scanner
import Front.Parser
import Front.Ast

parseStr :: String -> Either String Program
parseStr s = runAlex s parse

main :: IO ()
main = do putStrLn "Parser test"
          print $ parseStr $ unlines ["main(clk : Ref Sched Int) ="
                                     ,"  loop"
                                     ,"    wait clk"
                                     ,"    wait clk"]

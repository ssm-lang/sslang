module Main where

import Front.Scanner ( Token(..), TokenType(..), alexMonadScan, runAlex,
                       AlexPosn(..) )
import Duration       

import Data.List ( intercalate )

showTokens :: [Token] -> String
showTokens tks = "[" ++ intercalate "\n," (map show tks) ++ "]"

scan :: String -> [Token]
scan s = case runAlex s tokens of
  Right tokens -> tokens
  Left s -> error $ "Scanner failed " ++ s
  where
    tokens = do tok <- alexMonadScan
                case tok of Token _ TEOF -> return []
                            _            -> (:) <$> pure tok <*> tokens

printTokens :: String -> IO ()
printTokens s = do putStrLn "Input:"
                   putStrLn s
                   putStrLn "Tokens:"
                   mapM_ (putStrLn . show) $ scan s

checkTokens :: [Token] -> String -> IO ()
checkTokens expected s = do putStrLn "Input:"
                            putStrLn s
                            let tokens = scan s
                            if tokens == expected then putStrLn "OK"
                            else do putStrLn "FAILED"
                                    putStrLn "Expected:"
                                    putStrLn $ showTokens expected
                                    putStrLn "Got:"
                                    putStrLn $ showTokens tokens
                            
main :: IO ()
main = do putStrLn "Scanner test"

          checkTokens [Token (AlexPn 0 1 1) (TInteger 123)]
            "123"
            
          checkTokens [Token (AlexPn 1 1 2) (TInteger 123)] 
            " 123 "
            
          checkTokens [Token (AlexPn 1 1 2) (TInteger 123)
                      ,Token (AlexPn 5 1 6) (TInteger 42)]
            " 123 42"
            
          checkTokens [Token (AlexPn 0 1 1) TLet
                      ,Token (AlexPn 4 1 5) TLbrace
                      ,Token (AlexPn 4 1 5) (TId "a")
                      ,Token (AlexPn 6 1 7) TEq
                      ,Token (AlexPn 8 1 9) TLbrace
                      ,Token (AlexPn 8 1 9) (TInteger 42)
                      ,Token (AlexPn 35 2 5) TRbrace
                      ,Token (AlexPn 35 2 5) TAnd
                      ,Token (AlexPn 35 2 5) (TId "b")
                      ,Token (AlexPn 37 2 7) TEq
                      ,Token (AlexPn 39 2 9) TLbrace
                      ,Token (AlexPn 39 2 9) (TInteger 31)
                      ,Token (AlexPn 73 3 9) TSemicolon
                      ,Token (AlexPn 73 3 9) (TInteger 245)
                      ,Token (AlexPn 77 3 13) (TInteger 42)
                      ,Token (AlexPn 109 4 10) (TId "contin")
                      ,Token (AlexPn 136 5 1) TRbrace
                      ,Token (AlexPn 136 5 1) TRbrace
                      ,Token (AlexPn 136 5 1) TSemicolon
                      ,Token (AlexPn 136 5 1) (TId "another")
                      ,Token (AlexPn 144 5 9) (TInteger 42)
                      ,Token (AlexPn 149 6 3) (TId "lineContinued")
                      ,Token (AlexPn 163 6 17) (TInteger 82)
                      ,Token (AlexPn 166 7 1) TSemicolon
                      ,Token (AlexPn 166 7 1) TWhile
                      ,Token (AlexPn 172 7 7) (TInteger 1)
                      ,Token (AlexPn 174 7 9) (TInteger 2)
                      ,Token (AlexPn 176 7 11) (TInteger 3)
                      ,Token (AlexPn 177 7 12) TDo
                      ,Token (AlexPn 179 8 2) TLbrace
                      ,Token (AlexPn 179 8 2) (TId "block")
                      ,Token (AlexPn 185 8 8) (TInteger 3)
                      ,Token (AlexPn 190 9 4) (TInteger 4)
                      ,Token (AlexPn 192 9 6) (TInteger 3)
                      ,Token (AlexPn 195 10 2) TSemicolon
                      ,Token (AlexPn 195 10 2) (TId "line")
                      ,Token (AlexPn 200 10 7) (TInteger 4)
                      ,Token (AlexPn 202 11 1) TRbrace
                      ,Token (AlexPn 202 11 1) TSemicolon
                      ,Token (AlexPn 202 11 1) (TId "nothing")]
            $ unlines ["let a = 42 // enter two blocks"
                      ,"    b = 31 // 31 a line by itself"
                      ,"        245 42 // 245 on next line"
                      ,"         contin // (line continued)"
                      ,"another 42"
                      ,"  lineContinued 82"
                      ,"while 1 2 3"
                      ," block 3"
                      ,"   4 3"
                      ," line 4"
                      ,"nothing"]
            
          checkTokens [Token (AlexPn 0 1 1) TLet
                      ,Token (AlexPn 4 1 5) TLbrace
                      ,Token (AlexPn 4 1 5) (TId "a")
                      ,Token (AlexPn 6 1 7) TEq
                      ,Token (AlexPn 8 1 9) TLbrace
                      ,Token (AlexPn 8 1 9) TLet
                      ,Token (AlexPn 12 1 13) TLbrace
                      ,Token (AlexPn 12 1 13) (TId "b")
                      ,Token (AlexPn 14 1 15) TEq
                      ,Token (AlexPn 16 1 17) TLbrace
                      ,Token (AlexPn 16 1 17) TLet
                      ,Token (AlexPn 20 1 21) TLbrace
                      ,Token (AlexPn 20 1 21) (TId "c")
                      ,Token (AlexPn 22 1 23) TEq
                      ,Token (AlexPn 24 1 25) TLbrace
                      ,Token (AlexPn 24 1 25) (TInteger 42)
                      ,Token (AlexPn 26 1 27) TRbrace
                      ,Token (AlexPn 26 1 27) TRbrace
                      ,Token (AlexPn 26 1 27) TRbrace
                      ,Token (AlexPn 26 1 27) TRbrace
                      ,Token (AlexPn 26 1 27) TRbrace
                      ,Token (AlexPn 26 1 27) TRbrace]
                $ "let a = let b = let c = 42"
          checkTokens [Token (AlexPn 0 1 1) TLet
                      ,Token (AlexPn 4 1 5) TLbrace
                      ,Token (AlexPn 4 1 5) (TId "a")
                      ,Token (AlexPn 6 1 7) TEq
                      ,Token (AlexPn 8 1 9) TLbrace
                      ,Token (AlexPn 8 1 9) TLet
                      ,Token (AlexPn 12 1 13) TLbrace
                      ,Token (AlexPn 12 1 13) (TId "b")
                      ,Token (AlexPn 14 1 15) TEq
                      ,Token (AlexPn 16 1 17) TLbrace
                      ,Token (AlexPn 16 1 17) TLet
                      ,Token (AlexPn 20 1 21) TLbrace
                      ,Token (AlexPn 20 1 21) (TId "c")
                      ,Token (AlexPn 22 1 23) TEq
                      ,Token (AlexPn 24 1 25) TLbrace
                      ,Token (AlexPn 24 1 25) (TInteger 42)
                      ,Token (AlexPn 27 2 1) TRbrace
                      ,Token (AlexPn 27 2 1) TRbrace
                      ,Token (AlexPn 27 2 1) TRbrace
                      ,Token (AlexPn 27 2 1) TRbrace
                      ,Token (AlexPn 27 2 1) TRbrace
                      ,Token (AlexPn 27 2 1) TRbrace]
              $ "let a = let b = let c = 42\n"
          checkTokens [Token (AlexPn 0 1 1) TLet
                      ,Token (AlexPn 4 1 5) TLbrace
                      ,Token (AlexPn 4 1 5) (TId "a")
                      ,Token (AlexPn 6 1 7) TEq
                      ,Token (AlexPn 8 1 9) TLbrace
                      ,Token (AlexPn 8 1 9) TLet
                      ,Token (AlexPn 12 1 13) TLbrace
                      ,Token (AlexPn 12 1 13) (TId "b")
                      ,Token (AlexPn 14 1 15) TEq
                      ,Token (AlexPn 16 1 17) TLbrace
                      ,Token (AlexPn 16 1 17) TLet
                      ,Token (AlexPn 20 1 21) TLbrace
                      ,Token (AlexPn 20 1 21) (TId "c")
                      ,Token (AlexPn 22 1 23) TEq
                      ,Token (AlexPn 24 1 25) TLbrace
                      ,Token (AlexPn 24 1 25) (TInteger 42)
                      ,Token (AlexPn 28 3 1) TRbrace
                      ,Token (AlexPn 28 3 1) TRbrace
                      ,Token (AlexPn 28 3 1) TRbrace
                      ,Token (AlexPn 28 3 1) TRbrace
                      ,Token (AlexPn 28 3 1) TRbrace
                      ,Token (AlexPn 28 3 1) TRbrace]
            $ "let a = let b = let c = 42\n\n"
            
          checkTokens [Token (AlexPn 0 1 1) TLet
                      ,Token (AlexPn 4 1 5) TLbrace
                      ,Token (AlexPn 4 1 5) (TId "a")
                      ,Token (AlexPn 6 1 7) TEq
                      ,Token (AlexPn 8 1 9) TLbrace
                      ,Token (AlexPn 8 1 9) TLet
                      ,Token (AlexPn 12 1 13) TLbrace
                      ,Token (AlexPn 12 1 13) (TId "b")
                      ,Token (AlexPn 14 1 15) TEq
                      ,Token (AlexPn 16 1 17) TLbrace
                      ,Token (AlexPn 16 1 17) TLet
                      ,Token (AlexPn 20 1 21) TLbrace
                      ,Token (AlexPn 20 1 21) (TId "c")
                      ,Token (AlexPn 22 1 23) TEq
                      ,Token (AlexPn 24 1 25) TLbrace
                      ,Token (AlexPn 24 1 25) (TInteger 42)
                      ,Token (AlexPn 36 5 1) TRbrace
                      ,Token (AlexPn 36 5 1) TRbrace
                      ,Token (AlexPn 36 5 1) TRbrace
                      ,Token (AlexPn 36 5 1) TRbrace
                      ,Token (AlexPn 36 5 1) TRbrace
                      ,Token (AlexPn 36 5 1) TRbrace]
            $ unlines ["let a = let b = let c = 42"
                      ,""
                      ,"//   "
                      ," "]
            
          checkTokens [Token (AlexPn 0 1 1) TIf
                      ,Token (AlexPn 3 1 4) (TId "a")
                      ,Token (AlexPn 5 1 6) (TId "equals")
                      ,Token (AlexPn 12 1 13) (TId "b")
                      ,Token (AlexPn 13 1 14) TThen
                      ,Token (AlexPn 16 2 3) TLbrace
                      ,Token (AlexPn 16 2 3) (TInteger 42)
                      ,Token (AlexPn 19 2 6) (TInteger 57)
                      ,Token (AlexPn 26 3 5) (TId "continued")
                      ,Token (AlexPn 38 4 3) TSemicolon
                      ,Token (AlexPn 38 4 3) (TId "newline")
                      ,Token (AlexPn 46 5 1) TRbrace
                      ,Token (AlexPn 46 5 1) TSemicolon
                      ,Token (AlexPn 46 5 1) TElse
                      ,Token (AlexPn 53 6 3) TLbrace
                      ,Token (AlexPn 53 6 3) (TId "forgetit")
                      ,Token (AlexPn 62 7 1) TRbrace]
            $ unlines ["if a equals b"
                      ,"  42 57"
                      ,"    continued"
                      ,"  newline"
                      ,"else"
                      ,"  forgetit"]
            
          checkTokens [Token (AlexPn 0 1 1) TIf
                      ,Token (AlexPn 3 1 4) TLparen
                      ,Token (AlexPn 4 1 5) (TId "a")
                      ,Token (AlexPn 6 1 7) (TInteger 42)
                      ,Token (AlexPn 14 2 6) (TInteger 43)
                      ,Token (AlexPn 17 2 9) (TInteger 44)
                      ,Token (AlexPn 24 3 5) (TId "cont")
                      ,Token (AlexPn 36 4 8) TRparen
                      ,Token (AlexPn 37 4 9) TThen
                      ,Token (AlexPn 40 5 3) TLbrace
                      ,Token (AlexPn 40 5 3) (TId "body1")
                      ,Token (AlexPn 46 5 9) (TId "body2")
                      ,Token (AlexPn 56 6 5) (TId "cont")
                      ,Token (AlexPn 63 7 3) TSemicolon
                      ,Token (AlexPn 63 7 3) (TId "body3")
                      ,Token (AlexPn 69 8 1) TRbrace]
            $ unlines ["if (a 42"
                      ,"     43 44"
                      ,"    cont"
                      ,"       )"
                      ,"  body1 body2"
                      ,"    cont"
                      ,"  body3"]
            
          checkTokens [Token (AlexPn 0 1 1) TIf
                      ,Token (AlexPn 3 1 4) TLparen
                      ,Token (AlexPn 4 1 5) TRparen] "if ()"
            
          checkTokens [Token (AlexPn 0 1 1) TIf
                      ,Token (AlexPn 3 1 4) TLparen
                      ,Token (AlexPn 4 1 5) TRparen
                      ,Token (AlexPn 5 1 6) TThen
                      ,Token (AlexPn 6 2 1) TLbrace
                      ,Token (AlexPn 6 2 1) TRbrace]
            $ unlines ["if ()"]
          
          checkTokens [Token (AlexPn 0 1 1) TIf
                      ,Token (AlexPn 3 1 4) TLparen
                      ,Token (AlexPn 4 1 5) TRparen
                      ,Token (AlexPn 5 1 6) TThen
                      ,Token (AlexPn 7 2 2) TLbrace
                      ,Token (AlexPn 7 2 2) (TId "line1")
                      ,Token (AlexPn 13 3 1) TRbrace]
            $ unlines ["if ()"
                      ," line1"]
            
          checkTokens [Token (AlexPn 0 1 1) TIf
                      ,Token (AlexPn 3 1 4) TLparen
                      ,Token (AlexPn 4 1 5) TRparen
                      ,Token (AlexPn 5 1 6) TThen
                      ,Token (AlexPn 8 3 2) TLbrace
                      ,Token (AlexPn 8 3 2) (TId "line1")
                      ,Token (AlexPn 42 7 3) (TId "line1cont")
                      ,Token (AlexPn 53 8 2) TSemicolon
                      ,Token (AlexPn 53 8 2) (TId "line2")
                      ,Token (AlexPn 78 12 2) TSemicolon
                      ,Token (AlexPn 78 12 2) (TId "line3")
                      ,Token (AlexPn 84 13 1) TRbrace]
                      $ unlines ["if ()"
                                ,""
                                ," line1"
                                ,""
                                ," // Testing empty lines"
                                ,""
                                ,"  line1cont"
                                ," line2"
                                ,""
                                ,"// More empties"
                                ,""
                                ," line3"]
            
          checkTokens [Token (AlexPn 0 1 1) TLet
                      ,Token (AlexPn 4 1 5) TLbrace
                      ,Token (AlexPn 4 1 5) (TId "a")
                      ,Token (AlexPn 6 1 7) TEq
                      ,Token (AlexPn 8 1 9) TLbrace
                      ,Token (AlexPn 8 1 9) TLparen
                      ,Token (AlexPn 9 1 10) (TInteger 42)
                      ,Token (AlexPn 13 2 2) (TInteger 12)
                      ,Token (AlexPn 16 2 5) (TInteger 46)
                      ,Token (AlexPn 19 2 8) (TInteger 823)
                      ,Token (AlexPn 23 3 1) TRparen
                      ,Token (AlexPn 29 4 5) TRbrace
                      ,Token (AlexPn 29 4 5) TAnd
                      ,Token (AlexPn 29 4 5) (TId "b")
                      ,Token (AlexPn 31 4 7) TEq
                      ,Token (AlexPn 33 4 9) TLbrace
                      ,Token (AlexPn 33 4 9) (TInteger 51)
                      ,Token (AlexPn 44 5 9) TSemicolon
                      ,Token (AlexPn 44 5 9) (TId "run")
                      ,Token (AlexPn 58 6 11) (TId "jump")
                      ,Token (AlexPn 63 7 1) TRbrace
                      ,Token (AlexPn 63 7 1) TRbrace]
            $ unlines ["let a = (42"
                      ," 12 46 823"
                      ,")"
                      ,"    b = 51"
                      ,"        run"
                      ,"          jump"]
            
          checkTokens [Token (AlexPn 0 1 1) TIf
                      ,Token (AlexPn 3 1 4) (TId "a")
                      ,Token (AlexPn 5 1 6) (TId "b")
                      ,Token (AlexPn 7 1 8) (TId "c")
                      ,Token (AlexPn 9 1 10) TLbrace
                      ,Token (AlexPn 11 1 12) (TId "ignoring")
                      ,Token (AlexPn 20 1 21) TSemicolon
                      ,Token (AlexPn 22 1 23) (TId "indentation")
                      ,Token (AlexPn 35 2 1) (TId "for_this")
                      ,Token (AlexPn 44 2 10) (TId "block")
                      ,Token (AlexPn 53 3 4) (TId "and")
                      ,Token (AlexPn 57 3 8) (TId "a")
                      ,Token (AlexPn 59 3 10) TSemicolon
                      ,Token (AlexPn 61 3 12) (TId "few")
                      ,Token (AlexPn 65 3 16) (TId "more")
                      ,Token (AlexPn 70 3 21) TRbrace
                      ,Token (AlexPn 72 4 1) TSemicolon
                      ,Token (AlexPn 72 4 1) (TId "not")
                      ,Token (AlexPn 76 4 5) (TId "a")
                      ,Token (AlexPn 78 4 7) (TId "new")
                      ,Token (AlexPn 82 4 11) (TId "block")]
            $ unlines ["if a b c { ignoring ; indentation "
                      ,"for_this block"
                      ,"   and a ; few more }"
                      ,"not a new block"]
            
          checkTokens [Token (AlexPn 0 1 1) TIf
                      ,Token (AlexPn 3 1 4) (TInteger 42)
                      ,Token (AlexPn 6 1 7) (TInteger 21)
                      ,Token (AlexPn 9 1 10) TLbrace
                      ,Token (AlexPn 11 1 12) (TId "explicit")
                      ,Token (AlexPn 20 1 21) TSemicolon
                      ,Token (AlexPn 22 1 23) (TId "block")
                      ,Token (AlexPn 28 1 29) TRbrace
                      ,Token (AlexPn 30 2 1) TSemicolon
                      ,Token (AlexPn 30 2 1) TElse
                      ,Token (AlexPn 35 2 6) TLbrace
                      ,Token (AlexPn 35 2 6) (TId "real")
                      ,Token (AlexPn 45 3 6) TSemicolon
                      ,Token (AlexPn 45 3 6) (TId "block")
                      ,Token (AlexPn 51 3 12) (TInteger 42)
                      ,Token (AlexPn 54 4 1) TRbrace]
            $ unlines ["if 42 21 { explicit ; block }"
                      ,"else real"
                      ,"     block 42"]

          checkTokens [Token (AlexPn 0 1 1)   (TInteger 42)
                      ,Token (AlexPn 3 1 4)   (TDuration (Duration             42))
                      ,Token (AlexPn 9 1 10)  (TDuration (Duration          41000))
                      ,Token (AlexPn 15 1 16) (TDuration (Duration       40000000))
                      ,Token (AlexPn 21 1 22) (TDuration (Duration     2000000000))
                      ,Token (AlexPn 25 1 26) (TDuration (Duration   180000000000))
                      ,Token (AlexPn 29 1 30) (TDuration (Duration 14400000000000))
                      ]
             "42 42 ns 41 us 40 ms 2 s 3m  4     h"

          checkTokens [Token (AlexPn 1 1 2) (TOp "!")
                      ,Token (AlexPn 3 1 4) (TOp "#")
                      ,Token (AlexPn 5 1 6) (TOp "$")
                      ,Token (AlexPn 7 1 8) (TOp "%")
                      ,Token (AlexPn 9 1 10) (TOp "&")
                      ,Token (AlexPn 11 1 12) (TOp "*")
                      ,Token (AlexPn 13 1 14) (TOp "+")
                      ,Token (AlexPn 15 1 16) (TOp ".")
                      ,Token (AlexPn 17 1 18) (TOp "/")
                      ,Token (AlexPn 19 1 20) (TOp "<")
                      ,Token (AlexPn 21 1 22) (TOp "='")
                      ,Token (AlexPn 24 1 25) (TOp ">")
                      ,Token (AlexPn 26 1 27) (TOp "?")
                      ,Token (AlexPn 28 1 29) (TOp "@_")
                      ,Token (AlexPn 31 1 32) (TOp "\\")
                      ,Token (AlexPn 33 1 34) (TOp "^")
                      ,Token (AlexPn 35 1 36) (TOp "|\"")
                      ,Token (AlexPn 38 1 39) (TOp "-")
                      ,Token (AlexPn 40 1 41) (TOp "~")
                      ,Token (AlexPn 42 1 43) (TOp "!_:\"'")]
            " ! # $ % & * + . / < =' > ? @_ \\ ^ |\" - ~ !_:\"'  "

          checkTokens [Token (AlexPn 1 1 2) (TOp "add")
                      ,Token (AlexPn 7 1 8) (TOp "plus")
                      ,Token (AlexPn 14 1 15) (TOp "foo_42_bar'")]
            " `add` `plus` `foo_42_bar'`"
           


{-
if do one
      two
      three
  body1
  body2


-- Completely ignoring indentation in delimited regions
-- Reasonable

              (sdf asf sdf
1 2 3 4)

         (sdf asd sdf
        1 2 3 4)

-- No indentation errors

let a = 3
     b = 4
  c = 5

let {a = {3}
     b = {4}}
  c = 5

let { a = { 3 } b = { 4 } } c = 5

{ asdfa } { 42 }

asdfa 42


f(x,y,z)

f x y z



A problem with "if"

if (asdfads) {asd ; asd}

looks like "apply asdfds to {asd; asd}"

Really need something like "then"




-}

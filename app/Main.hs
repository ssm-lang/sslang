{-# LANGUAGE RecordWildCards,
             LambdaCase #-}

module Main where

import Scanner ( runAlex )
import Parser  ( parse )

import ParseOperators ( parseOperators, Fixity(..) )

--import Ast ( printAST )
import Ast2IR  ( astToIR )
import CGen ( cgen, hgen )

import qualified Ast as A

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr(..),
                              ArgDescr(NoArg, ReqArg), ArgOrder(RequireOrder))
import System.IO( hPutStrLn, hPutStr, stderr )
import System.Exit( exitSuccess, exitFailure )
import Control.Monad( when, unless )

data Mode = DumpIR
          | DumpAST
          | DumpASTP
          | GenerateC
          | GenerateH
  deriving (Eq, Show)

data Options = Options
  { optMode :: Mode
  , modName :: String
  }

defaultOptions :: Options
defaultOptions = Options
  { optMode = GenerateC
  , modName = "top"
  }
      

usageMessage :: IO ()
usageMessage = do prg <- getProgName
                  let header = "Usage: " ++ prg ++ " [options] file..."
                  hPutStr stderr (usageInfo header optionDescriptions)

optionDescriptions :: [ OptDescr (Options -> IO Options) ]
optionDescriptions =
    [ Option "h" ["help"]
        (NoArg (\_ -> usageMessage >> exitSuccess ))
        "Print help"
    , Option "" ["dump-ast"]
        (NoArg (\ opt -> return opt { optMode = DumpAST }))
        "Print the AST"      
    , Option "" ["dump-ast-parsed"]
        (NoArg (\ opt -> return opt { optMode = DumpASTP }))
        "Print the AST after operators are parsed"      
    , Option "" ["dump-ir"]
        (NoArg (\ opt -> return opt { optMode = DumpIR }))
        "Print the IR"
    , Option "" ["generate-c"]
        (NoArg (\ opt -> return opt { optMode = GenerateC }))
        "Generate C (default)"
    , Option "" ["generate-h"]
        (NoArg (\ opt -> return opt { optMode = GenerateH }))
        "Generate a C header file"
    , Option "m" ["module-name"]
        (ReqArg (\mn opt -> return opt { modName = mn }) "<module-name>")
        "Set the module name"
    ]

defaultOps :: [Fixity]      -- FIXME
defaultOps = [Infixl 6 "+"
             ,Infixl 6 "-"
             ,Infixl 7 "*"
             ,Infixl 8 "/"
             ,Infixr 8 "^"
             ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, filenames, errors) =
        getOpt RequireOrder optionDescriptions args
  options <- foldl (>>=) (return defaultOptions) actions -- Perform actions
  unless (null errors) (do mapM_ (hPutStr stderr) errors
                           usageMessage
                           exitFailure)
  when (null filenames) (do hPutStrLn stderr "Error: no source files"
                            usageMessage
                            exitFailure)

  let Options {..} = options -- Bring the named options into scope

  inputs <- mapM (\case
                   "-"      -> getContents -- Read from stdin
                   filename -> readFile filename) filenames
                              
  ast <- case runAlex (head inputs) parse of -- FIXME: multiple inputs?
           Right a -> return a
           Left s -> do hPutStrLn stderr $ "Error: " ++ s
                        exitFailure

  when (optMode == DumpAST) $ print ast >> exitSuccess

  -- FIXME: Should take into account fixity definitions in the program
  let A.Program decls = ast
      ast' = A.Program $ map helper decls
      helper (A.Function s bs e t) = A.Function s bs (parseOperators defaultOps e) t

  when (optMode == DumpASTP) $ print ast' >> exitSuccess

  let ir = astToIR ast'

  when (optMode == DumpIR) $ print ir >> exitSuccess

  when (optMode == GenerateH) $ print (hgen modName ir) >> exitSuccess
  
  when (optMode == GenerateC) $ print (cgen modName ir) >> exitSuccess

  

-- | CLI front-end to the sslang compiler.
module Main where

import qualified Codegen
import           Common.Compiler                ( passIO )
import           Common.Default                 ( Default(..) )
import qualified Front
import qualified IR

import           Control.Monad                  ( unless
                                                , when
                                                )
import           System.Console.GetOpt          ( ArgDescr(..)
                                                , ArgOrder(..)
                                                , OptDescr(..)
                                                , getOpt
                                                , getOpt'
                                                , usageInfo
                                                )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( hPutStr
                                                , hPutStrLn
                                                , stderr
                                                )

-- | Print the usage message, collating options from each compiler stage.
usageMessage :: IO ()
usageMessage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [options] file"
  hPutStr stderr $ unlines
    [ header
    , usageInfo "" options
    , usageInfo "" Front.options
    , usageInfo "" IR.options
    , usageInfo "" Codegen.options
    ]

-- | CLI options.
options :: [OptDescr (IO ())]
options =
  [Option "h" ["help"] (NoArg (usageMessage >> exitSuccess)) "Print help"]

-- | Read input from file or stdin.
readInput :: String -> IO String
readInput "-"      = getContents
readInput filename = readFile filename

{- | Compiler executable entry point.

This
-}
main :: IO ()
main = do
  args <- getArgs

  let (cliActions, fArgs, fArgs', cliErrors) =
        getOpt' RequireOrder options args
      (fOpts, iArgs, iArgs', fErr) =
        getOpt' RequireOrder Front.options $ fArgs' ++ fArgs
      (iOpts, cArgs, cArgs', iErr) =
        getOpt' RequireOrder IR.options $ iArgs' ++ iArgs
      (cOpts, filenames, cErr) =
        getOpt RequireOrder Codegen.options $ cArgs' ++ cArgs
      errors      = cliErrors ++ fErr ++ iErr ++ cErr
      frontOpts   = foldr ($) def fOpts
      irOpts      = foldr ($) def iOpts
      codegenOpts = foldr ($) def cOpts

  unless (null errors) $ do
    mapM_ (hPutStr stderr) errors
    usageMessage
    exitFailure

  mapM_ return cliActions

  when (null filenames) $ do
    hPutStrLn stderr "Error: no source files"
    usageMessage
    exitFailure

  when (length filenames > 1) $ do
    hPutStrLn stderr "Error: specified more than one file"
    usageMessage
    exitFailure

  input <- readInput $ head filenames
  ast   <- passIO $ Front.run frontOpts input
  ir    <- passIO $ IR.run irOpts ast
  c     <- passIO $ Codegen.run codegenOpts ir
  putStrLn c

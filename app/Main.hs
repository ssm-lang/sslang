module Main where

import qualified Common.Compiler as Compiler
import qualified Front
import qualified IR
import qualified Types

import           Control.Monad                  ( unless
                                                , when
                                                )
import           System.Console.GetOpt          ( ArgDescr(NoArg, ReqArg)
                                                , ArgOrder(RequireOrder)
                                                , OptDescr(..)
                                                , getOpt
                                                , usageInfo
                                                )
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( hPrint
                                                , hPutStr
                                                , hPutStrLn
                                                , stderr
                                                )

data Mode
  = DumpAST    -- ^ AST before operator parsing
  | DumpASTP   -- ^ AST after operators are parsed
  | DumpIR     -- ^ Intermediate representation
  deriving (Eq, Show)

data Options = Options
  { optMode :: Mode
  , modName :: String
  }

defaultOptions :: Options
defaultOptions = Options { optMode = DumpIR, modName = "top" }


usageMessage :: IO ()
usageMessage = do
  prg <- getProgName
  let header = "Usage: " ++ prg ++ " [options] file..."
  hPutStr stderr (usageInfo header optionDescriptions)

optionDescriptions :: [OptDescr (Options -> IO Options)]
optionDescriptions =
  [ Option "h" ["help"] (NoArg (\_ -> usageMessage >> exitSuccess)) "Print help"
  , Option ""
           ["dump-ast"]
           (NoArg (\opt -> return opt { optMode = DumpAST }))
           "Print the AST"
  , Option ""
           ["dump-ast-parsed"]
           (NoArg (\opt -> return opt { optMode = DumpASTP }))
           "Print the AST after operators are parsed"
  , Option ""
           ["dump-ir"]
           (NoArg (\opt -> return opt { optMode = DumpIR }))
           "Print the IR"
{-    , Option "" ["generate-c"]
        (NoArg (\ opt -> return opt { optMode = GenerateC }))
        "Generate C (default)"
    , Option "" ["generate-h"]
        (NoArg (\ opt -> return opt { optMode = GenerateH }))
        "Generate a C header file"
-}
  , Option "m"
           ["module-name"]
           (ReqArg (\mn opt -> return opt { modName = mn }) "<module-name>")
           "Set the module name"
  ]

doPass :: Compiler.Pass a -> IO a
doPass p = case Compiler.runPass p of
             Left e -> hPrint stderr e >> exitFailure
             Right a -> return a

readInput :: String -> IO String
readInput "-"      = getContents
readInput filename = readFile filename

main :: IO ()
main = do
  args <- getArgs
  let (actions, filenames, errors) =
        getOpt RequireOrder optionDescriptions args
  opts <- foldl (>>=) (return defaultOptions) actions -- Perform actions
  unless (null errors) $ do
    mapM_ (hPutStr stderr) errors
    usageMessage
    exitFailure

  when (null filenames) $ do
    hPutStrLn stderr "Error: no source files"
    usageMessage
    exitFailure

  inputs <- mapM readInput filenames

  ast    <- doPass $ Front.parseSource (head inputs)
  when (optMode opts == DumpAST) $ print ast >> exitSuccess

  ast' <- doPass $ Front.desugarAst ast
  when (optMode opts == DumpASTP) $ print ast' >> exitSuccess

  () <- doPass $ Front.checkAst ast'

  irA <- doPass $ Front.lowerAst ast'

  irC <- doPass $ Types.inferTypes irA

  irP <- doPass $ Types.instantiateClasses irC

  irF <- doPass $ Types.monomorphize irP

  irL <- doPass $ IR.yieldAbstraction irF

  irG <- doPass $ IR.lambdaLift irL

  irI <- doPass $ IR.defunctionalize irG

  irD <- doPass $ IR.inferDrops irI

  _ <- doPass $ IR.codegen irD

  putStrLn "TODO"

--  when (optMode == DumpIR) $ print ir >> exitSuccess

--  when (optMode == GenerateH) $ print (hgen modName ir) >> exitSuccess

--  when (optMode == GenerateC) $ print (cgen modName ir) >> exitSuccess

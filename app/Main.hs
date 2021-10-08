module Main where

import           Common.Errors                  ( CompileError(..) )
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

runPass :: Either CompileError a -> IO a
runPass (Left e) = do
  hPrint stderr e >> exitFailure
runPass (Right a) = return a

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

  ast    <- runPass $ Front.parseSource (head inputs)
  when (optMode opts == DumpAST) $ print ast >> exitSuccess

  ast' <- runPass $ Front.desugarAst ast
  when (optMode opts == DumpASTP) $ print ast' >> exitSuccess

  () <- runPass $ Front.checkAst ast'

  irA <- runPass $ Front.lowerAst ast'

  irC <- runPass $ Types.inferTypes irA

  irP <- runPass $ Types.instantiateClasses irC

  irF <- runPass $ Types.monomorphize irP

  irL <- runPass $ IR.yieldAbstraction irF

  irG <- runPass $ IR.lambdaLift irL

  irI <- runPass $ IR.defunctionalize irG

  irD <- runPass $ IR.inferDrops irI

  _ <- runPass $ IR.codegen irD

  putStrLn "TODO"

--  when (optMode == DumpIR) $ print ir >> exitSuccess

--  when (optMode == GenerateH) $ print (hgen modName ir) >> exitSuccess

--  when (optMode == GenerateC) $ print (cgen modName ir) >> exitSuccess

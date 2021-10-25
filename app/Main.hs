module Main where

import qualified Codegen
import qualified Common.Compiler               as Compiler
import qualified Front
import qualified IR

import           Control.Monad                  ( unless
                                                , when
                                                )
import           Prettyprinter                  ( pretty )
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
                                                , stdout
                                                )

data Mode
  = DumpTokens  -- ^ Token stream before parsing
  | DumpAST     -- ^ AST before operator parsing
  | DumpASTP    -- ^ AST after operators are parsed
  | DumpIR      -- ^ Intermediate representation
  | GenerateC   -- ^ Generate C backend
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
           ["dump-tokens"]
           (NoArg (\opt -> return opt { optMode = DumpTokens }))
           "Print the token stream"
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
  , Option ""
           ["generate-c"]
           (NoArg (\opt -> return opt { optMode = GenerateC }))
           "Generate C (default)"
  , Option "m"
           ["module-name"]
           (ReqArg (\mn opt -> return opt { modName = mn }) "<module-name>")
           "Set the module name"
  ]

doPass :: Compiler.Pass a -> IO a
doPass p = case Compiler.runPass p of
  Left  e -> hPrint stderr e >> exitFailure
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

  when (optMode opts == DumpTokens) $ do
    doPass (Front.tokenStream (head inputs)) >>= mapM_ (print . pretty)
    exitSuccess
  ast <- doPass $ Front.parseSource (head inputs)
  when (optMode opts == DumpAST) $ putStrLn (Front.renderAst ast) >> exitSuccess

  ast' <- doPass $ Front.desugarAst ast
  when (optMode opts == DumpASTP)
    $  putStrLn (Front.renderAst ast')
    >> exitSuccess

  ()    <- doPass $ Front.checkAst ast'

  irA   <- doPass $ IR.lowerAst ast'

  irC   <- doPass $ IR.inferTypes irA

  irP   <- doPass $ IR.instantiateClasses irC

  irY   <- doPass $ IR.yieldAbstraction irP

  irL   <- doPass $ IR.lambdaLift irY

  irI   <- doPass $ IR.defunctionalize irL

  irD   <- doPass $ IR.inferDrops irI

  irM   <- doPass $ IR.monomorphize irD

  cDefs <- doPass $ Codegen.genIR irM

  when (optMode opts == GenerateC) $ doPass (Codegen.prettyC cDefs) >>= putStrLn

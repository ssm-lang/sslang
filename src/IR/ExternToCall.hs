{-# LANGUAGE DerivingVia #-}
-- | Turns non-nullary data constructors into calls to constructor functions.
module IR.ExternToCall
  ( externToCall
  ) where

import qualified Common.Compiler               as Compiler

import           Common.Compiler                ( MonadError )
import           Common.Identifiers             ( fromId
                                                , fromString
                                                , ident
                                                )
import qualified IR.IR                         as I
import qualified IR.Types                      as I

import           Control.Monad.Reader           ( MonadReader(..)
                                                , ReaderT(..)
                                                , asks
                                                )
import           Data.Bifunctor                 ( first )
import           Data.Generics.Aliases          ( mkM )
import           Data.Generics.Schemes          ( everywhereM )
import qualified Data.Set                      as S


-- | Environment storing arity of each 'DCon' 
type ExternEnv = S.Set I.VarId

-- | Extern Reader Monad
newtype ExternFn a = ExternFn (ReaderT ExternEnv Compiler.Pass a)
  deriving Functor                      via (ReaderT ExternEnv Compiler.Pass)
  deriving Applicative                  via (ReaderT ExternEnv Compiler.Pass)
  deriving Monad                        via (ReaderT ExternEnv Compiler.Pass)
  deriving MonadFail                    via (ReaderT ExternEnv Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (ReaderT ExternEnv Compiler.Pass)
  deriving (MonadReader ExternEnv)      via (ReaderT ExternEnv Compiler.Pass)

-- | Run a computation within an arity environment
runExternFn :: ExternEnv -> ExternFn a -> Compiler.Pass a
runExternFn env (ExternFn m) = runReaderT m env

{- | 'externToCall' modifies programDefs and traverses the IR to accomplish two tasks:

  (1) Add top-level constructor functions for each non-nullary 'DCon' to progamDefs
  2. Turn non-nullary data constuctors into calls to top level constructor funcs
-}
externToCall :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
externToCall p@I.Program { I.programDefs = defs, I.externDecls = xds } =
  runExternFn (S.fromList $ map fst xds) $ do
    externs' <- mapM makeExternFunc xds
    defs'    <- everywhereM (mkM dataToApp) defs
    return p { I.programDefs = externs' ++ defs' }

dataToApp :: I.Expr I.Type -> ExternFn (I.Expr I.Type)
dataToApp (I.Var n t) = do
  isExtern <- asks (S.member n)
  return $ I.Var (if isExtern then liftExtern n else n) t
  -- FIXME: this does not account for name shadowing
dataToApp a = pure a

makeExternFunc :: (I.VarId, I.Type) -> ExternFn (I.VarId, I.Expr I.Type)
makeExternFunc (x, t) = do
  let (ats, rt) = I.unfoldArrow t
      args      = zip (map argName [0 ..]) ats
      body      = I.Prim (I.FfiCall $ fromId x) (map (uncurry I.Var) args) rt
  if null ats
    then Compiler.typeError errMsg
    else return (liftExtern x, I.foldLambda (map (first Just) args) body)
 where
  errMsg = "Extern symbol does not have function type: " ++ show x
  argName :: Int -> I.VarId
  argName i = fromString ("__arg" ++ show i)

liftExtern :: I.VarId -> I.VarId
liftExtern dconid = fromString $ "__" ++ ident dconid

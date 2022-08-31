{-# LANGUAGE ViewPatterns #-}
module IR.Types.Typechecking
  ( typecheckProgram
  ) where

import           IR.IR                          ( Annotation
                                                , Annotations
                                                , DConId
                                                , Expr
                                                , Program(..)
                                                , TConId
                                                , Type
                                                , TypeDef(..)
                                                , TypeVariant(..)
                                                )
import           IR.Types.Inference             ( inferProgram )
import           IR.Types.Type                  ( Annotation
                                                  ( AnnArrows
                                                  , AnnDCon
                                                  , AnnType
                                                  )
                                                , Type(TCon, TVar)
                                                , foldArrow
                                                , unAnnotations
                                                )
import qualified Common.Compiler               as Compiler

import           Control.Monad.Reader           ( MonadReader(..)
                                                , MonadTrans(..)
                                                , ReaderT(..)
                                                , unless
                                                )
import           Data.Bifunctor                 ( Bifunctor(..) )
import qualified Data.Map                      as M

type Kind = Int
type KindEnv = M.Map TConId Kind
type DConEnv = M.Map DConId ([Type], Type)
type AnnRavel = ReaderT (KindEnv, DConEnv) Compiler.Pass

typecheckProgram :: Program Annotations -> Compiler.Pass (Program Type)
typecheckProgram p = do
  let kenv = M.fromList $ map (second $ length . targs) $ typeDefs p
  denv <- M.unions <$> mapM (checkTypeDef kenv) (typeDefs p)
  defs <- (`runReaderT` (kenv, denv)) $ unravelAnns $ unwrapAnns $ programDefs p
  inferProgram $ p { programDefs = defs }
 where
  -- | Unwrap each 'Annotations' into a bare 'Annotation' list.
  unwrapAnns :: [(a, Expr Annotations)] -> [(a, Expr [Annotation])]
  unwrapAnns = fmap $ second $ fmap unAnnotations

  -- | Unravel each 'Annotation' in the list into a 'Type', and check kindness.
  unravelAnns :: [(a, Expr [Annotation])] -> AnnRavel [(a, Expr [Type])]
  unravelAnns (unzip -> (bs, es)) = do
    es'       <- mapM (mapM $ mapM unravelAnnotation) es
    (kenv, _) <- ask
    lift $ mapM_ (mapM $ mapM $ checkType kenv) es'
    return $ zip bs es'

checkTypeDef :: KindEnv -> (TConId, TypeDef) -> Compiler.Pass DConEnv
checkTypeDef kenv (tcon, TypeDef { targs = tvs, variants = vs }) =
  M.unions <$> mapM variant2env vs
 where
  variant2env :: (DConId, TypeVariant) -> Compiler.Pass DConEnv
  variant2env (dcon, VariantNamed ts) =
    mapM_ (checkType kenv . snd) ts >> mkDConInfo dcon undefined
  variant2env (dcon, VariantUnnamed ts) =
    mapM_ (checkType kenv) ts >> mkDConInfo dcon ts

  mkDConInfo :: DConId -> [Type] -> Compiler.Pass DConEnv
  mkDConInfo dcon ts = return $ M.singleton dcon (ts, TCon tcon $ map TVar tvs)

checkType :: KindEnv -> Type -> Compiler.Pass ()
checkType _    TVar{}         = return ()
checkType kenv (TCon tcon ts) = do
  k <- maybe missing return $ M.lookup tcon kenv
  unless (k == length ts) $ do
    wrongKind k $ length ts
  mapM_ (checkType kenv) ts
 where
  missing :: Compiler.Pass a
  missing =
    Compiler.typeError $ "Could not find type constructor: " ++ show tcon
  wrongKind :: Kind -> Kind -> Compiler.Pass a
  wrongKind expected actual = Compiler.typeError $ unlines
    [ "Wrong kind for type: " ++ show tcon
    , "Expected: " ++ fmtKind expected
    , "Actual: " ++ fmtKind actual
    ]
  fmtKind :: Kind -> String
  fmtKind k = concat $ "*" : replicate k " -> *"


unravelAnnotation :: Annotation -> AnnRavel Type
unravelAnnotation (AnnType t       ) = return t
unravelAnnotation (AnnArrows ats rt) = do
  ats' <- mapM unravelAnnotation ats
  rt'  <- unravelAnnotation rt
  return $ foldArrow (ats', rt')
unravelAnnotation (AnnDCon dcon anns) = do
  (kenv , denv ) <- ask
  (dargs, dtype) <- maybe missing return $ M.lookup dcon denv
  undefined
 where
  missing :: AnnRavel a
  missing =
    Compiler.typeError $ "Could not find data constructor: " ++ show dcon

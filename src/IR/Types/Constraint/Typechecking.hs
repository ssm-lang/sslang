{-# LANGUAGE ViewPatterns #-}
module IR.Types.Constraint.Typechecking
  ( typecheckProgram
  ) where

import           IR.IR
import           IR.Types.Constraint.Inference  ( inferProgram )
import           IR.Types.Type

import qualified Common.Compiler               as Compiler
import           Data.Bifunctor                 ( Bifunctor(..) )

typecheckProgram :: Program Annotations -> Compiler.Pass (Program Type)
typecheckProgram p = do
  checkTypeDefs $ typeDefs p
  defs <- unravelAnns . unwrapAnns $ programDefs p
  inferProgram $ p { programDefs = defs }
 where
  -- | Unwrap each 'Annotations' into a bare 'Annotation' list.
  unwrapAnns :: [(a, Expr Annotations)] -> [(a, Expr [Annotation])]
  unwrapAnns = fmap $ second $ fmap unAnnotations

  -- | Unravel each 'Annotation' in the list into a 'Type'.
  unravelAnns :: [(a, Expr [Annotation])] -> Compiler.Pass [(a, Expr [Type])]
  unravelAnns (unzip -> (bs, es)) = do
    -- TODO: add more context to support annotations deep inside patterns
    zip bs <$> mapM (mapM $ mapM unravelAnnotation) es


unravelAnnotation :: Annotation -> Compiler.Pass Type
unravelAnnotation a =
  Compiler.todo
    $  "unravelAnnotation not yet able to handle non-trivial types: "
    ++ show a

-- What is this for?
checkTypeDefs :: [(TConId, TypeDef)] -> Compiler.Pass ()
checkTypeDefs _ = return ()

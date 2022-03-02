-- | Turns unapplied and partially applied data constructors into constructor functions.

module IR.DConToFunc
  ( dConToFunc
  ) where

import qualified Common.Compiler               as Compiler

import           Common.Identifiers             ( fromString
                                                , ident
                                                )
import           Data.Bifunctor                 ( first )
import qualified IR.IR                         as I
import qualified IR.Types.Poly                 as Poly
import           IR.Types.TypeSystem            ( TypeDef(..)
                                                , TypeVariant
                                                  ( VariantNamed
                                                  , VariantUnnamed
                                                  )
                                                )
import Data.Maybe(mapMaybe)

{-
top level fuctions look like
(I.VarId, I.Expr I.Type)
where we match to get (name, l@(I.Lambda _ _ ty))
so we have (varid, lambda expr)
lamda should contain parameter names and body in a recursive/nested form

so for every dcon and args, create a top level func called dcon
which contains a lamda representing constructor.

Take typdefs from program state, and create constructors directly from table???

-- | Create a lambda chain given a list of argument-type pairs and a body.
makeLambdaChain :: TypeSystem t => [(Binder, t)] -> Expr t -> Expr t
makeLambdaChain args body = foldr chain body args
  where chain (v, t) b = Lambda v b $ t `arrow` extract b

-- | A name to be bound; 'Nothing' represents a wildcard, e.g., @let _ = ...@.
type Binder = Maybe VarId

-- Items 2 and 3 include both declarations and definitions.
genTop
  :: TypegenInfo
  -> (I.VarId, I.Expr I.Type)
  -> Compiler.Pass ([C.Definition], [C.Definition])
genTop info (name, l@(I.Lambda _ _ ty)) =
  runGenFn (fromId name) (zip argIds argTys) retTy body info $ do
    (stepDecl , stepDefn ) <- genStep
    (enterDecl, enterDefn) <- genEnter
    structDefn             <- genStruct
    return ([structDefn, enterDecl, stepDecl], [enterDefn, stepDefn])
 where
  (argIds, body ) = I.collectLambda l
  (argTys, retTy) = I.collectArrow ty
genTop _ (_, I.Lit _ _) = todo
genTop _ (_, _        ) = nope


-}


{-
data Program t = Program
  { programEntry :: VarId
  , programDefs  :: [(VarId, Expr t)]
  , typeDefs     :: [(TConId, TypeDef t)]
  }

 | 
 Add constructor functions for every dcon to top level function defs (mangled if necessary)
 App (Expr t) (Expr t) t
   let (fn, args) = I.collectApp a
   case fn of --(For each App of type data constructor) 
      (I.Data dcon dty) turn this into an (I.Var varid type)
      where varid = constructor function name, and type is tcon corresponding to dcon!

-- | Create an app chain given a list of arguments, a return type and a function name.
makeAppChain :: TypeSystem t => [Expr t] -> Expr t -> t -> Expr t
makeAppChain [h] f ret = App f h ret -- TODO make this func 1-2 lines instead of 3
makeAppChain args f ret= App f (foldr (chain ret) (last args) (tail args)) ret
 where chain typ acc arg = App arg acc typ
-}

dConToFunc :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
dConToFunc p@I.Program { I.programDefs = defs, I.typeDefs = tDefs } =
  pure p { I.programDefs = defs ++ concat (createFuncs <$> tDefs) }


-- | Create a group of top level functions for each type constructor
createFuncs :: (I.TConId, TypeDef Poly.Type) -> [(I.VarId, I.Expr Poly.Type)]
createFuncs (tconid, TypeDef { variants = vars }) = createFunc tconid `mapMaybe` vars
 where
  -- | Create a top level function for each data constructor
  createFunc
    :: I.TConId
    -> (I.DConId, TypeVariant Poly.Type)
    -> Maybe (I.VarId, I.Expr Poly.Type)
  createFunc _ (_, VariantNamed []) = Nothing
  createFunc tcon (I.DConId dcon, VariantNamed params) = Just (I.VarId dcon, lambda)
   where
    lambda = I.makeLambdaChain args body
    args   = first Just <$> params
    body   = I.makeAppChain (uncurry I.Var <$> params)
                            (I.Data (I.DConId dcon) (Poly.TCon tcon []))
  createFunc tcon (I.DConId dcon, VariantUnnamed params) = createFunc
    tcon
    (I.DConId dcon, VariantNamed mangledNames)
   where
    mangle :: [Char] -> t -> Int -> (I.VarId, t)
    mangle str typ index =
      (I.VarId $ fromString ("__" ++ str ++ show index), typ)
    mangledNames = zipWith (mangle $ ident dcon) params [0 ..]


{- | Replace data constructor application with function application

Turn I.App instances of the form (I.Data _ _) into (I.Var _ _)
-}

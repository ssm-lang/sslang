{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE DerivingVia #-}

{- | Turns non-nullary data constructors into calls to constructor functions.

Worked example of ADT definition and corresponding constructor functions:

> type Shape 
>   Square Int 
>   Rect Int Int

Let's turn data constructor `Square` into constructor function `__Square`:

> __Square arg0 : Int -> Shape =  Square arg0

The difference is that `Square` cannot be partially-applied, whereas `__Square` can.

Next, `Rect` turns into this:

> __Rect arg0 arg1 : Int -> Int -> Shape =  Rect arg0 arg1

The difference is that `Rect` cannot be partially-applied, whereas `__Rect` can.


Representing constructor functions in the IR:

Every top-level function has the form (I.VarId, I.Expr Poly.Type) = (functionName, functionBody)
The function body is a lambda expression representing a call to the fully applied data constructor.

Let's turn the top-level func for `Square` into IR: 

@
(__Square, body) 
body = fun arg0 { App L R t } : Int -> Shape
 where L = Square : Int -> Shape
       R = arg0 : type Int
       t = Shape, because the type of a fully applied data constructor
           is its type constructor@
@
Next `Rect` turns into this:

@
(Rect, body)
body = fun arg0 { fun arg1 { App L R t } : Int -> Shape } : Int -> Int -> Shape
 where L = App L2 R2 t
       R = arg1 : Int
       t = Shape, because the type of a fully applied data constructor 
           is its type constructor
        where L2 = Rect : Int -> Int -> Shape
              R  = arg0 : Int
              t = Int -> Shape, because at this point in the inner App, 
                  Rect is partially applied with only 1 arg.
@
-}
module IR.DConToFunc
  ( dConToFunc
  ) where

import qualified Common.Compiler               as Compiler

import           Common.Compiler                ( MonadError )
import           Common.Identifiers             ( fromId
                                                , fromString
                                                , ident
                                                )
import           Control.Monad.Reader
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.Generics.Aliases          ( mkM )
import           Data.Generics.Schemes          ( everywhereM )
import           Data.List                      ( inits )
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )
import qualified IR.IR                         as I
import qualified IR.Types.Poly                 as Poly
import           IR.Types.TypeSystem            ( TypeDef(..)
                                                , TypeVariant
                                                  ( VariantNamed
                                                  , VariantUnnamed
                                                  )
                                                , arrow
                                                , variantFields
                                                )

-- | Environment storing arity of each 'DCon' 
type ArityEnv = M.Map I.DConId Int

-- | Arity Reader Monad
newtype ArityFn a = ArityFn (ReaderT ArityEnv Compiler.Pass a)
  deriving Functor                      via (ReaderT ArityEnv Compiler.Pass)
  deriving Applicative                  via (ReaderT ArityEnv Compiler.Pass)
  deriving Monad                        via (ReaderT ArityEnv Compiler.Pass)
  deriving MonadFail                    via (ReaderT ArityEnv Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (ReaderT ArityEnv Compiler.Pass)
  deriving (MonadReader ArityEnv)       via (ReaderT ArityEnv Compiler.Pass)

-- | Run a computation within an arity environment
runArityFn :: ArityEnv -> ArityFn a -> Compiler.Pass a
runArityFn env (ArityFn m) = runReaderT m env

-- | Create a map of (DConId, Arity) key-value pairs from a list of type defintions
createArityEnv :: [(I.TConId, TypeDef Poly.Type)] -> ArityEnv
createArityEnv defs = M.fromList $ concatMap arities defs
 where
  arities :: (I.TConId, TypeDef Poly.Type) -> [(I.DConId, Int)]
  arities (_, TypeDef { variants = vars }) = map (second variantFields) vars

{- | 'dConToFunc' modifies programDefs and traverses the IR to accomplish two tasks:

  (1) Add top-level constructor functions for each non-nullary 'DCon' to progamDefs
  2. Turn non-nullary data constuctors into calls to top level constructor funcs
-}
dConToFunc :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
dConToFunc p@I.Program { I.programDefs = defs, I.typeDefs = tDefs } =
  runArityFn (createArityEnv tDefs) $ do
    defs' <- defs'' -- user defined functions
    return p { I.programDefs = tDefs' ++ defs' } -- constructor funcs ++ user funcs
 where
  tDefs' = concat (createFuncs <$> tDefs)
  -- ^ top-level constructor functions
  defs'' =
    filter ((`notElem` (fst <$> tDefs')) . fst)
      <$> everywhereM (mkM dataToApp) defs
  -- ^ if a user defined func's name conflicts with a constructor func's name,
  -- ^ omit it the user defined func in favor of constructor.
  createFuncs (tconid, TypeDef { variants = vars }) =
    createFunc tconid `mapMaybe` vars

{- | Replace data constructor application with function application

  Turn I.App instances of the form (I.Data _ _) into (I.Var _ _)
-}
dataToApp :: I.Expr Poly.Type -> ArityFn (I.Expr Poly.Type)
dataToApp a@(I.Data dconid t) = do
  Just arity <- asks (M.lookup dconid)
  case arity of
    0 -> return a -- leave nullary data constructors alone
    _ -> return $ I.Var (nameFunc dconid) t
dataToApp a = pure a

{- | Create a top level function for each data constructor

  Returns Nothing for nullary data constructors, 
  which don't need top-level constructor functions.
-}
createFunc
  :: I.TConId
  -> (I.DConId, TypeVariant Poly.Type)
  -> Maybe (I.VarId, I.Expr Poly.Type)
  -- case of nullary dcon; guarantees params to be non-empty in the next pattern
createFunc _    (_     , VariantNamed []    ) = Nothing
createFunc tcon (dconid, VariantNamed params) = Just (func_name, lambda)
 where
  func_name = nameFunc dconid -- distinguish func name from fully applied dcon in IR
  lambda    = I.makeLambdaChain (first Just <$> params) body
  body      = I.zipApp dcon args
  dcon      = I.Data (fromId dconid) t
  args      = zip (reverse $ uncurry I.Var <$> params) (tconTyp : ts)
  tconTyp   = Poly.TCon tcon []
  (t : ts) =
    reverse $ foldl1 arrow . (++ [tconTyp]) <$> tail (inits (snd <$> params))
  {- Turns a list of types into arrow types representing nested lambdas
     @[Int, Int, Shape]@ becomes
     @[Int -> Int -> Shape, Int -> Shape, Shape]@
     @(t:ts)@ is permitted because params is always non-empty
     @tail@ is permitted because inits on a non-empty list always returns a list of at least two elements.
  -}
createFunc tcon (dcon, VariantUnnamed params) = createFunc
  tcon
  (dcon, VariantNamed argNames)
  where argNames = zipWith (\t i -> (nameArg i, t)) params [0 ..]

-- | Create a name for the constructor function
nameFunc :: I.DConId -> I.VarId
nameFunc dconid = fromString $ "__" ++ ident dconid

-- | Create a name for a constructor function argument
nameArg :: Int -> I.VarId
nameArg i = fromString ("__arg" ++ show i)

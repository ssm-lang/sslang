{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
-- | Turns unapplied and partially applied data constructors into calls to constructor functions.

module IR.DConToFunc
  ( dConToFunc
  ) where

import qualified Common.Compiler               as Compiler

import           Common.Compiler                ( MonadError )
import           Common.Identifiers             ( fromString
                                                , ident
                                                )
import           Control.Comonad                ( Comonad(extract) )
import           Control.Monad.Reader
import           Data.Bifunctor                 ( first )
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
                                                , peel
                                                , variantFields
                                                )

-- | Environment storing arity of each 'DCon' 
type ArityEnv = M.Map I.DConId Int

-- | Arity Reader Monad.
newtype ArityFn a = ArityFn (ReaderT ArityEnv Compiler.Pass a)
  deriving Functor                      via (ReaderT ArityEnv Compiler.Pass)
  deriving Applicative                  via (ReaderT ArityEnv Compiler.Pass)
  deriving Monad                        via (ReaderT ArityEnv Compiler.Pass)
  deriving MonadFail                    via (ReaderT ArityEnv Compiler.Pass)
  deriving (MonadError Compiler.Error)  via (ReaderT ArityEnv Compiler.Pass)
  deriving (MonadReader ArityEnv)       via (ReaderT ArityEnv Compiler.Pass)

-- | Run a computation within an arity environment
runArityFn :: ArityFn a -> Compiler.Pass a
runArityFn (ArityFn m) = runReaderT m M.empty

-- | Create a map of (DConId, Arity) key-value pairs from a list of type defintions
createArityEnv :: [(I.TConId, TypeDef Poly.Type)] -> ArityEnv
createArityEnv = foldl (\acc def -> acc <> insertArities def) M.empty
 where
  -- | Add a group of DCons to the arity environment
  insertArities :: (I.TConId, TypeDef Poly.Type) -> ArityEnv
  insertArities (_, TypeDef { variants = vars }) = foldl
    (\acc var -> acc <> insertArity var)
    M.empty
    vars
   where
    -- | Add single DCon to the arity environment
    insertArity :: (I.DConId, TypeVariant Poly.Type) -> ArityEnv
    insertArity (dconid, variant) =
      M.insert dconid (variantFields variant) M.empty

{- | 'dConToFunc' modifies programDefs and traverses the IR to accomplish three tasks:

  1) Add top-level constructor functions for each 'DCon' to progamDefs
  2) Turn unapplied data constuctors into calls to top level constructor funcs
  3) Turn partially applied data constructors into calls to top level constructor funcs
-}
dConToFunc :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
dConToFunc p@I.Program { I.programDefs = defs, I.typeDefs = tDefs } =
  runArityFn $ do
    --defs' <- local (createArityEnv tDefs <>) (mapM (second dataToApp) defs)
    defs' <- local (createArityEnv tDefs <>) (mapM dataToAppW defs)
    return p { I.programDefs = defs' ++ concat (createFuncs <$> tDefs) }


{- | Worked example of ADT definition and corresponding constructor functions

type Shape 
  Square Int 
  Rect Int Int

  Every top-level function has the form (I.VarId, I.Expr Poly.Type) = (functionName, functionBody)
  The function body is a lambda expression representing a "call" to a fully applied data constructor.

  Let's first consider the top-level func for Square:
  (Square, body) 
  body = fun arg0 { App L R t } : Int -> Shape
   where L = Square : Int -> Shape
         R = arg0 : type Int
         t = Shape, because the type of a fully applied data constructor is its type constructor

  Next consider the top-level func for Rect:
  (Rect, body*) *Notice the body of the lambda contains another lambda (curried arguments),
                 and the Application expression contains another Application (curried arguments).  
  body = fun arg0 { fun arg1 { App L R t } : Int -> Shape } : Int -> Int -> Shape
   where L = App L2 R2 t
         R = arg1 : Int
         t = Shape, because the type of a fully applied data constructor is its type constructor
          where L2 = Rect : Int -> Int -> Shape
                R  = arg0 : Int
                t = Int -> Shape, because at this point in the inner App, Rect is partially applied with only 1 arg.

-}

-- | Create a group of top level constructor functions for each type constructor
createFuncs :: (I.TConId, TypeDef Poly.Type) -> [(I.VarId, I.Expr Poly.Type)]
createFuncs (tconid, TypeDef { variants = vars }) =
  createFunc tconid `mapMaybe` vars
 where
  {- | Create a top level function for each data constructor

    Returns Nothing for nullary data constructors, 
    which don't need top-level constructor functions.
  -}
  createFunc
    :: I.TConId
    -> (I.DConId, TypeVariant Poly.Type)
    -> Maybe (I.VarId, I.Expr Poly.Type)
  -- case of nullary dcon; guarantees params to be non-empty in the next pattern
  createFunc _    (_                     , VariantNamed []    ) = Nothing
  createFunc tcon (dconid@(I.DConId dcon), VariantNamed params) = Just
    (I.VarId func_name, lambda)
   where
    func_name = fromString $ "__" ++ ident dcon -- distinguish name from fully applied dcon in IR
    lambda    = I.makeLambdaChain names body
    names     = first Just <$> params
    body      = I.makeAppChain (tail args) initApp initTyp
     where
       -- create the inner-most App node first
       -- initApp always has the form App DConId arg0 : arg0 -> TConId
      initApp =
        I.App (I.Data dconid dconTyp) arg0 (arrow (extract arg0) tconTyp) -- arg0 -> TConId
      args@(arg0 : _) = uncurry I.Var <$> params -- guaranteed to be non-empty
      Just initTyp    = peel dconTyp
      tconTyp         = Poly.TCon tcon []
      dconTyp         = foldl1 arrow $ (snd <$> params) ++ [tconTyp]
  createFunc tcon (dcon, VariantUnnamed params) = createFunc
    tcon
    (dcon, VariantNamed mangledNames)
   where
    mangledNames = zipWith mangle params [0 ..]
    mangle :: t -> Int -> (I.VarId, t)
    mangle t i = (I.VarId $ fromString ("__arg" ++ show i), t)


{- | Replace data constructor application with function application

Turn I.App instances of the form (I.Data _ _) into (I.Var _ _) where needed

Case 1: Unapplied Data Constructors that look like Nullary Data Constructors
Case 2: Partially Applied Data Constructors

-}

{-

  | Data DConId t
  {- ^ @Data d t@ is a data constructor named @n@ of type @t@. -}
  | Lit Literal t
  {- ^ @Lit l t@ is a literal value @l@ of type @t@. -}
  | App (Expr t) (Expr t) t

-}
-- dataToApp :: I.Expr Poly.Type -> ArityFn (I.Expr Poly.Type)
-- --dataToApp l@(I.Data dconid t) = pure l
-- --dataToApp (I.App l@(I.Data dconid t2) r t) = 
-- dataToApp (I.App l@(I.App _ _ _) r t) = do 
--   l' <- dataToApp l
--   return (I.App l' r t)
-- dataToApp a = pure a






-- dataToApp (I.App l@(I.App {}) r t) = do
--  -- l' <- dataToApp l
--   return (I.App l r t)
-- dataToApp = pure -- stub

dataToAppW
  :: (I.VarId, I.Expr Poly.Type) -> ArityFn (I.VarId, I.Expr Poly.Type)
--dataToAppW = pure
dataToAppW (v, e) = do
  e' <- dataToApp e
  return (v, e')


dataToApp :: I.Expr Poly.Type -> ArityFn (I.Expr Poly.Type)
dataToApp a@(I.Data _ _) = fst <$> helper (a, 0)
dataToApp (I.App left right t) =
  I.App
    <$> (fst <$> helper (left, 1))
    <*> (fst <$> helper (right, 0))
    <*> pure t
dataToApp (I.Let args body t) =
  I.Let <$> mapM recurse args <*> dataToApp body <*> pure t
  where recurse = \(b, e) -> (,) b <$> dataToApp e
dataToApp (I.Lambda b e t) = I.Lambda b <$> dataToApp e <*> pure t
dataToApp (I.Match scrut arms t) =
  I.Match <$> dataToApp scrut <*> mapM recurse arms <*> pure t
  where recurse = \(b, e) -> (,) b <$> dataToApp e
dataToApp (I.Prim p es t) = I.Prim p <$> mapM dataToApp es <*> pure t
dataToApp a               = pure a

{-
  
  | Let [(Binder, Expr t)] (Expr t) t
  {- ^ @Let [(n, v)] b t@ binds value @v@ to variable @v@ in its body @b@.

  The bindings list may only be of length greater than 1 for a set of mutually
  co-recursive functions.
  -}
  | Lambda Binder (Expr t) t
  {- ^ @Lambda v b t@ constructs an anonymous function of type @t@ that binds
  a value to parameter @v@ in its body @b@.
  -}
  | Match (Expr t) [(Alt, Expr t)] t
  {- ^ @Match s alts t@ pattern-matches on scrutinee @s@ against alternatives
  @alts@, each producing a value of type @t@.
  -}
  | Prim Primitive [Expr t] t

-}

helper :: (I.Expr Poly.Type, Int) -> ArityFn (I.Expr Poly.Type, Int)
helper (w@(I.Data dconid t), depth) = do
  Just arity <- asks (M.lookup dconid)
  if arity <= depth
    then return (w, depth)
    -- then fail
    --   (  "arity of "
    --   ++ show arity
    --   ++ " <= "
    --   ++ show (depth)
    --   ++ "; "
    --   ++ ident dconid
    --   ++ "\n"
    --   )
    else pure (I.Var (I.VarId $ fromString $ "__" ++ ident dconid) t, depth)

helper (I.App l r t, depth) =
  (, depth)
    <$> (   I.App
        <$> (fst <$> helper (l, depth + 1))
        <*> (fst <$> helper (r, depth))
        <*> pure t
        )

  -- (,)
  --   <$> (   I.App
  --       <$> (fst <$> helper (l, depth + 1))
  --       <*> (fst <$> helper (r, depth))
  --       <*> pure t
  --       )
  --   <*> pure depth
  --(fst <$>) <*>(fst <$>) <*>
  -- do
  -- (l', _) <- helper (l, depth + 1)
  -- (r', _) <- helper (r, depth)
  -- pure (I.App l' r' t, depth)
helper e = pure e

-- helper ( I.App l r t, depth) = do
--   let l' = case l of
--              I.App _ _ _ -> helper (l,depth+1)
--              _ -> do return (l,0)
--   let r' = case r of
--              I.App _ _ _ -> helper (r,depth)
--              _ -> do return (r,0)
--   (l'',ld) <- l'
--   (r'',rd) <- r'
--   pure (I.App l'' r'' t, 1)
--  -- pure w
  -- (l',ld) <- case l of 
  --              I.App _ _ _ -> helper (l,depth+1)
  --              _ -> do return (l,0)

-- helper depth w@(I.App (I.Data dconid t2) r t) = do
--       Just arity <- asks (M.lookup dconid)
--       if arity <= depth + 1 
--       then return (w,0)
--       else do
--            let func = I.VarId $ fromString $ "__" ++ ident dconid
--            return (I.App (Var func t2) r t, 1)
-- helper depth (I.App l@I.App {} r t) = do
--       (l', depth') <- helper (depth+1) l
--       return (I.App l' r t, depth')
-- helper d e = pure (e,d)

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

import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT(..)
                                                , asks
                                                )
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import           Data.Generics.Aliases          ( mkM )
import           Data.Generics.Schemes          ( everywhereM )
import           Data.List                      ( inits )
import qualified Data.Map                      as M
import           Data.Maybe                     ( mapMaybe )
import qualified IR.IR                         as I
import qualified IR.Types                      as I

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
runArityFn :: [(I.TConId, I.TypeDef)] -> ArityFn a -> Compiler.Pass a
runArityFn tds (ArityFn m) = runReaderT m $ M.fromList env
  where env = concatMap (map (second I.variantFields) . I.variants . snd) tds


{- | 'dConToFunc' modifies programDefs and traverses the IR to accomplish two tasks:

  (1) Add top-level constructor functions for each non-nullary 'DCon' to progamDefs
  2. Turn non-nullary data constuctors into calls to top level constructor funcs
-}
dConToFunc :: I.Program I.Type -> Compiler.Pass (I.Program I.Type)
dConToFunc p@I.Program { I.programDefs = defs, I.typeDefs = tDefs } =
  runArityFn tDefs $ do
    defs'' <- defs' -- user defined functions
    return p { I.programDefs = tDefs' ++ defs'' } -- constructor funcs ++ user funcs
 where
  tDefs' = concat (createFuncs <$> tDefs) -- top-level constructor functions
  defs' =
    filter ((`notElem` (fst <$> tDefs')) . fst)
      <$> everywhereM (mkM dataToApp) defs
  -- We need to filter defs to account for name collisions
  -- between user defined funcs and newly created and inserted constructor funcs
  {- Example of name collision case and correction
  Given the SSLANG program
  @
  type Color
    White
    Black
    RGB Int Int Int

  main ( cout : & Int ) -> () = ()
  @
  dConToFunc will produce
  @
  type Color
    White
    Black
    RGB Int Int Int

  __RGB (__arg0 : Int) (__arg1 : Int) (__arg2 : Int) -> Color = 
  RGB __arg0 __arg1 __arg2
    
  main ( cout : & Int ) -> () = ()
  @
  This is okay.
  Now, given the SSLANG program
  @
  type Color
    White
    Black
    RGB Int Int Int

  __RGB (r : Int) (g : Int) (b : Int) -> Color = 
  RGB r (g+2) b
    
  main ( cout : & Int ) -> () = ()
  @
  dConToFunc (without filtering defs!) produces
  @
  type Color
    White
    Black
    RGB Int Int Int

  __RGB (__arg0 : Int) (__arg1 : Int) (__arg2 : Int) -> Color = 
  RGB __arg0 __arg1 __arg2

  __RGB (r : Int) (g : Int) (b : Int) -> Color = 
  RGB r (g+2) b
    
  main ( cout : & Int ) -> () = ()
  @
  This is not okay, because there are two functions named > __RGB.
  To prevent this duplicate function case, we search defs for any user defined functions
  that have the same name as our newly inserted constructor functions, and remove them.
  @
  // defs = [ (__RGB, ...), (main, ...) ]
  // tDefs' = [ (__RGB,...) ]
  // If defs contains func w/ same name as a func in tDefs', remove it!
  // defs' = [ (main, ...) ]
  @
  dConToFunc (with filtering of defs) produces
  @
  type Color
    White
    Black
    RGB Int Int Int

  __RGB (r : Int) (g : Int) (b : Int) -> Color = 
  RGB r (g+2) b
    
  main ( cout : & Int ) -> () = ()
  @
  Now this is okay.
    -}
  createFuncs (tconid, I.TypeDef { I.variants = vars }) =
    createFunc tconid `mapMaybe` vars

{- | Replace data constructor application with function application

  Turn I.App instances of the form (I.Data _ _) into (I.Var _ _)
-}
dataToApp :: I.Expr I.Type -> ArityFn (I.Expr I.Type)
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
  :: I.TConId -> (I.DConId, I.TypeVariant) -> Maybe (I.VarId, I.Expr I.Type)
  -- case of nullary dcon; guarantees params to be non-empty in the next pattern
createFunc _    (_     , I.VariantNamed []    ) = Nothing
createFunc tcon (dconid, I.VariantNamed params) = Just (func_name, lambda)
 where
  func_name = nameFunc dconid -- distinguish func name from fully applied dcon in IR
  lambda    = I.foldLambda (first Just <$> params) body
  body      = I.foldApp dcon args
  dcon      = I.Data (fromId dconid) t
  args      = reverse $ zip (uncurry I.Var <$> params) ts
  tconTyp   = I.TCon tcon []
  (t : ts)  = reverse $ foldr1 I.Arrow . reverse <$> tail
    (inits $ reverse ((snd <$> params) ++ [tconTyp]))
  {- Turns a list of types into arrow types representing nested lambdas
     @[Int, Int, Shape]@ becomes
     @[Int -> Int -> Shape, Int -> Shape, Shape]@
     @(t:ts)@ is permitted because params is always non-empty
     @tail@ is permitted because inits on a non-empty list always returns a list of at least two elements.
  -}
createFunc tcon (dcon, I.VariantUnnamed params) = createFunc
  tcon
  (dcon, I.VariantNamed argNames)
  where argNames = zipWith (\t i -> (nameArg i, t)) params [0 ..]

-- | Create a name for the constructor function
nameFunc :: I.DConId -> I.VarId
nameFunc dconid = fromString $ "__" ++ ident dconid

-- | Create a name for a constructor function argument
nameArg :: Int -> I.VarId
nameArg i = fromString ("__arg" ++ show i)

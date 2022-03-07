{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- | Turns unapplied and partially applied data constructors into constructor functions.

module IR.DConToFunc
  ( dConToFunc
  ) where

import qualified Common.Compiler               as Compiler

import           Common.Identifiers             ( fromString
                                                , ident
                                                )
import           Control.Comonad                ( Comonad(extract) )
import           Data.Bifunctor                 ( first )
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
                                                )

dConToFunc :: I.Program Poly.Type -> Compiler.Pass (I.Program Poly.Type)
dConToFunc p@I.Program { I.programDefs = defs, I.typeDefs = tDefs } =
  pure p { I.programDefs = defs ++ concat (createFuncs <$> tDefs) }


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
    func_name = fromString $ "__" ++ ident dcon
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
  createFunc tcon (I.DConId dcon, VariantUnnamed params) = createFunc
    tcon
    (I.DConId dcon, VariantNamed mangledNames)
   where
    mangledNames = zipWith mangle params [0 ..]
    mangle :: t -> Int -> (I.VarId, t)
    mangle t i = (I.VarId $ fromString ("__arg" ++ show i), t)


{- | Replace data constructor application with function application

Turn I.App instances of the form (I.Data _ _) into (I.Var _ _)
-}

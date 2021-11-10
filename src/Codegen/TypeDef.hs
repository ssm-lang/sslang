
{-# LANGUAGE QuasiQuotes #-}
module Codegen.TypeDef where

import qualified IR.Types.Flat                 as L
import qualified IR.Types.TypeSystem           as L

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import Common.Identifiers (DConId, ident, fromString)
import IR.Types.TypeSystem(TypeVariant)
import Codegen.Identifiers
import Data.Char(toLower)
import Data.List(intercalate)


-- | Generate definitions for SSM type definitions.
genTypeDef :: L.TypeDef L.Type -> (C.Definition,[C.Definition])
genTypeDef (L.TypeDef dCons _ ) =   (x, y)
 where x = [cedecl| typedef struct {
                       $sdecls:(map structField $ genGCHeader)
                       union {
                        $sdecls:(structFieldPtr <$> extractDConDecl <$> dCons) 
                       } $id:payload; 
                     } $id:myTConId;     
       |]
  
       myTConId = "Dummy" --change this later!!
       genGCHeader = [(tag,u_char),(ref_count,u_char)]
 
       structField :: (CIdent, C.Type) -> C.FieldGroup
       structField (n, t) = [csdecl|$ty:t $id:n;|]

       structFieldPtr :: (CIdent, C.Type) -> C.FieldGroup
       structFieldPtr (n, t) = [csdecl|$ty:t* $id:n;|]

       extractDConDecl :: (DConId,TypeVariant L.Type) -> (CIdent, C.Type)
       extractDConDecl (a,_) = (n,t) 
        where n = CIdent $ fromString $ lower a 
              t = ctype $ CIdent $ fromString $ intercalate "_" [myTConId, ident a]
  
       -- | Helper that turns a DConId into a string 
       -- | and makes the first letter lowercase 
       lower :: DConId -> String
       lower d
        | null c = ""
        | otherwise = toLower h:t
        where c@(h:t) = ident d
       y = []

       
--genTypeDef = error "Not yet implemented"  snakeConcat :: String -> DConId -> String
  -- snakeConcat a b = a ++ "_" ++ lower b
-- import           Data.Bifunctor                 ( Bifunctor(bimap,first) )
--(L.TypeDef dCons _) (L.TCon typeName)
--TConId is the name of the type
--L.Type breaks into a TCon and TConid (this is how i know name of struct)
--TypeDef cotnains the name of data constructors and each data cosntructor's named and unnamed fields and types
-- args to data constructors are listed as tuples of (fieldname, typeName) OR just typNamee if unnamed

{-

  = TBuiltin (Builtin Type) -- ^ Builtin types
  | TCon TConId 

struct Tree {
	unsigned char tag;            //GC header field
	unsigned char num_ptrs;       //GC header field
	unsigned char refcount;       //GC header field
	union Payload{
		Tree_twochildren twochildren;
		Tree_onechild onechild;	
		//Don't need a Leaf or MarkedLeaf option because these are packed integers!
	}payload;
};

-}

{-------------------------------- NOTES  -----------------------------------------
From TypeSystem.hs:

{- | The type definition associated with a type constructor.

A definition for `data MyList a = Cons a (MyList a) | Nil` looks like:

@
  TypeDef { arity = 1
          , [ ("Cons", VariantUnnamed [TVar 0, TCon ("MyList" [TVar 0])])
            , ("Nil", VariantUnnamed [])
            ]
          }
@

(Data constructors for identifiers are omitted for brevity.)

Note that for a flat type system, where all type constructors are nullary, arity
will just be set to 0.
-}
data TypeDef t = TypeDef
  { variants :: [(DConId, TypeVariant t)]
  , arity    :: Arity
  }
  deriving Show

-- | Arguments to a data constructor, whose fields may or may not be named
data TypeVariant t
  = VariantNamed [(FieldId, t)] -- ^ A record with named fields
  | VariantUnnamed [t]          -- ^ An algebraic type with unnamed fields
  deriving (Show, Eq)

instance Functor TypeDef where
  fmap f TypeDef { variants = vs, arity = a } =
    TypeDef { variants = fmap (second $ fmap f) vs, arity = a }

instance Functor TypeVariant where
  fmap f (VariantNamed   fs) = VariantNamed $ fmap (second f) fs
  fmap f (VariantUnnamed fs) = VariantUnnamed $ fmap f fs

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From flat.hs:
-- | The language of type expressions, e.g., what appears in a type signature.
data Type
  = TBuiltin (Builtin Type) -- ^ Builtin types
  | TCon TConId             -- ^ Type constructors
  deriving (Eq, Show)

instance TypeSystem Type where
  projectBuiltin = TBuiltin
  injectBuiltin (TBuiltin t) = Just t
  injectBuiltin _            = Nothing

instance Pretty Type where
  pretty (TBuiltin a) = pretty a
  pretty (TCon     a) = pretty a

-- | Flatten a list of type constructors into a
flattenApp :: TConId -> [Type] -> TConId
flattenApp t [] = t
flattenApp t ts =
  error $ "flat cat: " ++ show t ++ " and " ++ concatMap show ts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From poly.hs:
-- | The language of type expressions, e.g., what appears in a type signature.
data Type
  = TBuiltin (Builtin Type)         -- ^ Builtin types
  | TCon TConId [Type]              -- ^ Type constructor, e.g., @Option '0@
  | TVar TVarIdx                    -- ^ Type variables, e.g., @'0@
  deriving Eq
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{- | Generate struct definition for an SSM 'Procedure'.
This is where local variables, triggers, and parameter values are stored.
-}
genStruct :: GenFn C.Definition
genStruct = do
  name   <- gets fnName
  params <- gets fnParams
  locs   <- gets fnLocs
  trigs  <- gets fnMaxWaits

  return [cedecl|
    typedef struct {
      $ty:act_t $id:act_member;

      $sdecls:(map structField $ genParams params)
      $sdecls:(map structField $ genLocals locs)
      $sdecls:(map structField $ genTrigs trigs)

    } $id:(act_ name);
  |]
 where
  structField :: (CIdent, C.Type) -> C.FieldGroup
  structField (n, t) = [csdecl|$ty:t $id:n;|]

-----------------------------------------------------------------------------------}
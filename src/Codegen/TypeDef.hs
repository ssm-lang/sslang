{-# LANGUAGE QuasiQuotes #-}
module Codegen.TypeDef where

import qualified IR.Types.Flat                 as L
import qualified IR.Types.TypeSystem           as L

import           Language.C.Quote.GCC
import qualified Language.C.Syntax             as C

import Common.Identifiers (DConId, TConId, ident, fromString)

import Codegen.Identifiers
import Data.Char(toLower)
import Data.List(partition,maximumBy)
import Data.Bifunctor(second)


{-

// data MyBool = MyTrue int | MyFalse int int 

enum myBoolTag {MyTrueTag = 0, MyFalseTag};

struct MyBool{
	struct ssm_mm_md header;
	struct ssm_object_t* payload[2]; //max number of fields	
};

data Type
  = TBuiltin (Builtin Type) -- ^ Builtin types
  | TCon TConId             -- ^ Type constructors
  deriving (Eq, Show)
data TypeDef t = TypeDef
  { variants :: [(DConId, TypeVariant t)]
  , arity    :: Arity
  }
  deriving Show




    = VariantNamed [(FieldId, t)] -- ^ A record with named fields
  | VariantUnnamed [t]          -- ^ An algebraic type with unnamed fields
  maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a 

  enum
    An enum member. The argument must have type CEnum.
enums
    An list of enum members. The argument must have type [CEnum].
     $sdecls:(structField <$> snd (foldl extractFieldDecl (0,[]) typeNamePairs))
     structField :: (CIdent, C.Type) -> C.FieldGroup
-}



-- | Generate definitions for SSM type definitions.
genTypeDef :: TConId -> L.TypeDef L.Type -> (C.Definition, C.Definition)
genTypeDef tconid (L.TypeDef dCons _ ) =   (tags, structDef)
 where structDef = [cedecl| typedef struct {
                       $ty:ssm_mm_md* $id:header;
                       $ty:ssm_object_t* $id:payload[1]; 
                     } $id:tconid;     
                   |]
       tags = [cedecl| enum $id:enumName {
                          dummy = 0, dummy1, dummy2
                       };  
             |]
       (big, small) = partition (\(_,sz)-> sz >= 32) $ augmentWSize <$> dCons
       (packed, medium) = smallEnough small
       maxNumFields = maximumBy (\(_,sz1) (_,sz2) -> sz1 `compare` sz2)  (big ++ medium) 
       enumName = CIdent $ fromString $ lower $ ident tconid
       tagged = small ++ big ++ medium

       -- | categorize data constructor as either small enough or too big  to be represented in 31 bits 
       smallEnough :: [((DConId, L.TypeVariant L.Type),Int)] ->
                   ([((DConId, L.TypeVariant L.Type),Int)], [((DConId, L.TypeVariant L.Type),Int)])
       smallEnough candidates = partition (\(_,sz)-> (sz+tagBits) < 32 ) candidates
                          where tagBits = q+r where (q,r) = quotRem (length candidates) 2

                   
       -- | augment data constructor with size info in bits
       augmentWSize:: (DConId, L.TypeVariant L.Type) -> ((DConId, L.TypeVariant L.Type),Int)
       augmentWSize (dconid, fields) = ((dconid, fields), dConSize fields)

       -- | return the size in bits of a given data constructor
       dConSize :: (L.TypeVariant L.Type) -> Int
       dConSize (L.VariantNamed fields) = sum $ fieldSize.snd <$> fields
       dConSize (L.VariantUnnamed fields) = sum $ fieldSize <$> fields

       -- | return the size in bits of a given field
       -- | assume all built-in types are one word for now
       fieldSize:: L.Type -> Int
       fieldSize (L.TBuiltin _) = 32
       fieldSize (L.TCon _) = 32

       -- | Helper that makes the first letter lowercase 
       lower :: String -> String
       lower "" = ""
       lower (h:t) = toLower h:t
       
 





-- | Determine which (if any) data constructors of the ADT 
-- | will be represented as a word vs an object on heap




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
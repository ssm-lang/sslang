{-# LANGUAGE OverloadedStrings #-}

module IR.Constraint.Constraint where

import qualified Common.Identifiers            as Ident
import qualified Data.Map.Strict               as Map
import qualified IR.Constraint.Type            as Type
import qualified IR.Constraint.UnionFind       as UF


-- | CONSTRAINTS

data Constraint
  = CTrue
  | CSaveTheEnvironment
  | CEqual Type Type
  | CPattern Type Type
  | CLocal Ident.VarId Type
  | CForeign Ident.VarId Type.Scheme Type
  | CAnd [Constraint]
  | CLet { _rigidVars :: [Variable]
         , _flexVars :: [Variable]
         , _header :: Map.Map Ident.VarId Type
         , _headerCon :: Constraint
         , _bodyCon :: Constraint
         }

exists :: [Variable] -> Constraint -> Constraint
exists flexVars constraint = CLet [] flexVars Map.empty constraint CTrue


-- | TYPE PRIMITIVES

type Variable = UF.Point Descriptor

data FlatType = TCon1 Ident.TConId [Variable]

data Type
  = TConN Ident.TConId [Type]
  | TVarN Variable


-- | DESCRIPTORS

data Descriptor = Descriptor
  { _content :: Content
  , _rank    :: Int
  , _mark    :: Mark
  , _copy    :: Maybe Variable -- for instantiation
  }

data Content
  = FlexVar (Maybe Ident.TVarId)
  | RigidVar Ident.TVarId
  | Structure FlatType
  | Error

mkDescriptor :: Content -> Descriptor
mkDescriptor content = Descriptor content noRank noMark Nothing


-- | RANKS

-- No rank means that the variable is generic
noRank :: Int
noRank = 0

-- Outermost rank means that we have not entered header of any CLet
outermostRank :: Int
outermostRank = 1


-- | MARKS

newtype Mark = Mark Int
  deriving (Eq, Ord)

noMark :: Mark
noMark = Mark 2

-- occursMark :: Mark
-- occursMark = Mark 1

-- getVarNamesMark :: Mark
-- getVarNamesMark = Mark 0

{-# INLINE nextMark #-}
nextMark :: Mark -> Mark
nextMark (Mark mark) = Mark (mark + 1)


-- | BUILT-IN TYPES

infixr 0 ==>
(==>) :: Type -> Type -> Type
(==>) t1 t2 = TConN "->" [t1, t2]

unit :: Type
unit = TConN "()" []

ref :: Type -> Type
ref a = TConN "&" [a]

list :: Type -> Type
list a = TConN "[]" [a]

time :: Type
time = TConN "Time" []

i64 :: Type
i64 = TConN "Int64" []

u64 :: Type
u64 = TConN "UInt64" []

i32 :: Type
i32 = TConN "Int32" []

u32 :: Type
u32 = TConN "UInt32" []

i16 :: Type
i16 = TConN "Int16" []

u16 :: Type
u16 = TConN "UInt16" []

i8 :: Type
i8 = TConN "Int8" []

u8 :: Type
u8 = TConN "UInt8" []


-- | MAKE FLEX VARIABLES

mkFlexVar :: IO Variable
mkFlexVar = UF.fresh flexVarDescriptor

flexVarDescriptor :: Descriptor
flexVarDescriptor = mkDescriptor unnamedFlexVar

unnamedFlexVar :: Content
unnamedFlexVar = FlexVar Nothing


-- | MAKE NAMED VARIABLES
nameToFlex :: Ident.TVarId -> IO Variable
nameToFlex name = UF.fresh $ mkDescriptor $ FlexVar (Just name)

nameToRigid :: Ident.TVarId -> IO Variable
nameToRigid name = UF.fresh $ mkDescriptor $ RigidVar name

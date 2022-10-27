module IR.Constraint.Type where

import qualified Common.Identifiers            as Ident
import qualified Data.Map.Strict               as Map
import qualified IR.Constraint.UnionFind       as UF

data Constraint
  = CTrue
  | CEqual Type Type
  | CInstance Ident.VarId Type
  | CAnd [Constraint]
  | CLet { _rigidVars :: [Variable]
         , _flexVars :: [Variable]
         , _header :: Map.Map Ident.VarId Type
         , _headerCon :: Constraint
         , _bodyCon :: Constraint
         }

type Variable = UF.Point Descriptor

data FlatType = TCon1 Ident.TConId [Variable]

data Type
  = TConN Ident.TConId [Type]
  | TVarN Variable

data Descriptor = Descriptor

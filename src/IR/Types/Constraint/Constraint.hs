{-# LANGUAGE GADTs #-}

{- | Constraint definition and related utilities -}

module IR.Types.Constraint.Constraint where
import IR.Types.Constraint.Type (UType)
import IR.Types.Type (Type, Scheme)
import Common.Identifiers (VarId(..), TVarId (..))

newtype CVar = CVar Int
 deriving (Eq, Show)

data Co a where
  CTrue     :: Co ()
  CMap      :: Co a -> (a -> b) -> Co b
  CPure     :: a -> Co a
  CConj     :: Co a -> Co b -> Co (a, b)
  CEq       :: CVar -> CVar -> Co ()
  CExist    :: CVar -> Maybe (UType CVar) -> Co a -> Co a
  CDecode   :: CVar -> Co Type
  CInstance :: VarId -> CVar -> Co (Scheme, [Type])
  CDef      :: VarId -> CVar -> Co a -> Co a
  CLet      :: [CVar] -> [VarId] -> [CVar] -> Co a -> Co b -> Co ([TVarId], [Scheme], a, b)

instance Show (Co a) where
  show CTrue             = "(CTrue)"
  show (CMap c _       ) = "(CMap " ++ show c ++ ")"
  show (CPure _        ) = "(CPure)"
  show (CConj c1 c2    ) = "(" ++ show c1 ++ " ^& " ++ show c2 ++ ")"
  show (CEq   v1 v2    ) = "(CEq " ++ show v1 ++ " " ++ show v2 ++ ")"
  show (CExist v _ c   ) = "(CExist " ++ show v ++ " " ++ show c ++ ")"
  show (CDecode v      ) = "(CDecode " ++ show v ++ ")"
  show (CInstance vid v) = "(CInstance " ++ show vid ++ " " ++ show v ++ ")"
  show (CDef vid v c) =
    "(CDef " ++ show vid ++ " " ++ show v ++ " " ++ show c ++ ")"
  show (CLet vs vids vs' c1 c2) =
    "(CLet "
      ++ show vs
      ++ " "
      ++ show vids
      ++ " "
      ++ show vs'
      ++ " "
      ++ show c1
      ++ " "
      ++ show c2
      ++ ")"

instance Functor Co where
  fmap f c = CMap c f

instance Applicative Co where
  pure = CPure
  mf <*> mx = fmap (\(f, x) -> f x) (CConj mf mx)

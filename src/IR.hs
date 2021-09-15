{-|
Module: IR
Description: The compiler intermediate representation, designed to be
translated into C

-}

module IR where

import Data.Text.Prettyprint.Doc
import Prelude hiding ((<>), id, LT, GT)

import Duration

data Program = Program [Function]

data Function = Function Bind [Bind] [Bind] [Statement]

data Bind = Bind String Ty    -- a : int

data Ty = TCon String   -- Type constructors such as Int, Real, Ref, Sched
        | TApp Ty Ty

newtype Lab = Lab Int   -- Numbered labels for gotos

data Statement = Assign Bind Expression -- a = 42 + b
               | After Expression Bind Expression -- after 1ms a = 10
               | Wait [Bind]
               | Label Lab
               | Goto Lab
               | IfGoto Expression Lab
               | Fork [(Bind, [Expression])]
               | Verbatim String

data Expression = Var Bind       -- a 
                | IntLit Integer Ty -- 42
                | DurLit Duration Ty -- 42 ms
                | Unary UnOp Expression Ty -- not a
                | Binary Expression BinOp Expression Ty -- a + 42

data UnOp = Not

data BinOp = Add
           | Sub
           | Mult
           | LT
           | LE
           | GT
           | GE
           | NE

initialLabel :: Lab
initialLabel = Lab 0

nextLabel :: Lab -> Lab
nextLabel (Lab n) = Lab (succ n)

instance Show Lab where
  show (Lab n) = "L" ++ show n

-- | Type constructor for references (for pass-by-value variables)
refTCon :: Ty
refTCon = TCon "Ref"

ref :: Ty -> Ty
ref t = TApp refTCon t

-- | Determine if an object is a reference: stripRef "Ref t" = Just t
stripRef :: Ty -> Maybe Ty
stripRef (TApp (TCon "Ref") t) = Just t
stripRef _ = Nothing

-- | Type constructor for scheduled variables
-- (i.e., can be future assigned, waited on)
schedTCon :: Ty
schedTCon = TCon "Sched"

sched :: Ty -> Ty
sched t = TApp schedTCon t

-- | Determine if an object's type is scheduled: stripSched "Sched t" = Just t
stripSched :: Ty -> Maybe Ty
stripSched (TApp (TCon "Sched") t) = Just t
stripSched _ = Nothing

-- | Type constructor for integer variables
intTy :: Ty
intTy = TCon "Int"

-- | Type constructor for Boolean variables
boolTy :: Ty
boolTy = TCon "Bool"

-- | Type constructor for pure events
eventTy :: Ty
eventTy = TCon "Event"

-- | Type constructor for the unit type
unitTy :: Ty
unitTy = TCon "Unit"

-- | Type constructor for Time
timeTy :: Ty
timeTy = TCon "Time"

-- | Type constructor for the function type
functionTy :: Ty
functionTy = TCon "->"

-- | functionType [a,b,c] = a -> (b -> c) = (-> a) ((-> b) c) = (-> a) (-> b) c
functionType :: [Ty] -> Ty
functionType = foldr1 (\a t -> TApp (TApp functionTy a) t)

-- | functionArgTypes a -> (b -> c) = [a,b,c]
functionArgsTypes :: Ty -> [Ty]
functionArgsTypes (TApp (TApp (TCon "->") a) t) = a : functionArgsTypes t
functionArgsTypes t = [t]

-- | Return the type of an expression
exprType :: Expression -> Ty
exprType (Var (Bind _ t)) = t
exprType (IntLit _ t)     = t
exprType (DurLit _ t)     = t
exprType (Unary _ _ t)    = t
exprType (Binary _ _ _ t) = t

instance Show Program where
  show p = show $ pretty p

instance Pretty Program where
  pretty (Program fs) =
    concatWith (\x y -> x <> line <> line <> y) (map pretty fs)

instance Pretty Function where
  pretty (Function bind@(Bind fn _) formals locals body) =
    vsep [pretty bind, def]
    where def = nest 2 $ vsep $ (pretty fn <> tupled (map pretty formals) ) :
                                map (\l -> pretty "var " <> pretty l) locals ++
                                map pretty body

instance Pretty Bind where
  pretty (Bind n t) = pretty n <+> colon <+> pretty t

instance Pretty Statement where
  pretty (Assign n e) = pretty n <+> pretty '=' <+> pretty e
  pretty (After e1 n e2) = pretty "after" <+> pretty e1 <+>
        pretty n <+> pretty '=' <+> pretty e2
  pretty (Wait ids) = pretty "wait" <+>
                      hsep (punctuate comma $ map pretty ids)
  pretty (Label l) = pretty "label" <+> pretty (show l)
  pretty (Goto l) = pretty "goto" <+> pretty (show l)
  pretty (IfGoto e l) = pretty "if" <+> pretty e <+>
                        pretty "goto" <+> pretty (show l)
  pretty (Fork cs) = pretty "fork" <+>
     hsep (map (\(b, as) -> parens (pretty b) <> tupled (map pretty as)) cs)
  pretty (Verbatim s) = pretty "#{" <+> pretty s <+> pretty "}#"

instance Pretty Expression where
  pretty (Var v) = pretty v
  pretty (IntLit i ty) = pretty i <+> pretty ':' <+> pretty ty
  pretty (DurLit d ty) = pretty (show d) <+> pretty ':' <+> pretty ty
  pretty (Unary op e ty) = pretty op <+> parens (pretty e) <+>
                           pretty ':' <+> pretty ty
  pretty (Binary e1 op e2 ty) = parens (pretty e1) <+> pretty op <+>
                                parens (pretty e2) <+>
                                pretty ':' <+> pretty ty

instance Pretty UnOp where
  pretty Not = pretty "not"

instance Pretty BinOp where
  pretty Add = pretty '+'
  pretty Sub = pretty '-'
  pretty Mult = pretty '*'
  pretty LT = pretty '<'
  pretty LE = pretty "<="
  pretty GT = pretty '>'
  pretty GE = pretty ">="
  pretty NE = pretty "/="

parensIf :: Bool -> Doc ann -> Doc ann
parensIf False d = d
parensIf True d = parens d

{- 
  2: TCon 
  1: TApp  (left-to-right)
  0: TApp  -> (left-to-right)
  -}
instance Pretty Ty where
  pretty = pt 0
    where
      pt :: Int -> Ty -> Doc ann
      pt p (TApp (TApp (TCon "->") t1) t2) = parensIf (p > 0) $
        pt 1 t1 <+> pretty "->" <+> pt 0 t2
      pt p (TApp t1 t2) = parensIf (p > 1) $
        pt 2 t1 <+> pt 1 t2
      pt _ (TCon c) = pretty c


instance Show Ty where
  show = show . pretty

instance Show Expression where
  show = show . pretty

instance Show BinOp where
  show = show . pretty


{-

FIXME:

It should be

Ref (Sched Int)

Because Sched should be of kind * -> *
and Ref should also be of kind * -> *
(add an attribute to an existing type)


"Ref Sched Int"

is probably wrong because in keeping with function application
syntax parsing, it should be parsed as (Ref Sched) Int,
which isn't right


-}

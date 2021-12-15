module IR.ClassInstantiation where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers                       ( FieldId
                                                          , TConId(..)
                                                          , fromString
                                                          )
import           Data.Bifunctor                           ( second )
import           IR.IR
import qualified IR.Types.Classes              as Classes
import qualified IR.Types.Poly                 as Poly
import           IR.Types.TypeSystem                      ( TypeDef(..)
                                                          , TypeVariant(..)
                                                          )

instProgram :: Program Classes.Type -> Compiler.Pass (Program Poly.Type)
instProgram Program { programEntry = pEntry, programDefs = pdefs, programTypes = tdefs, programClasses = cdefs, programInsts = idefs }
    = do
        let tdefs' = fmap (second $ fmap class2Poly) tdefs
            pdefs' = fmap (second $ fmap class2Poly) pdefs
        cdefs' <- instClasses cdefs
        -- idefs' <- instInsts idefs pdefs
        _      <- if null idefs
            then return idefs
            else Compiler.throw
                $ Compiler.TypeError "can't instantiate class instances yet"
        return $ Program { programEntry   = pEntry
                         , programDefs    = pdefs'
                         , programTypes   = cdefs' ++ tdefs'
                         , programClasses = []
                         , programInsts   = []
                         }

instClasses
    :: [ClassDef Classes.Type] -> Compiler.Pass [(TConId, TypeDef Poly.Type)]
instClasses = mapM instClass

instClass :: ClassDef Classes.Type -> Compiler.Pass (TConId, TypeDef Poly.Type)
instClass ClassDef { className = clsName, classTVar = clsTVar, classMethods = clsMethods }
    = do
        clsMethods' <- mapM instClassMethod clsMethods
        return
            ( fromString $ show clsName
            , TypeDef
                { arity       = 1
                , variants    = [ ( fromString $ show clsName
                                  , VariantNamed clsMethods'
                                  )
                                ]
                , typeDefVars = [class2Poly clsTVar]
                }
            )

instClassMethod :: (VarId, Classes.Type) -> Compiler.Pass (FieldId, Poly.Type)
instClassMethod (mName, mType) =
    return (fromString $ show mName, class2Poly mType)

instInsts
    :: [InstDef Classes.Type]
    -> [(VarId, Expr Classes.Type)]
    -> Compiler.Pass [(VarId, Expr Poly.Type)]
-- instInsts _ = fmap (second $ fmap class2Poly)
instInsts _ _ = Compiler.throw
    $ Compiler.TypeError "instance instantiation: not implemented"


class2Poly :: Classes.Type -> Poly.Type
class2Poly (Classes.TBuiltin bty) = Poly.TBuiltin $ fmap class2Poly bty
class2Poly (Classes.TCon tid tys) = Poly.TCon tid $ fmap class2Poly tys
class2Poly (Classes.TVar tid    ) = Poly.TVar tid

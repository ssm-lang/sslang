module IR.ClassInstantiation where

import qualified Common.Compiler               as Compiler
import           Common.Identifiers                       ( FieldId
                                                          , Identifier(..)
                                                          , TConId(..)
                                                          , fromString
                                                          )
import           Control.Comonad                          ( extract )
import           Data.Bifunctor                           ( second )
import           Data.Foldable                            ( find )
import           IR.IR
import qualified IR.Types.Classes              as Classes
import qualified IR.Types.Poly                 as Poly
import           IR.Types.TypeSystem                      ( TypeDef(..)
                                                          , TypeVariant(..)
                                                          , arrow
                                                          )

instProgram :: Program Classes.Type -> Compiler.Pass (Program Poly.Type)
instProgram Program { programEntry = pEntry, programDefs = pdefs, programTypes = tdefs, programClasses = cdefs, programInsts = idefs }
    = do
        let tdefs' = fmap (second $ fmap class2Poly) tdefs
            pdefs' = fmap (second $ fmap class2Poly) pdefs
        cdefs' <- instClasses cdefs
        idefs' <- instInsts cdefs idefs
        return $ Program { programEntry   = pEntry
                         , programDefs    = idefs' ++ pdefs'
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
    :: [ClassDef Classes.Type]
    -> [InstDef Classes.Type]
    -> Compiler.Pass [(VarId, Expr Poly.Type)]
instInsts cdefs idefs = do
    mapM instInst' idefs
  where
    instInst' :: InstDef Classes.Type -> Compiler.Pass (VarId, Expr Poly.Type)
    instInst' idef =
        let clsName = case instConstraint idef of
                IsIn _ clsName' -> clsName'
            cdef = find (\cdef' -> className cdef' == clsName) cdefs
        in  case cdef of
                Just cdef' -> instInst cdef' idef
                Nothing    -> Compiler.throw $ Compiler.TypeError
                    "can't instantiate because class does not exist"


instInst
    :: ClassDef Classes.Type
    -> InstDef Classes.Type
    -> Compiler.Pass (VarId, Expr Poly.Type)
instInst cdef idef = do
    let (IsIn itype cid@(ClassId (Identifier cid'))) = instConstraint idef
        iDataName = fromString $ instNameMangle cid itype
        iDataType = Poly.TCon (fromString . show $ cid) [class2Poly itype]
        methods =
            [ find (\(imName, _) -> imName == cmName) (instMethods idef)
            | (cmName, _) <- classMethods cdef
            ]
        buildType :: [Poly.Type] -> Poly.Type
        buildType []       = iDataType
        buildType (t : ts) = t `arrow` buildType ts
        buildData
            :: [Poly.Type]
            -> [Maybe (VarId, Expr Classes.Type)]
            -> Compiler.Pass (Expr Poly.Type)
        buildData fieldTypes [] =
            return $ Data (fromString cid') (buildType fieldTypes)
        buildData fieldTypes (m : ms) = do
            case m of
                Just (_, body) ->
                    let mType = class2Poly $ extract body
                    in  do
                            dataVal <- buildData (mType : fieldTypes) ms
                            return $ App dataVal
                                         (fmap class2Poly body)
                                         (buildType fieldTypes)
                Nothing -> Compiler.throw
                    $ Compiler.TypeError "Instance is missing a method"

    dataVal <- buildData [] (reverse methods)
    return (iDataName, dataVal)

class2Poly :: Classes.Type -> Poly.Type
class2Poly (Classes.TBuiltin bty) = Poly.TBuiltin $ fmap class2Poly bty
class2Poly (Classes.TCon tid tys) = Poly.TCon tid $ fmap class2Poly tys
class2Poly (Classes.TVar tid    ) = Poly.TVar tid

instNameMangle :: ClassId -> Classes.Type -> String
instNameMangle cid instType = show cid ++ "_" ++ show instType

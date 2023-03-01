{-# LANGUAGE Rank2Types #-}

module IR.Constraint.Unify where

import qualified IR.Constraint.Error           as ET
import           IR.Constraint.Monad            ( TC )
import           IR.Constraint.Type            as Type
import qualified IR.Constraint.UnionFind       as UF

-- | UNIFY

data Answer
  = Ok
  | Err ET.Type ET.Type

unify :: Variable -> Variable -> TC Answer
unify v1 v2 = case guardedUnify v1 v2 of
  Unify k -> k onSuccess $ \() -> do
    t1 <- Type.toErrorType v1
    t2 <- Type.toErrorType v2
    UF.union v1 v2 errorDescriptor
    return $ Err t1 t2

onSuccess :: () -> TC Answer
onSuccess () = return Ok

errorDescriptor :: Descriptor
errorDescriptor = Descriptor Error noRank noMark Nothing


-- | CPS style UNIFIER

newtype Unify a =
  Unify (forall r.
    (a -> TC r)
    -> (() -> TC r)
    -> TC r
  )

mismatch :: Unify a
mismatch = Unify $ \_ err -> err ()


-- | UNIFICATION HELPERS
data Context = Context
  { _first      :: Variable
  , _firstDesc  :: Descriptor
  , _second     :: Variable
  , _secondDesc :: Descriptor
  }


-- | MERGE

merge :: Context -> Content -> Unify ()
merge (Context var1 (Descriptor _ rank1 _ _) var2 (Descriptor _ rank2 _ _)) content
  = Unify $ \ok _ -> ok =<< UF.union
    var1
    var2
    (Descriptor content (min rank1 rank2) noMark Nothing)


-- | ACTUALLY UNIFY

guardedUnify :: Variable -> Variable -> Unify ()
guardedUnify left right = Unify $ \ok err -> do
  equivalent <- UF.equivalent left right
  if equivalent
    then ok ()
    else do
      leftDesc  <- UF.get left
      rightDesc <- UF.get right
      case actuallyUnify (Context left leftDesc right rightDesc) of
        Unify k -> k ok err

subUnify :: Variable -> Variable -> Unify ()
subUnify var1 var2 = guardedUnify var1 var2


actuallyUnify :: Context -> Unify ()
actuallyUnify context@(Context _ (Descriptor firstContent _ _ _) _ (Descriptor secondContent _ _ _))
  = case firstContent of
    FlexVar  _ -> unifyFlex context firstContent secondContent

    RigidVar _ -> unifyRigid context firstContent secondContent

    Structure flatType ->
      unifyStructure context flatType firstContent secondContent

    Error ->
        -- If there was an error, just pretend it is okay. This lets us avoid
        -- "cascading" errors where one problem manifests as multiple message.
      merge context Error


-- UNIFY FLEXIBLE VARIABLES


unifyFlex :: Context -> Content -> Content -> Unify ()
unifyFlex context _ otherContent = case otherContent of
  Error       -> merge context Error

  FlexVar   _ -> merge context otherContent

  RigidVar  _ -> merge context otherContent

  Structure _ -> merge context otherContent



-- UNIFY RIGID VARIABLES


unifyRigid :: Context -> Content -> Content -> Unify ()
unifyRigid context content otherContent = case otherContent of
  FlexVar   _ -> merge context content

  RigidVar  _ -> mismatch

  Structure _ -> mismatch

  Error       -> merge context Error


-- UNIFY STRUCTURES

unifyStructure :: Context -> FlatType -> Content -> Content -> Unify ()
unifyStructure context flatType content otherContent = case otherContent of
  FlexVar   _             -> merge context content

  RigidVar  _             -> mismatch

  Structure otherFlatType -> case (flatType, otherFlatType) of
    (TCon1 name args, TCon1 otherName otherArgs) | name == otherName ->
      Unify $ \ok err ->
        let ok1 () = case merge context otherContent of
              Unify k -> k ok err
        in  unifyArgs context args otherArgs ok1 err

    _ -> mismatch

  Error -> merge context Error


-- UNIFY ARGS

unifyArgs
  :: Context -> [Variable] -> [Variable] -> (() -> TC r) -> (() -> TC r) -> TC r
unifyArgs context (arg1 : others1) (arg2 : others2) ok err =
  case subUnify arg1 arg2 of
    Unify k -> k (\() -> unifyArgs context others1 others2 ok err)
                 (\() -> unifyArgs context others1 others2 err err)
unifyArgs _ [] [] ok _   = ok ()
unifyArgs _ _  _  _  err = err ()

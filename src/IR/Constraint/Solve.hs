module IR.Constraint.Solve where

import qualified Common.Identifiers            as Ident
import           Control.Monad                  ( foldM
                                                , forM_
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( (!) )
import qualified Data.Vector                   as Vector
import qualified Data.Vector.Mutable           as MVector
import           GHC.Base                       ( liftM )
import qualified IR.Constraint.Canonical       as Can
import qualified IR.Constraint.Error           as ET
import           IR.Constraint.Monad            ( TC )
import qualified IR.Constraint.Occurs          as Occurs
import           IR.Constraint.Type            as Type
import qualified IR.Constraint.Unify           as Unify
import qualified IR.Constraint.UnionFind       as UF

-- | RUN SOLVER

run :: Constraint -> TC (Either [ET.Error] ())
run constraint = do
  pools              <- MVector.replicate 8 []
  (State _ _ errors) <- solve Map.empty
                              outermostRank
                              pools
                              emptyState
                              constraint
  if null errors then return $ Right () else return $ Left errors

emptyState :: State
emptyState = State Map.empty (nextMark noMark) []

-- | SOLVER

type Env = Map.Map Ident.Identifier Variable

type Pools = MVector.IOVector [Variable]

data State = State
  { _env    :: Env
  , _mark   :: Mark
  , _errors :: [ET.Error]
  }

solve :: Env -> Int -> Pools -> State -> Constraint -> TC State
solve env rank pools state constraint = case constraint of
  CTrue                   -> return state

  CSaveTheEnvironment     -> return (state { _env = env })

  CEqual tipe expectation -> do
    actual   <- typeToVariable rank pools tipe
    expected <- typeToVariable rank pools expectation
    answer   <- Unify.unify actual expected
    case answer of
      Unify.Ok -> do
        -- introduce rank pools vars
        return state

      Unify.Err actualType expectedType -> do
        -- introduce rank pools vars
        return $ addError state $ ET.BadExpr actualType expectedType

  CLocal name expectation -> do
    actual   <- makeCopy rank pools (env ! name)
    expected <- typeToVariable rank pools expectation
    answer   <- Unify.unify actual expected
    case answer of
      Unify.Ok -> do
        -- introduce rank pools vars
        return state

      Unify.Err actualType expectedType -> do
        -- introduce rank pools vars
        return $ addError state $ ET.BadExpr actualType expectedType

  CForeign name (Can.Forall freeVars srcType) expectation -> do
    actual   <- schemeToVariable rank pools freeVars srcType
    expected <- typeToVariable rank pools expectation
    answer   <- Unify.unify actual expected
    case answer of
      Unify.Ok -> do
        -- introduce rank pools vars
        return state

      Unify.Err actualType expectedType -> do
        -- introduce rank pools vars
        return $ addError state $ ET.BadExpr actualType expectedType


  CPattern tipe expectation -> do
    actual   <- typeToVariable rank pools tipe
    expected <- typeToVariable rank pools expectation
    answer   <- Unify.unify actual expected
    case answer of
      Unify.Ok -> do
        -- introduce rank pools vars
        return state

      Unify.Err actualType expectedType -> do
        -- introduce rank pools vars
        return $ addError state $ ET.BadPattern actualType expectedType

  CAnd constraints -> foldM (solve env rank pools) state constraints

  CLet [] flexs _ headerCon CTrue -> do
    introduce rank pools flexs
    solve env rank pools state headerCon

  CLet [] [] header headerCon subCon -> do
    state1 <- solve env rank pools state headerCon
    locals <- traverse (typeToVariable rank pools) header
    let newEnv = Map.union env locals
    state2 <- solve newEnv rank pools state1 subCon
    foldM occurs state2 $ Map.toList locals

  CLet rigids flexs header headerCon subCon -> do
    -- work in the next pool to localize header
    let nextRank    = rank + 1
    let poolsLength = MVector.length pools
    nextPools <- if nextRank < poolsLength
      then return pools
      else MVector.grow pools poolsLength

    -- introduce variables
    let vars = rigids ++ flexs
    forM_ vars $ \var -> UF.modify var $ \(Descriptor content _ mark copy) ->
      Descriptor content nextRank mark copy
    MVector.write nextPools nextRank vars

    -- run solver in next pool
    locals <- traverse (typeToVariable nextRank nextPools) header
    (State savedEnv mark errors) <- solve env nextRank nextPools state headerCon

    let youngMark = mark
    let visitMark = nextMark youngMark
    let finalMark = nextMark visitMark

    -- pop pool
    generalize youngMark visitMark nextRank nextPools
    MVector.write nextPools nextRank []

    -- check that things went well
    mapM_ isGeneric rigids

    let newEnv    = Map.union env locals
    let tempState = State savedEnv finalMark errors
    newState <- solve newEnv rank nextPools tempState subCon

    foldM occurs newState (Map.toList locals)


-- Check that a variable has rank == noRank, meaning that it can be generalized
isGeneric :: Variable -> TC ()
isGeneric var = do
  (Descriptor _ rank _ _) <- UF.get var
  if rank == noRank
    then return ()
    else do
      error "Compiler bug: unification variable should be generic"


-- | ERROR HELPERS

addError :: State -> ET.Error -> State
addError (State savedEnv rank errors) err = State savedEnv rank (err : errors)


-- | OCCURS CHECK

occurs :: State -> (Ident.Identifier, Variable) -> TC State
occurs state (name, variable) = do
  hasOccurred <- Occurs.occurs variable
  if hasOccurred
    then do
      errorType                     <- Type.toErrorType variable
      (Descriptor _ rank mark copy) <- UF.get variable
      UF.set variable (Descriptor Error rank mark copy)
      return $ addError state $ ET.InfiniteType name errorType
    else return state


-- | GENERALIZE

{-| Every variable has rank less than or equal to the maxRank of the pool.
This sorts variables into the young and old pools accordingly.
-}
generalize :: Mark -> Mark -> Int -> Pools -> TC ()
generalize youngMark visitMark youngRank pools = do
  youngVars <- MVector.read pools youngRank
  rankTable <- poolToRankTable youngMark youngRank youngVars

  -- get the ranks right for each entry.
  -- start at low ranks so that we only have to pass
  -- over the information once.
  Vector.imapM_
    (\rank table -> mapM_ (adjustRank youngMark visitMark rank) table)
    rankTable

  -- For variables that have rank lowerer than youngRank, register them in
  -- the appropriate old pool if they are not redundant.
  Vector.forM_ (Vector.unsafeInit rankTable) $ \vars -> forM_ vars $ \var -> do
    isRedundant <- UF.redundant var
    if isRedundant
      then return ()
      else do
        (Descriptor _ rank _ _) <- UF.get var
        MVector.modify pools (var :) rank

  -- For variables with rank youngRank
  --   If rank < youngRank: register in oldPool
  --   otherwise generalize
  forM_ (Vector.unsafeLast rankTable) $ \var -> do
    isRedundant <- UF.redundant var
    if isRedundant
      then return ()
      else do
        (Descriptor content rank mark copy) <- UF.get var
        if rank < youngRank
          then MVector.modify pools (var :) rank
          else UF.set var $ Descriptor content noRank mark copy


poolToRankTable :: Mark -> Int -> [Variable] -> TC (Vector.Vector [Variable])
poolToRankTable youngMark youngRank youngInhabitants = do
  mutableTable <- MVector.replicate (youngRank + 1) []

  -- Sort the youngPool variables into buckets by rank.
  forM_ youngInhabitants $ \var -> do
    (Descriptor content rank _ copy) <- UF.get var
    UF.set var (Descriptor content rank youngMark copy)
    MVector.modify mutableTable (var :) rank

  Vector.unsafeFreeze mutableTable


-- | ADJUST RANK

--
-- Adjust variable ranks such that ranks never increase as you move deeper.
-- This way the outermost rank is representative of the entire structure.
--
adjustRank :: Mark -> Mark -> Int -> Variable -> TC Int
adjustRank youngMark visitMark groupRank var = do
  (Descriptor content rank mark copy) <- UF.get var
  if mark == youngMark
    then do  -- Set the variable as marked first because it may be cyclic.
      UF.set var $ Descriptor content rank visitMark copy
      maxRank <- adjustRankContent youngMark visitMark groupRank content
      UF.set var $ Descriptor content maxRank visitMark copy
      return maxRank
    else if mark == visitMark
      then return rank
      else do
        let minRank = min groupRank rank
        -- TODO how can minRank ever be groupRank?
        UF.set var $ Descriptor content minRank visitMark copy
        return minRank


adjustRankContent :: Mark -> Mark -> Int -> Content -> TC Int
adjustRankContent youngMark visitMark groupRank content =
  let go = adjustRank youngMark visitMark groupRank
  in  case content of
        FlexVar   _        -> return groupRank

        RigidVar  _        -> return groupRank

        Structure flatType -> case flatType of
          TCon1 _ args ->
            foldM (\rank arg -> max rank <$> go arg) outermostRank args

        Error -> return groupRank


-- | REGISTER VARIABLES

introduce :: Int -> Pools -> [Variable] -> TC ()
introduce rank pools variables = do
  MVector.modify pools (variables ++) rank
  forM_ variables $ \var -> UF.modify var
    $ \(Descriptor content _ mark copy) -> Descriptor content rank mark copy


-- | TYPE TO VARIABLE

typeToVariable :: Int -> Pools -> Type -> TC Variable
typeToVariable rank pools tipe = typeToVar rank pools Map.empty tipe

typeToVar
  :: Int -> Pools -> Map.Map Ident.TVarId Variable -> Type -> TC Variable
typeToVar rank pools aliasDict tipe =
  let go = typeToVar rank pools aliasDict
  in  case tipe of
        TVarN v         -> return v

        TConN name args -> do
          argVars <- traverse go args
          register rank pools (Structure (TCon1 name argVars))

register :: Int -> Pools -> Content -> TC Variable
register rank pools content = do
  var <- UF.fresh (Descriptor content rank noMark Nothing)
  MVector.modify pools (var :) rank
  return var


-- SOURCE TYPE TO VARIABLE


schemeToVariable
  :: Int -> Pools -> Map.Map Ident.TVarId () -> Can.Type -> TC Variable
schemeToVariable rank pools freeVars srcType =
  let nameToContent name = FlexVar name

      makeVar name _ =
        UF.fresh (Descriptor (nameToContent name) rank noMark Nothing)
  in  do
        flexVars <- Map.traverseWithKey makeVar freeVars
        MVector.modify pools (Map.elems flexVars ++) rank
        schemeToVar rank pools flexVars srcType


schemeToVar
  :: Int -> Pools -> Map.Map Ident.TVarId Variable -> Can.Type -> TC Variable
schemeToVar rank pools flexVars srcType =
  let go = schemeToVar rank pools flexVars
  in  case srcType of
        Can.TVar name      -> return (flexVars ! name)

        Can.TCon name args -> do
          argVars <- traverse go args
          register rank pools (Structure (TCon1 name argVars))



-- | COPY

makeCopy :: Int -> Pools -> Variable -> TC Variable
makeCopy rank pools var = do
  copy <- makeCopyHelp rank pools var
  restore var
  return copy


makeCopyHelp :: Int -> Pools -> Variable -> TC Variable
makeCopyHelp maxRank pools variable = do
  (Descriptor content rank _ maybeCopy) <- UF.get variable

  case maybeCopy of
    Just copy -> return copy

    Nothing   -> if rank /= noRank
      then return variable
      else do
        let makeDescriptor c = Descriptor c maxRank noMark Nothing
        copy <- UF.fresh $ makeDescriptor content
        MVector.modify pools (copy :) maxRank

        -- Link the original variable to the new variable. This lets us
        -- avoid making multiple copies of the variable we are instantiating.
        --
        -- Need to do this before recursively copying to avoid looping.
        UF.set variable $ Descriptor content rank noMark (Just copy)

        -- Now we recursively copy the content of the variable.
        -- We have already marked the variable as copied, so we
        -- will not repeat this work or crawl this variable again.
        case content of
          Structure term -> do
            newTerm <- traverseFlatType (makeCopyHelp maxRank pools) term
            UF.set copy $ makeDescriptor (Structure newTerm)
            return copy

          FlexVar  _    -> return copy

          RigidVar name -> do
            UF.set copy $ makeDescriptor $ FlexVar name
            return copy

          Error -> return copy



-- RESTORE


restore :: Variable -> TC ()
restore variable = do
  (Descriptor content _ _ maybeCopy) <- UF.get variable
  case maybeCopy of
    Nothing -> return ()

    Just _  -> do
      UF.set variable $ Descriptor content noRank noMark Nothing
      restoreContent content


restoreContent :: Content -> TC ()
restoreContent content = case content of
  FlexVar   _    -> return ()

  RigidVar  _    -> return ()

  Structure term -> case term of
    TCon1 _ args -> mapM_ restore args

  Error -> return ()



--  | TRAVERSE FLAT TYPE


traverseFlatType :: (Variable -> TC Variable) -> FlatType -> TC FlatType
traverseFlatType f flatType = case flatType of
  TCon1 name args -> liftM (TCon1 name) (traverse f args)

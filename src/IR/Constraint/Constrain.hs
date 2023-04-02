module IR.Constraint.Constrain where

import           Data.Bifunctor                 ( second )
import qualified IR.Constraint.Canonical       as Can
import qualified IR.Constraint.Constrain.Program
                                               as Prog
import           IR.Constraint.Monad            ( TC )
import           IR.Constraint.Type
import qualified IR.IR                         as I

run :: I.Program Can.Annotations -> TC (Constraint, I.Program Variable)
run pAnn = do
  pSprinkled <- sprinkleVariables pAnn
  constraint <- Prog.constrain pSprinkled
  let pVar = discardAnnotations pSprinkled
  return (constraint, pVar)

sprinkleVariables
  :: I.Program Can.Annotations -> TC (I.Program (Can.Annotations, Variable))
sprinkleVariables prog = do
  sprinkledDefs <- mapM sprinkleDef (I.programDefs prog)
  return prog { I.programDefs = sprinkledDefs }
 where
  sprinkleDef (name, expr) = do
    expr' <- mapM sprinkle expr
    return (name, expr')
  sprinkle ann = do
    v <- mkIRFlexVar
    return (ann, v)


constrain :: I.Program (Can.Annotations, Variable) -> TC Constraint
constrain = undefined

discardAnnotations
  :: I.Program (Can.Annotations, Variable) -> I.Program Variable
discardAnnotations sprinkledProg =
  let discardedDefs =
        map (second $ fmap (\(_, v) -> v)) (I.programDefs sprinkledProg)
  in  sprinkledProg { I.programDefs = discardedDefs }

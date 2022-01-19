#if 0

Original EDSL source:

module Regression.FlipFlopLoopSpec where

import           Data.Map                       ( fromList )
import           SSM.Core.Syntax
import qualified Test.Hspec                    as H
import qualified Test.Hspec.QuickCheck         as H
import qualified Test.SSM.Prop                 as T

p :: Program
p = Program
  { entry = "fun1"
  , args  = [Right ("ref2", Ref TUInt64)]
  , funs  = fromList
              [ ( "fun1"
                , Procedure
                  { name      = "fun1"
                  , arguments = [("ref2", Ref TUInt64)]
                  , body      = [ While
                                    (Lit TBool (LBool True))
                                    [ After (Lit TUInt64 (LUInt64 2))
                                            ("ref2", Ref TUInt64)
                                            (Lit TUInt64 (LUInt64 0))
                                    , Wait [("ref2", Ref TUInt64)]
                                    , After (Lit TUInt64 (LUInt64 2))
                                            ("ref2", Ref TUInt64)
                                            (Lit TUInt64 (LUInt64 1))
                                    , Wait [("ref2", Ref TUInt64)]
                                    ]
                                ]
                  }
                )
              ]
  }

spec :: H.Spec
spec = T.correctSpec "FlipFlopLoop" p


#endif


#include "ssm.h"

#define SSM_DEBUG_MICROTICK(x)
#define SSM_DEBUG_TRACE(x)

#if 0
static int _add(int a, int b)
{
    return a + b;
}
#endif

typedef struct {
            struct ssm_act act;
            ssm_u64_t *ref2;
            struct ssm_trigger trig1;
        } act_fun1_t;

struct ssm_act *enter_fun1(struct ssm_act *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_u64_t *ref2);

void step_fun1(struct ssm_act *actg);

struct ssm_act *enter_fun1(struct ssm_act *caller, ssm_priority_t priority, ssm_depth_t depth, ssm_u64_t *ref2)
{
    struct ssm_act *actg = ssm_enter(sizeof(act_fun1_t), step_fun1, caller, priority, depth);
    act_fun1_t *acts = container_of(actg, act_fun1_t, act);
    acts->ref2 = ref2;
    acts->trig1.act = actg;
    return actg;
}
void step_fun1(struct ssm_act *actg)
{
    SSM_DEBUG_MICROTICK();
    SSM_DEBUG_TRACE("ActStepBegin \"fun1\"");
    act_fun1_t *acts = container_of(actg, act_fun1_t, act);
    switch (actg->pc) {
      case 0:
        ;
        while (true) {
            SSM_DEBUG_MICROTICK();
            ssm_later_u64(acts->ref2, ssm_now() + 100 * SSM_MILLISECOND, (u64) 0);
            ssm_sensitize(&acts->ref2->sv, &acts->trig1);
            actg->pc = 1;
            return;
          case 1:
            ;
            ssm_desensitize(&acts->trig1);
            ssm_later_u64(acts->ref2, ssm_now() + 100 * SSM_MILLISECOND, (u64) 1);
            ssm_sensitize(&acts->ref2->sv, &acts->trig1);
            actg->pc = 2;
            return;
          case 2:
            ;
            ssm_desensitize(&acts->trig1);
        }
      default:
        break;
    }
    ssm_leave(actg, sizeof(act_fun1_t));
}
// struct ssm_act *(*ssm_entry_point)(struct ssm_act *, ssm_priority_t, ssm_depth_t) = enter_fun1;

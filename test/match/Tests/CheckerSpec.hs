module Tests.CheckerSpec where

import           Sslang.Test

import           Common.Identifiers                       ( fromString )
import           IR.MatchChecker                          ( C(..)
                                                          , CSet(..)
                                                          , P(..)
                                                          , PM(..)
                                                          , PV(..)
                                                          , cSet
                                                          , defaultPM
                                                          , eqCSet
                                                          , fromListPM
                                                          , fromListPV
                                                          , isExhaustive
                                                          , isSubCSet
                                                          , justPV
                                                          , specializedPM
                                                          , specializedPV
                                                          , tailPV
                                                          , useful
                                                          , uselessClause
                                                          )

zero = C 0 (fromString "Zero")
one = C 1 (fromString "One")
two = C 2 (fromString "Two")
cs = CSet [zero, one, two]
isCompleteCSet cs' = cs' `eqCSet` cs


spec :: Spec
spec = do
  it "simple non-exhaustive pattern match - 1" $ do
    let pats   = [PCon one [PWild], PCon two [PWild, PWild]]
        result = isExhaustive isCompleteCSet pats
    result `shouldBe` False

  it "simple non-exhaustive pattern match - 2" $ do
    let pats   = [PWild]
        result = isExhaustive isCompleteCSet pats
    result `shouldBe` True

  it "simple exhaustive pattern match" $ do
    let pats   = [PCon zero [], PCon one [PWild], PCon two [PWild, PWild]]
        result = isExhaustive isCompleteCSet pats
    result `shouldBe` True

  it "recursive non-exhaustive pattern match" $ do
    let pats =
          [ PCon zero []
          , PCon one  [PCon zero []]
          , PCon one  [PCon one [PWild]]
          , PCon one  [PCon two [PWild, PWild]]
          , PCon two  [PCon zero [], PWild]
          , PCon two  [PCon one [PWild], PWild]
          ]
        result = isExhaustive isCompleteCSet pats
    result `shouldBe` False

  it "recursive exhaustive pattern match" $ do
    let pats =
          [ PCon zero []
          , PCon one  [PCon zero []]
          , PCon one  [PCon one [PWild]]
          , PCon one  [PCon two [PWild, PWild]]
          , PCon two  [PCon zero [], PWild]
          , PCon two  [PCon one [PWild], PWild]
          , PCon two  [PCon two [PWild, PWild], PWild]
          ]
        result = isExhaustive isCompleteCSet pats
    result `shouldBe` True

  it "calculates cset" $ do
    let pats1 = [PCon one [PWild], PCon two [PWild, PWild]]
        cs1   = CSet [one, two]
        pm1   = fromListPM pats1

        pats2 = [PCon zero [], PCon one [PWild], PCon two [PWild, PWild]]
        cs2   = CSet [zero, one, two]
        pm2   = fromListPM pats2

        pats3 =
          [ PCon zero []
          , PCon one  [PCon zero []]
          , PCon one  [PCon one [PWild]]
          , PCon one  [PCon two [PWild, PWild]]
          , PCon two  [PCon zero [], PWild]
          , PCon two  [PCon one [], PWild]
          ]
        cs3 = CSet [zero, one, two]
        pm3 = fromListPM pats3

        cs4 = CSet [zero]
        pm4 = PM { getPMM = 1
                 , getPMN = 2
                 , getPML = [fromListPV [PCon zero [], PCon one [PWild]]]
                 }

        cs5 = CSet [zero, one]
        pm5 = PM
          { getPMM = 2
          , getPMN = 2
          , getPML = [ fromListPV [PCon zero [], PCon one [PWild]]
                     , fromListPV [PCon one [PWild], PCon two [PWild, PWild]]
                     ]
          }

    (cSet pm1 `eqCSet` cs1) `shouldBe` True
    (cSet pm2 `eqCSet` cs2) `shouldBe` True
    (cSet pm3 `eqCSet` cs3) `shouldBe` True
    (cSet pm4 `eqCSet` cs4) `shouldBe` True
    (cSet pm5 `eqCSet` cs5) `shouldBe` True

  it "compares cset" $ do
    let cs1 = CSet [zero, one]
        cs2 = CSet [zero, one, two]
    (cs1 `isSubCSet` cs2) `shouldBe` True
    (cs2 `isSubCSet` cs1) `shouldBe` False
    (cs1 `eqCSet` cs2) `shouldBe` False
    (cs2 `eqCSet` cs2) `shouldBe` True

  it "calculates simple S(c,P) - 1" $ do
    let pm = fromListPM [PCon one [PWild], PCon two [PWild, PWild]]
    specializedPM two pm
      `shouldBe` PM
                   { getPMM = 1
                   , getPMN = 2
                   , getPML = [fromListPV [PWild, PWild]]
                   }

  it "calculates simple D(P) - 2" $ do
    let pats = [PCon one [PWild], PCon two [PWild, PWild]]
        pm   = fromListPM pats
    -- let pm = PM
    --       { getPMM = 2
    --       , getPMN = 1
    --       , getPML = [ justPV (PCon one [PWild])
    --                  , justPV (PCon two [PWild, PWild])
    --                  ]
    --       }
    defaultPM pm `shouldBe` PM { getPMM = 0, getPMN = 0, getPML = [] }

  it "simple justPV" $ do
    justPV PWild `shouldBe` PV { getPVN = 1, getPVL = [PWild] }

  it "simple tailPV" $ do
    tailPV (justPV PWild) `shouldBe` PV { getPVN = 0, getPVL = [] }

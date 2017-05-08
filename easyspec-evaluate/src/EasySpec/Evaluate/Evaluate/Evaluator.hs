module EasySpec.Evaluate.Evaluate.Evaluator where

import Import

import Text.Printf

import qualified EasySpec.Discover.CodeUtils as ES
import qualified EasySpec.Discover.SignatureInference.Utils as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate.Evaluator.Types

evaluators :: [Evaluator]
evaluators =
    concat
        [ baseEvaluators
        , mapMaybe
              (uncurry addEvaluators)
              (ES.unorderedCombinations baseEvaluators)
        , mapMaybe
              (uncurry subtractEvaluators)
              (orderedCombinations baseEvaluators)
        , mapMaybe
              (uncurry divideEvaluators)
              (orderedCombinations baseEvaluators)
        ]

orderedCombinations :: [a] -> [(a, a)]
orderedCombinations [] = []
orderedCombinations [_] = []
orderedCombinations (x:xs) = go [] x xs
  where
    go _ _ [] = []
    go acc el rests@(r:rs) = map ((,) el) (rests ++ acc) ++ go (el : acc) r rs

addEvaluators :: Evaluator -> Evaluator -> Maybe Evaluator
addEvaluators e1 e2 =
    if evaluatorUnit e1 == evaluatorUnit e2 &&
       evaluatorQuantity e1 == evaluatorQuantity e2
        then Just
                 Evaluator
                 { evaluatorName =
                       intercalate
                           "-"
                           [evaluatorName e1, "plus", evaluatorName e2]
                 , evaluatorGather =
                       \ei ->
                           liftM2
                               (+)
                               (evaluatorGather e1 ei)
                               (evaluatorGather e2 ei)
                 , evaluatorPretty =
                       \ei ->
                           unwords
                               [ evaluatorPretty e1 ei
                               , "+"
                               , evaluatorPretty e2 ei
                               ]
                 , evaluatorUnit = evaluatorUnit e1
                 , evaluatorQuantity = evaluatorQuantity e1
                 }
        else Nothing

subtractEvaluators :: Evaluator -> Evaluator -> Maybe Evaluator
subtractEvaluators e1 e2 =
    if evaluatorUnit e1 == evaluatorUnit e2 &&
       evaluatorQuantity e1 == evaluatorQuantity e2
        then Just
                 Evaluator
                 { evaluatorName =
                       intercalate
                           "-"
                           [evaluatorName e1, "minus", evaluatorName e2]
                 , evaluatorGather =
                       \ei ->
                           liftM2
                               (-)
                               (evaluatorGather e1 ei)
                               (evaluatorGather e2 ei)
                 , evaluatorPretty =
                       \ei ->
                           unwords
                               [ evaluatorPretty e1 ei
                               , "-"
                               , evaluatorPretty e2 ei
                               ]
                 , evaluatorUnit = evaluatorUnit e1
                 , evaluatorQuantity = evaluatorQuantity e1
                 }
        else Nothing

divideEvaluators :: Evaluator -> Evaluator -> Maybe Evaluator
divideEvaluators e1 e2 =
    Just
        Evaluator
        { evaluatorName =
              intercalate "-" [evaluatorName e1, "divided-by", evaluatorName e2]
        , evaluatorGather =
              \ei -> do
                  n <- evaluatorGather e1 ei
                  d <- evaluatorGather e2 ei
                  let l = n / d
                  if isNaN l
                      then Nothing
                      else Just l
        , evaluatorPretty =
              \ei -> unwords [evaluatorPretty e1 ei, "/", evaluatorPretty e2 ei]
        , evaluatorUnit =
              if evaluatorUnit e1 == evaluatorUnit e2
                  then "ratio"
                  else unwords [evaluatorUnit e1, "/", evaluatorUnit e2]
        , evaluatorQuantity =
              if evaluatorQuantity e1 == evaluatorQuantity e2
                  then "1"
                  else unwords [evaluatorQuantity e1, "/", evaluatorQuantity e2]
        }

baseEvaluators :: [Evaluator]
baseEvaluators =
    [ equationsEvaluator
    , runtimeEvaluator
    , relevantEquationsEvaluator
    , irrelevantEquationsEvaluator
    ]

equationsEvaluator :: Evaluator
equationsEvaluator =
    Evaluator
    { evaluatorName = "equations"
    , evaluatorGather = Just . genericLength . eiDiscoveredEqs
    , evaluatorPretty = show . length . eiDiscoveredEqs
    , evaluatorUnit = "#"
    , evaluatorQuantity = "equation"
    }

runtimeEvaluator :: Evaluator
runtimeEvaluator =
    Evaluator
    { evaluatorName = "runtime"
    , evaluatorGather = Just . eiRuntime
    , evaluatorPretty = printf "%.3f" . eiRuntime
    , evaluatorUnit = "time"
    , evaluatorQuantity = "seconds"
    }

relevantEquationsEvaluator :: Evaluator
relevantEquationsEvaluator =
    Evaluator
    { evaluatorName = "relevant-equations"
    , evaluatorGather = Just . genericLength . go
    , evaluatorPretty = show . length . go
    , evaluatorUnit = "#"
    , evaluatorQuantity = "equation"
    }
  where
    go ei = filter (mentionsEq $ eiFocusFuncName ei) (eiDiscoveredEqs ei)

irrelevantEquationsEvaluator :: Evaluator
irrelevantEquationsEvaluator =
    Evaluator
    { evaluatorName = "irrelevant-equations"
    , evaluatorGather = Just . genericLength . go
    , evaluatorPretty = show . length . go
    , evaluatorUnit = "#"
    , evaluatorQuantity = "equation"
    }
  where
    go ei = filter (not . mentionsEq (eiFocusFuncName ei)) (eiDiscoveredEqs ei)

mentionsEq :: ES.EasyName -> ES.EasyEq -> Bool
mentionsEq n (ES.EasyEq e1 e2) = ES.mentions n e1 || ES.mentions n e2

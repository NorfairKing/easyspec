module EasySpec.Evaluate.Evaluate.Evaluator where

import Import

import Text.Printf

import qualified EasySpec.Discover.CodeUtils as ES
import qualified EasySpec.Discover.Types as ES

import EasySpec.Evaluate.Evaluate.Evaluator.Types

evaluators :: [Evaluator]
evaluators =
    [ equationsEvaluator
    , runtimeEvaluator
    , relevantEquationsEvaluator
    , irrelevantEquationsEvaluator
    , relativeRelevantEquationsEvaluator
    ]

evaluate :: EvaluationInput -> Evaluator -> String
evaluate ei e = evaluatorPretty e $ evaluatorGather e ei

equationsEvaluator :: Evaluator
equationsEvaluator =
    Evaluator "equations" (genericLength . eiDiscoveredEqs) show

runtimeEvaluator :: Evaluator
runtimeEvaluator = Evaluator "runtime" eiRuntime (printf "%.3f")

relativeRelevantEquationsEvaluator :: Evaluator
relativeRelevantEquationsEvaluator =
    Evaluator "relative-relevant-equations" go (printf "%.2f")
  where
    go ei =
        let l =
                evaluatorGather relevantEquationsEvaluator ei /
                evaluatorGather equationsEvaluator ei
        in if isNaN l
               then 0
               else l

relevantEquationsEvaluator :: Evaluator
relevantEquationsEvaluator = Evaluator "relevant-equations" go show
  where
    go ei =
        genericLength $
        filter (mentionsEq $ eiFocusFuncName ei) (eiDiscoveredEqs ei)

irrelevantEquationsEvaluator :: Evaluator
irrelevantEquationsEvaluator = Evaluator "irrelevant-equations" go show
  where
    go ei =
        genericLength $
        filter (not . mentionsEq (eiFocusFuncName ei)) (eiDiscoveredEqs ei)

mentionsEq :: ES.EasyName -> ES.EasyEq -> Bool
mentionsEq n (ES.EasyEq e1 e2) = ES.mentions n e1 || ES.mentions n e2

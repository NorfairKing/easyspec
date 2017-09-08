module EasySpec.Evaluate.Evaluate.Evaluator
    ( evaluators
    , scopeSizeEvaluator
    , equationsEvaluator
    , runtimeEvaluator
    , relevantEquationsEvaluator
    , relevantFunctionsEvaluator
    ) where

import Import

import EasySpec.Evaluate.Evaluate.Evaluator.Combinators
import EasySpec.Evaluate.Evaluate.Evaluator.Equations
import EasySpec.Evaluate.Evaluate.Evaluator.RelevantEquations
import EasySpec.Evaluate.Evaluate.Evaluator.RelevantFunctions
import EasySpec.Evaluate.Evaluate.Evaluator.Runtime
import EasySpec.Evaluate.Evaluate.Evaluator.ScopeSize
import EasySpec.Evaluate.Evaluate.Evaluator.Types

evaluators :: [Evaluator]
evaluators =
    baseEvaluators ++
    catMaybes
        [ subtractEvaluators
              SmallerIsBetter
              equationsEvaluator
              relevantEquationsEvaluator
        , relevantEquationsEvaluator `divideEvaluators` runtimeEvaluator
        ]

baseEvaluators :: [Evaluator]
baseEvaluators =
    [ scopeSizeEvaluator
    , equationsEvaluator
    , runtimeEvaluator
    , relevantEquationsEvaluator
    , relevantFunctionsEvaluator
    ]

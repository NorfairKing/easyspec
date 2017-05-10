{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module EasySpec.Evaluate.Evaluate.Evaluator.Combinators
    ( makeCombinationsOf
    ) where

import Import

import EasySpec.Evaluate.Analyse.Utils
import EasySpec.Evaluate.Evaluate.Evaluator.Types

makeCombinationsOf :: [Evaluator] -> [Evaluator]
makeCombinationsOf baseEvaluators =
    concat
        [ mapMaybe
              (uncurry addEvaluators)
              (unorderedCombinationsWithoutSelfCombinations baseEvaluators)
        , mapMaybe
              (uncurry subtractEvaluators)
              (orderedCombinationsWithoutSelfCombinations baseEvaluators)
        , mapMaybe
              (uncurry multiplyEvaluators)
              (unorderedCombinationsWithoutSelfCombinations baseEvaluators)
        , mapMaybe
              (uncurry divideEvaluators)
              (orderedCombinationsWithoutSelfCombinations baseEvaluators)
        ]

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
                 , evaluatorRelevantFiles =
                       $(mkRelFile __FILE__) :
                       (evaluatorRelevantFiles e1 ++ evaluatorRelevantFiles e2)
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
                 , evaluatorRelevantFiles =
                       $(mkRelFile __FILE__) :
                       (evaluatorRelevantFiles e1 ++ evaluatorRelevantFiles e2)
                 }
        else Nothing

multiplyEvaluators :: Evaluator -> Evaluator -> Maybe Evaluator
multiplyEvaluators e1 e2 =
    Just
        Evaluator
        { evaluatorName =
              intercalate
                  "-"
                  [evaluatorName e1, "multiplied-by", evaluatorName e2]
        , evaluatorGather =
              \ei -> liftM2 (*) (evaluatorGather e1 ei) (evaluatorGather e2 ei)
        , evaluatorPretty =
              \ei -> unwords [evaluatorPretty e1 ei, "*", evaluatorPretty e2 ei]
        , evaluatorUnit =
              if evaluatorUnit e1 == evaluatorUnit e2
                  then evaluatorUnit e1 ++ "^2"
                  else unwords [evaluatorUnit e1, "*", evaluatorUnit e2]
        , evaluatorQuantity =
              if evaluatorQuantity e1 == evaluatorQuantity e2
                  then evaluatorQuantity e1 ++ "^2"
                  else unwords [evaluatorQuantity e1, "*", evaluatorQuantity e2]
        , evaluatorRelevantFiles =
              $(mkRelFile __FILE__) :
              (evaluatorRelevantFiles e1 ++ evaluatorRelevantFiles e2)
        }

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
                  divMaybe n d
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
        , evaluatorRelevantFiles =
              $(mkRelFile __FILE__) :
              (evaluatorRelevantFiles e1 ++ evaluatorRelevantFiles e2)
        }

divMaybe :: (RealFloat a, Fractional a) => a -> a -> Maybe a
divMaybe n d =
    let l = n / d
    in if isNaN l || isInfinite l
           then Nothing
           else Just l

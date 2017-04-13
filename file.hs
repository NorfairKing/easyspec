import Prelude

pad :: Char -> Int -> String -> String
pad c i s
    | length s < i = s ++ replicate (i - length s) c
    | otherwise = s

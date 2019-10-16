module Script where

    isBracket :: Char -> Bool
    isBracket '"' = True
    isBracket _ = False

    isNotBracket :: Char -> Bool
    isNotBracket '"' = False
    isNotBracket _ = True

    getStringFromBrackets :: String -> (String, String)
    getStringFromBrackets ('\"':t) = 
        let 
            nameAsStr = takeWhile isNotBracket t
            rest = drop (length nameAsStr + 1) t
        in (nameAsStr, rest)

    available :: String -> Either String (Int, Int, String)
    available ('{':t) = 
        let (pr, pab) = getStringFromBrackets t 
        in if pr == "prev"
                then 
                    let 
                        Right (first, second, rest) = available (drop 1 pab)
                    in
                        case first == second of
                            True -> Right (first + 1, second, rest)
                            False -> Right (first, second + 1, rest)
                else if pr == "result"
                    then 
                        let (result, pab2) = getStringFromBrackets (drop 1 pab)
                        in Right (1, 0, result)
                    else Left "Klaida"
                    
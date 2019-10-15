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

    available :: String -> Either (String, String) (Int, Int)
    available ('{':t) = 
        let (pr, pab) = getStringFromBrackets t 
        in if pr == "coord"
                then Left ("Ok", pab)
                else Left ("Bad", pab)



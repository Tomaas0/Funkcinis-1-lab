module Script where
    
import Entities
import Parse
import Control.Applicative
import Data.List

checkIfMovesValid :: Msg -> Bool
checkIfMovesValid message = not(checkIfExist playerA || checkIfExist playerB)
  where
    (playerA, playerB) = splitMoves message [] []
    checkIfExist :: [(String, String)] -> Bool
    checkIfExist [] = False
    checkIfExist (x:coords) =
      (x `elem` coords) || checkIfExist coords

available :: String -> Either String (Int, Int)
available input =
    case createMessage input of
        Left err -> Left err
        Right (msg, rest) ->
            if checkIfMovesValid msg
            then calculate msg 
            else Left "Same move occured multiple times"
            where
                calculate :: Msg -> Either String (Int, Int)
                calculate (Msg coords "" Empty) = Right (99, 100)
                calculate (Msg coords result prev) = 
                    let 
                        Right (playerA, playerB) = calculate prev
                    in
                        case result of
                            "HIT" -> Right (playerA - 1, playerB)
                            "MISS" -> Right (playerB - 1, playerA)

splitMoves :: Msg -> [(String, String)] -> [(String, String)] -> ([(String, String)], [(String, String)])
splitMoves (Msg ('0':_, '0':_) _ Empty) playerA playerB = (playerA, playerB)
splitMoves (Msg ('0':_, '0':_) _ prev) playerA playerB = splitMoves prev playerB playerA
splitMoves (Msg coords _ Empty) playerA playerB = (coords : playerA, playerB)
splitMoves (Msg coords _ prev) playerA playerB = splitMoves prev playerB (coords:playerA)
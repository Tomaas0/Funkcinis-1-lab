module Parse where

    import Data.List
    import Data.Char
    import Entities
    createMessage :: String -> Either String (Msg, String)
    createMessage msg =
      case parse msg (Msg ("0", "0") "" Empty) of
          Right (message, msg1) -> Right (message, msg1)
          Left errorMsg -> Left errorMsg
    
    parse :: String -> Msg -> Either String (Msg, String)
    parse message (Msg iniCoord iniRes prev) =
      case message of 
        ('{':message) ->
          parse message (Msg iniCoord iniRes prev)
        ('\"':'c':'o':'o':'r':'d':'\"':':':message) ->  
          case parseCoords message of 
            Left error -> Left error
            Right ((coord1, coord2), message) ->
              parse (newMessage) newMsg 
              where
                newMsg = Msg (coord1, coord2) iniRes prev
                newMessage = case message of
                    (',':x) -> drop 1 message
                    (x) -> message
        ('\"':'r':'e':'s':'u':'l':'t':'\"':':':message) ->
            let 
                Right (result, rest) = getStringFromBrackets message
                restmsg = case rest of
                    (',':x) -> drop 1 rest
                    (x) -> rest
            in
                case result of
                    "HIT" -> parse restmsg (Msg iniCoord result prev)
                    "MISS" -> parse restmsg (Msg iniCoord result prev)
                    _ -> Left "HIT or MISS expected"
        ('\"':'p':'r':'e':'v':'\"':':':message) ->
            let 
              (coord1, coord2) = iniCoord
              some :: Either String (String, (String, String), String)
              some = 
                if iniRes == "" && coord1 /= "0"
                then 
                  case parseResult message of
                    Left err -> Left err
                    Right (r, rest) -> Right (r, iniCoord, rest)
                else if iniRes /= "" && coord1 == "0"
                then 
                  case parseCoordsAfter message of
                    Left err -> Left err
                    Right (coords, rest) -> Right (iniRes, coords, rest)
                else if iniRes == "" && coord1 == "0" && checkIfCoordsAtTheEnd message
                then 
                  case parseCoordsAfter message of
                    Left err -> Left err
                    Right (c, rest1) -> 
                      case parseResult (rest1 ++ "}") of
                        Left err -> Left err
                        Right (r, rest) -> 
                          Right (r, c, rest)
                else if iniRes == "" && coord1 == "0" && checkIfCoordsAtTheEnd (message) == False
                  then 
                  case parseResult message of
                    Left err -> Left err
                    Right (r, rest1) ->
                      case parseCoordsAfter (rest1 ++ "}") of
                        Left err -> Left err
                        Right (c, rest) ->
                          Right (r, c, rest)
                else Right (iniRes, iniCoord, take (length message - 1) message)
            in
            case some of 
                Left err -> Left err
                Right (result, coord, message) ->
                    parse ("next" ++ message) newMsg 
                    where
                    newMsg =  Msg coord result prev
        ('n':'e':'x':'t':message) ->
          case createMessage message of
            Left error -> Left error
            Right (newMsg, mess) ->
                parse mess (Msg iniCoord iniRes newMsg)        
        ('}':_) ->
          Right (Msg iniCoord iniRes prev, message)
        message ->
          Left "Unecpected sequence of symbols"
    
    parseStr :: String -> Either String (String, String)
    parseStr msg = readStr len $ drop (length lengthAsStr) msg
      where
        lengthAsStr = takeWhile isDigit msg
        len :: Int 
        len = read lengthAsStr
        readStr :: Int -> String ->Either String (String, String)
        readStr n (':':m) = Right (take n m, drop n m)
        readStr n m = Left "Colon expected "

    isNotBracket :: Char -> Bool
    isNotBracket '"' = False
    isNotBracket _ = True

    getStringFromBrackets :: String -> Either String (String, String)
    getStringFromBrackets ('\"':t) = 
        let 
            nameAsStr = takeWhile isNotBracket t
            rest = drop (length nameAsStr + 1) t
        in Right (nameAsStr, rest)
    getStringFromBrackets (_) = Left "Quotation expected"
    
    parseCoords :: String -> Either String ((String, String), String)
    parseCoords ('[':']':msg) = Right (("1", "0"), msg)
    parseCoords ('[':msg) = 
      case getStringFromBrackets msg of 
        Left error1 -> Left error1
        Right (r1,l1) ->
          case getStringFromBrackets (drop 1 l1) of
            Left error2 -> Left error2
            Right (r2, ']':l2) -> Right((r1, r2), l2)
            Right (_, _) -> Left "End of list expected"
    parseCoords _ = Left "List opening expected"
    
    parseCoordsAfter :: String -> Either String ((String, String), String)
    parseCoordsAfter message = parseResultz (reverse message)
      where 
        parseResultz ('}':message) = parseResults (reverse(drop 7 (dropWhile (/='d') message))) ("\"coord" ++ reverse (takeWhile (/= 'd') message))
          where 
            parseResults message ('\"':'c':'o':'o':'r':'d':'\"':':':tempMessage) = 
              case parseCoords tempMessage of 
                Left error -> Left error
                Right (result, _) -> Right (result, message)
            parseResults _ _ = Left "Result expected"      
        parseResultz _ = Left "End of dictionary expected"
    
    checkIfCoordsAtTheEnd :: String -> Bool
    checkIfCoordsAtTheEnd (message) = goNext (reverse message)
      where 
        goNext ('}' : ']' : message) = True
        goNext (message) = False

    parseResult :: String -> Either String (String, String)
    parseResult message = parseResultz (reverse message)
      where 
        parseResultz ('}':message) = parseResults (reverse(drop 8 (dropWhile (/='t') message))) ("\"result" ++ reverse (takeWhile (/= 't') message))
          where 
            parseResults message ('\"':'r':'e':'s':'u':'l':'t':'\"':':':tempMessage) = 
              case getStringFromBrackets tempMessage of 
                Left error -> Left error
                Right (result, []) -> 
                  case result of
                    "HIT" -> Right (result, message)
                    "MISS" -> Right (result, message)
                    _ -> Left "HIT or MISS expected"
                Right (_, _) -> Left "No more symbols expected"
            parseResults _ _ = Left "Result expected"      
        parseResultz _ = Left "End of dictionary expected"


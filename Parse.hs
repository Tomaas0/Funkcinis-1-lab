module Parser where

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
        ('\"':'p':'r':'e':'v':'\"':':':message) ->
          case parseResult message of 
            Left error -> Left error
            Right (result, message) ->
              parse ("next" ++ message) newMsg 
              where
                newMsg =  Msg iniCoord result prev
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
    parseCoords ('[':']':msg) = Right (("0", "0"), msg)
    parseCoords ('[':msg) = 
      case getStringFromBrackets msg of 
        Left error1 -> Left error1
        Right (r1,l1) ->
          case getStringFromBrackets (drop 1 l1) of
            Left error2 -> Left error2
            Right (r2, ']':l2) -> Right((r1, r2), l2)
            Right (_, _) -> Left "End of list expected"
    parseCoords _ = Left "List opening expected"
    
    parseResult :: String -> Either String (String, String)
    parseResult message = parseResultz (reverse message)
      where 
        parseResultz ('}':message) = parseResults (reverse(drop 8 (dropWhile (/='t') message))) ("\"result" ++ reverse (takeWhile (/= 't') message))--change
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


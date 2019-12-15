{-# LANGUAGE OverloadedStrings #-}
module Script2 where

import Parse
import Parse2
import Entities
import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as B

main :: String -> String -> IO ()
main gameId gameMode = do
    case gameMode of
        "attack" -> attack gameId
        "defence" -> defence gameId "B"
        _ -> putStrLn "Game mode has to be \"attack\" or \"defence\""

attack :: String -> IO()
attack gameId = do
    let s :: String
        s = "{\"coord\":[\"A\",\"1\"]}"
    let opts = defaults & header "Content-type" .~ ["application/json"]
    postWith opts ("http://battleship.haskell.lt/game/" ++ gameId ++ "/player/A") $ B.pack s
    defence gameId "A"

defence :: String -> String -> IO()
defence gameId player = do
    let opts = defaults & header "Accept" .~ ["application/json"]
    r <- getWith opts  ("http://battleship.haskell.lt/game/" ++ gameId ++ "/player/" ++ player)
    case r ^. responseStatus . statusCode of
        200 -> do        
            let mov :: String
                mov = B.unpack (r ^. responseBody)
            putStrLn ("From request: " ++ mov)
            case parse mov (Msg ("0", "0") "" Entities.Empty) of
                Left e -> putStrLn ("parser error: " ++ e)
                Right(msg, _) -> case makeResponse msg of
                    Left mess -> case mess of
                        'W' : 'o' : 'n' : rest -> do
                            putStrLn ("Won: " ++ rest)
                            -- let opts = defaults & header "Content-type" .~ ["application/json"]
                            -- postWith opts ("http://battleship.haskell.lt/game/" ++ gameId ++ "/player/" ++ player) $ B.pack rest
                            putStrLn "I just won the game."
                        'L' : 'o' : 's' : 't' : rest -> do
                            putStrLn ("Lost: " ++ rest)
                            let opts = defaults & header "Content-type" .~ ["application/json"]
                            postWith opts ("http://battleship.haskell.lt/game/" ++ gameId ++ "/player/" ++ player) $ B.pack rest
                            putStrLn "I lost the game..."
                        error -> putStrLn error
                    Right mess -> do
                        putStrLn("makeResponse: " ++ mess)
                        let opts = defaults & header "Content-type" .~ ["application/json"]
                        postWith opts ("http://battleship.haskell.lt/game/" ++ gameId ++ "/player/" ++ player) $ B.pack mess
                        defence gameId player  
        _ -> putStrLn $ "http request failed (return code " ++ show (r ^. responseStatus . statusCode) ++ ")"      
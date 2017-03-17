{-# LANGUAGE OverloadedStrings  #-}

module Main(main) where 

import Server
import qualified Data.ByteString as DB 

route1 = route ("GET", "/") (\req -> do
	let stat = "200"
	let msg = "OK"
	bod <- DB.readFile "purple.png"
 	return $ sendResponse stat msg bod )

main :: IO () 
main = do
	let serv = server ([route1])
	runServer serv 7000
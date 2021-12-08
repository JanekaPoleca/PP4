module Cipher.Cesar
( 

) where

    enc :: String -> Int -> String 
    enc xs key = map (\x -> enc' x key ) 

    enc' :: Char -> Int -> Char 
    enc' ch key = 'c'

    dec :: String -> Int -> String 


    
module Ciphers
(cesar
,vigenere
) where

    import Data.Char ( ord, isAlpha, isUpper, chr )

    cesar :: String -> Int -> String -> String 
    cesar dir key =
        let shift = if dir == "enc" then key else -key
        in map (`shiftCes` shift)

    shiftCes :: Char -> Int -> Char 
    shiftCes ch shift
        |isAlpha ch = chr $ ord start + (ord ch - ord start + shift) `mod` 26
        |otherwise = ch
        where start = if isUpper ch then 'A' else 'a'

    vigenere :: String -> String -> String -> String
    vigenere dir key xs =
        let ctrl = if dir == "enc" then 1 else -1
        in shiftVig xs (cycle (vigKey key)) ctrl

    shiftVig :: String -> [Int] -> Int -> String 
    shiftVig [] _ _  = []
    shiftVig _ [] _  = error "empty key"
    shiftVig (x:xs) (k:key) ctrl
        |isAlpha x = shiftCes x (k*ctrl):shiftVig xs key ctrl
        |otherwise = x:shiftVig xs (k:key) ctrl 
    
    vigKey :: String -> [Int]
    vigKey = map (\x -> (ord x - ord 'A') `mod` 26) 
module Cipher.Cesar
( process
) where
    --Considerar implementar um tipo de dados que guarde direcao, key e lista
    import Data.Char ( ord, isAlpha, isUpper, chr )

    process :: String -> Int -> String -> String 
    process dir key = map (\x -> encdec dir x key)

    encdec :: String -> Char -> Int -> Char 
    encdec dir ch key
        |isAlpha ch = chr (ord start + (ord ch - ord start + shift) `mod` 26) 
        |otherwise = ch
        where start = if isUpper ch then 'A' else 'a'
              shift = if dir == "enc" then key else -key




    
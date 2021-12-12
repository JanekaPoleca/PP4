import System.Environment
import Ciphers
main = do 
    (cipher:direction:key) <- getArgs 
    let (Just action) = lookup cipher dispatch
    action (cipher:direction:key)

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("cesar", cesar)  
            , ("vigenere", vigenere)  
            , ("substitution", substitution)  
            ]
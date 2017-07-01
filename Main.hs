import Regex
import Filecontrol

main :: IO()
main = do putStr "> "
          c <- getLine
          command c 

divideString :: String -> String
divideString s = drop 1 $ snd $ span (/= ' ') s

command :: String -> IO ()
command input = do case (fst $ span (/= ' ') input) of
                          ":f" -> divideFile $ divideString input
                          ":l" -> openTags $ divideString input
                          ":o" -> changePath $ divideString input
                          ":p" -> divideInput $ divideString input
                          ":s" -> saveTags 
                          ":q" -> clearMemory
                          otherwise -> if ((head input) == ':')
                                          then
                                              putStrLn "[ERROR] Comando inválido"
                                          else 
                                              case (verifyTags input) of
                                                    Left _ -> memoryTags input 
                                                    Right s -> putStrLn s
                                            
                   if not((fst $ span (/= ' ') input) == ":q") then main else putStr " "

divideFile :: String -> IO ()
divideFile = undefined

divideInput :: String -> IO ()
divideInput = undefined
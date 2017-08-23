--Matheus Ikeda 14.1.8070
--Sabrina Fermano 13.1.8474

import Regex
import Filecontrol
import Control.Exception
import System.IO.Error
import qualified System.IO.Strict as S

--import Automata

divideString :: String -> String -- separa a entrada tendo o espaco como divisor
divideString s = drop 1 $ snd $ span (/= ' ') s

command :: [Tag] -> IO ()
command t = do input <- getLine    
               case (fst $ span (/= ' ') input) of
                    ":f" -> do divideFile (divideString input) t
                               command t
                    ":l" -> do tag <- openTags (divideString input) []
                               command tag
                    ":o" -> do changePath $ divideString input
                               command t
                    ":p" -> do divideInput (divideString input) (reverse t)
                               command t
                    ":s" -> do saveTags $ divideString input
                               command t
                    ":q" -> do putStrLn "[INFO] Saindo do sistema"
                               clear "memoriatags.txt"
                               clear "resultado.txt" 
                    otherwise -> if ((head input) == ':')
                                    then 
                                        putStrLn "[ERROR] Comando inválido"
                                    else 
                                        case (verifyTags input) of
                                             Left _ -> do if ((searchTag t (fst $ span (/= ':') input) [] == []))
                                                             then 
                                                                 do memoryTags input
                                                                    putStrLn "[INFO] Tag adicionada"                                                                                                                                   
                                                                    command ((convToTags input):t)                                              
                                                             else
                                                                 do putStrLn "[ERROR] Tag ja adicionada"  
                                                                    command t                                                      
                                             Right s -> do putStrLn s
                                                           command t
divideFile :: String -> [Tag] -> IO ()
divideFile [] _ = putStrLn "[ERROR] Falta de parâmetro"
divideFile s t = do {catch (open) ex;}
                    where 
                         open = do
                                  input <- readFile s
                                  divideInput input t
                         ex erro = if isDoesNotExistError erro
                                      then do
                                             putStrLn "[ERROR] Arquivo inexistente "
                                      else ioError erro  
                  
divideInput :: String -> [Tag] -> IO ()
divideInput [] _ = putStrLn "[ERROR] Falta de parâmetro"
divideInput s t = do let r = (printTag' (divide (quebra s []) t ([],[])) "")  
                     putStrLn r
                     writeFile "resultado.txt" r

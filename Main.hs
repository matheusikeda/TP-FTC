--Matheus Ikeda 14.1.8070
--Sabrina Fermano 13.1.8474
module Main where

import Regex
import Filecontrol

main :: IO()
main = do c <- getLine
          command c 

divideString :: String -> String -- separa a entrada tendo o espaco como divisor
divideString s = drop 1 $ snd $ span (/= ' ') s

command :: String -> IO ()
command input = do case (fst $ span (/= ' ') input) of
                          ":f" -> divideFile $ divideString input
                          ":l" -> openTags $ divideString input
                          ":o" -> changePath $ divideString input
                          ":p" -> divideInput $ divideString input
                          ":s" -> saveTags 
                          ":q" -> do clear "memoriatags.txt"
                                     clear "caminho.txt"
                                     clear "defstags.txt"
                                     putStrLn "[INFO] Saindo do sistema"
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
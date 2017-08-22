--Matheus Ikeda 14.1.8070
--Sabrina Fermano 13.1.8474
module Filecontrol where

import Control.DeepSeq
import Control.Exception
import System.IO.Error
import qualified System.IO.Strict as S
import Regex

memoryTags :: String -> IO() -- memoria de tags
memoryTags s = do mem <- readFile "memoriatags.txt"
                  let aux = mem ++ s ++ "\n"
                  aux `deepseq` (writeFile "memoriatags.txt" aux)
                 -- putStrLn "[INFO] Tag adicionada"

saveTags :: String -> IO() -- salva as tags definidas 
saveTags [] = putStrLn "[ERROR] Falta de parâmetro"
saveTags s = do c <- readFile "memoriatags.txt"
                writeFile s c
                putStrLn "[INFO] Definições de tags salvas"

openTags :: String -> [Tag] -> IO [Tag] -- abre um arquivo contendo definicoes de tags
openTags [] _ = do putStrLn "[ERROR] Falta de parâmetro" 
                   return []
openTags s t = do {catch (open) ex;}
                   where 
                        open = do
                                  input <- readFile s
                                  let t = map convToTags (lines input)
                                  putStrLn "[INFO] Definições de tags carregadas"
                                  return t
                        
                        ex erro = if isDoesNotExistError erro
                                     then do
                                             putStrLn "[ERROR] Arquivo inexistente "
                                             return []
                                     else ioError erro                   

changePath :: String -> IO() -- muda o caminho de saida 
changePath [] = putStrLn "[ERROR] Falta de parâmetro"
changePath s = do path <- readFile "resultado.txt"
                  let aux = path ++ "\n"
                  aux `deepseq` (writeFile s aux)                  
                  putStrLn "[INFO] Resultado escrito no caminho escolhido"   

clear :: String -> IO() -- limpa arquivos
clear s = do input <- S.readFile s
             writeFile s ""

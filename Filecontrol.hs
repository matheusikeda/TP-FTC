--Matheus Ikeda 14.1.8070
--Sabrina Fermano 13.1.8474
module Filecontrol where

import Control.DeepSeq
import Control.Exception
import System.IO.Error
import qualified System.IO.Strict as S

memoryTags :: String -> IO() -- memoria de tags
memoryTags s = do mem <- readFile "memoriatags.txt"
                  let aux = mem ++ s ++ "\n"
                  aux `deepseq` (writeFile "memoriatags.txt" aux)
                  putStrLn "[INFO] Tag adicionada"

saveTags :: IO() -- salva as tags definidas 
saveTags = do s <- readFile "memoriatags.txt"
              writeFile "defstags.txt" s
              putStrLn "[INFO] Definições de tags salvas"

openTags :: String -> IO () -- abre um arquivo contendo definicoes de tags
openTags [] = putStrLn "[ERROR] Falta de parâmetro"
openTags s = do {catch (open) ex;}
                   where 
                        open = do
                        {
                           input <- readFile s;
                           putStrLn input;
                           putStrLn "[INFO] Definições de tags carregadas";
                        }
                        ex erro = if isDoesNotExistError erro
                           then do
                        {
                           putStrLn "[ERROR] Arquivo inexistente ";
                        }
                        else ioError erro                   

changePath :: String -> IO() -- muda o caminho de saida 
changePath [] = putStrLn "[ERROR] Falta de parâmetro"
changePath s = do path <- writeFile "caminho.txt" s
                  putStrLn "[INFO] Caminho de saída carregado"   

clear :: String -> IO() -- limpa arquivos
clear s = do input <- S.readFile s
             writeFile s ""

module Filecontrol where

import Control.DeepSeq
import Control.Exception
import System.IO.Error
import qualified System.IO.Strict as S

memoryTags :: String -> IO()
memoryTags s = do mem <- readFile "memoriatags.txt"
                  let aux = mem ++ s ++ "\n"
                  aux `deepseq` (writeFile "memoriatags.txt" aux)
                  putStrLn "[INFO] Tag adicionada"

saveTags :: IO()
saveTags = do s <- readFile "memoriatags.txt"
              writeFile "defstags.txt" s
              putStrLn "[INFO] Definições de tags salvas"

openTags :: String -> IO ()
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

changePath :: String -> IO()
changePath [] = putStrLn "[ERROR] Falta de parâmetro"
changePath s = do path <- writeFile "caminho.txt" s
                  putStrLn "[INFO] Caminho de saída carregado"   

clear :: String -> IO()
clear s = do input <- S.readFile s
             writeFile s ""

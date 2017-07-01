module Filecontrol where

import Control.DeepSeq
import Control.Exception
import System.IO.Error
import qualified System.IO.Strict as S

clearMemory :: IO()
clearMemory = do input <- S.readFile "memoriatags.txt"
                 writeFile "memoriatags.txt" ""
                 putStrLn "[INFO] Saindo do sistema"

memoryTags :: String -> IO()
memoryTags [] = putStrLn "[ERROR] Falta de parâmetro"
memoryTags s = do mem <- readFile "memoriatags.txt"
                  let aux = mem ++ s ++ "\n"
                  aux `deepseq` (writeFile "memoriatags.txt" aux)

saveTags :: IO()
saveTags = do s <- readFile "memoriatags.txt"
              writeFile "defstags.txt" s

openTags :: String -> IO ()
openTags [] = putStrLn "[ERROR] Falta de parâmetro"
openTags s = do {catch (open) ex;}
                   where 
                        open = do
                        {
                           input <- readFile s;
                           putStrLn input;
                           putStrLn "[INFO] As definições de tags foram carregadas";
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
                  putStrLn "[INFO] O caminho de saída foi carregado"   

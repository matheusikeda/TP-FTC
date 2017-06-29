import Regex
import System.IO
import Control.Exception
import System.IO.Error

leitura :: IO()
leitura = do putStr "> "
             c <- getLine
             comandos c 

divideString :: String -> String
divideString s = drop 1 $ snd $ span (/= ' ') s

comandos :: String -> IO ()
comandos entrada = do case (fst $ span (/= ' ') entrada) of
                        ":f" -> divArquivo $ divideString entrada
                        ":l" -> carregaTags $ divideString entrada
                        ":o" -> saidaTags $ divideString entrada
                        ":p" -> divEntrada $ divideString entrada
                        ":s" -> salvaTags $ memoriaTags entrada
                        ":q" -> sair (fst $ (span (/= ' ') entrada))
                        otherwise -> memoriaTags $ convToTags entrada
                      putStrLn "Operação concluída"
                      if not((fst $ span (/= ' ') entrada) == ":q") then leitura else putStr " "

sair :: String -> IO ()
sair com 
   | com == ":q" = putStrLn "[INFO] Saindo do sistema"
   | otherwise = putStrLn "[ERROR] Operação Inválida"

memoriaTags :: String -> String
memoriaTags s = verifyTags s ++ "\n"  


salvaTags :: String -> IO()
salvaTags s = salva <- writeFile "deftags.txt" s


carregaTags :: String -> IO ()
carregaTags s = do {catch (carrega) ex;}
                   where 
                        carrega = do
                        {
                           entrada <- readFile s;
                           putStrLn entrada;
                           putStrLn "[INFO] As definicoes de tags foram carregadas";
                        }
                        ex erro = if isDoesNotExistError erro
                           then do
                        {
                           putStrLn "[ERROR] Arquivo inexistente ";
                           leitura;
                        }
                        else ioError erro                   

saidaTags :: String -> IO()
saidaTags s = do caminho <- writeFile "caminho.txt" s; 
                 putStrLn "[INFO] O caminho de saida foi carregado";     

divArquivo :: String -> IO ()
divArquivo = undefined

divEntrada :: String -> IO ()
divEntrada = undefined
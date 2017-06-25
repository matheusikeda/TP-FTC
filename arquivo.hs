import System.IO
import Data.Char(toUpper)
main :: IO ()
main = do entrada <- openFile "entra.txt" ReadMode
          saida <- openFile "sai.txt" WriteMode
          loop_principal entrada saida
          hClose entrada
          hClose saida

loop_principal :: Handle -> Handle -> IO ()
loop_principal entrada saida = do fim_de_arquivo <- hIsEOF entrada
                                  if fim_de_arquivo then return ()
                                                    else do inpStr <- hGetLine entrada
                                                            hPutStrLn saida (map toUpper inpStr)
                                                            loop_principal entrada saida

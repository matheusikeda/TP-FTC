menu :: IO ( )
menu = do putStr "Comandos: "
	   putStrLn ":f - divisão em tags da string do arquivo informado"
	   putStrLn ":l - carrega um arquivo com definições de tags"
	   putStrLn ":o - especifica caminho do arquivo de saída para divisão de tags"
	   putStrLn ":p - realiza a divisão em tags da entrada informada"
	   putStrLn ":q - sair do programa"
	   putStrLn ":s - salvar as tags"
	   le_opcao

le_opcao :: IO ( )
le_opcao = do opcao <- getChar
		   putStr "\n"
		   f_menu opcao

f_menu :: Char -> IO ( )
f_menu s = do case s of
				’:f’ -> 
				’:l’ -> 
				’:o’ -> 
				’:p’ -> 
				’:s’ -> 
				otherwise -> sair s
		      putStrLn "Operação concluída"
			  if not(s == ’:q’) then menu else putStr " "

sair :: Char -> IO ( )
sair s
   | s == ’:q’ = putStrLn "Saindo do sistema . . . "
   |otherwise = putStrLn "Operação Inválida . . ."
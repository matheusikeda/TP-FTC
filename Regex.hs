--Matheus Ikeda 14.1.8070
--Sabrina Fermano 13.1.8474
module Regex where
import Token
import Data.List
import Data.Either.Utils

data  Reg = Lambda      --Define um tipo de ER
          | Literal Char
          | Or Reg Reg
          | Conct Reg Reg
          | Star Reg
          deriving (Eq, Show)

type Tag = (String, [Reg])

literals :: Reg -> [Char]     --Recebe uma ER e retorna uma cadeia de caracter nos moldes da notação polonesa inversa
literals Lambda = "\\l"
literals (Literal ch) = [ch]
literals (Or r1 r2) = literals r2 ++ literals r1 ++ "+"
literals (Conct r1 r2) = literals r2 ++ literals r1 ++ "."
literals (Star r) = literals r ++ "*"

splits :: [a] -> [([a],[a])]
splits st = [splitAt n st | n <- [0 .. length st]]

frontSplits :: [a] -> [([a],[a])]
frontSplits st = [splitAt n st | n <- [1  .. length st]]

matches :: Reg -> String -> Bool --verifica, dado uma ER, se a palavra faz parte 
matches Lambda st = (st == "")
matches (Literal ch) st = (st == [ch])
matches (Or r1 r2) st = matches r1 st || matches r2 st
matches (Conct r1 r2) st = or [matches r1 s1 && matches r2 s2 | (s1,s2) <- splits st]
matches (Star r) st = matches Lambda st || or [matches r s1 && matches (Star r) s2 | (s1,s2) <- frontSplits st]

verifyTags :: String -> Either Reg String   --Verifica se a tag foi definida seguindo os parâmetros definidos
verifyTags t = case (snd $ convToTags t) of 
                 [a] -> Left a
                 otherwise -> Right "[ERROR] Definição de tag incorreta" 

convToTags :: String -> Tag     --Divide uma string em uma tag 
convToTags s = (a, conv [] (drop 2 (convToToken [] b)))
             where (a,b) = span (/= ':') s

conv :: [Reg] -> [Token] -> [Reg]    --Recebe uma lista de ER's e uma lista de tokens e retorna uma lista de regs
conv xs [] = xs
conv (oe:od:xs) (TChar '+':ys) = conv (Or od oe:xs) ys
conv (oe:od:xs) (TChar '.':ys) = conv (Conct od oe:xs) ys
conv (o:xs) (TChar '*':ys) = conv (Star o:xs) ys
conv xs (TChar y:ys) = conv (Literal y:xs) ys
conv xs (TPlus:ys) = conv (Literal '+':xs) ys
conv xs (TConct:ys) = conv (Literal '.':xs) ys
conv xs (TTimes:ys) = conv (Literal '*':xs) ys
conv xs (TBackslash:ys) = conv (Literal '\\':xs) ys
conv xs (TLambda:ys) = conv (Lambda:xs) ys
conv xs (TNewline:ys) = conv (Literal '\n':xs) ys

searchTag :: [Tag] -> String -> [Bool] -> [Bool] --retorna [] se nao existir a tag procurada
searchTag [] s b = filter (==True) b
searchTag xs s b = if ((fst $ head xs) == s)
                    then 
                        searchTag (tail xs) s (True:b)
                    else
                        searchTag (tail xs) s b 
                        
verifyMatches :: [Tag] -> String -> [String] -> [String] --verifica, dado um conj de Tags, se palavra faz parte de alguma 
verifyMatches [] s [] = []
verifyMatches [] s xs = reverse xs
verifyMatches t s xs = if (matches (head $ snd $ head t) s == True)
                            then verifyMatches (tail t) s ((fst $ head t):xs)
                            else verifyMatches (tail t) s xs

quebra :: String -> [String] -> [String] --encontra as combinacoes da palavra
quebra [] st = st 
quebra s st = quebra (tail s) (st ++ (tail $ inits s))

divide :: [String] -> [Tag] -> ([String], String) -> ([String], String) --retorna uma tupla (divisao em tags, aviso) 
divide [] t (a,b) = (a,b)
divide s t (a,b) = case (length (verifyMatches t (head s) [])) of
                        0 -> divide (tail s) t (a,b) 
                        1 -> divide (tail s) t (a ++ [head (verifyMatches t (head s) [])], "") 
                        otherwise -> divide (tail s) t (a ++ [head (verifyMatches t (head s) [])], "[WARNING] Sobreposicao de tags " ++ (printTag (verifyMatches t (head s) []) []))                       
      
printTag :: [String] -> String -> String --imprime uma lista de tags
printTag [] ys = tail ys
printTag xs ys = printTag (tail xs) (ys ++ " " ++ head xs) 

printTag' :: ([String],String) -> String -> String --imprime uma tupla (divisao em tags,aviso) {aviso vazio ou nao}
printTag' ([],b) xs = (tail xs) ++ "\n" ++ b
printTag' (a,b) xs = printTag' ((tail a),b) (xs ++ " " ++ (head a)) 

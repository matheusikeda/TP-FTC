module Regex where
import Token

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

--splits :: [a] -> [([a],[a])]
--splits st = [splitAt n st | n <- [0 .. length st]]

--frontSplits :: [a] -> [([a],[a])]
--frontSplits st = [splitAt n st | n <- [1  .. length st]]

--matches :: Reg -> String -> Bool
--matches Epsilon st = (st == "")
--matches (Literal ch) st = (st == [ch])
--matches (Or r1 r2) st = matches r1 st || matches r2 st
--matches (Conct r1 r2) st = or [matches r1 s1 && matches r2 s2 | (s1,s2) <- splits st]
--matches (Star r) st = matches Epsilon st || or [matches r s1 && matches (Star r) s2 | (s1,s2) <- frontSplits st]

verifyTags :: String -> Either Reg String   --Verifica se a tag foi definida seguindo os parâmetros definidos
verifyTags t = case (snd $ convToTags t) of 
                 [a] -> Left a
                 otherwise -> Right "[ERROR] Definição de tag incorreta" 

convToTags :: String -> Tag     --Divide uma string em uma tag até os 2 pontos e dos 2 pontos em diante em ER
convToTags s = (a, conv [] (drop 2 (convToToken [] b)))
             where (a,b) = span (/= ':') s

conv :: [Reg] -> [Token] -> [Reg]    --Recebe uma lista de ER's e uma lista de tokens e retorna uma lista de regs
conv xs [] = xs
conv (oe:od:xs) (TChar '+':ys) = conv (xs++[Or oe od]) ys
conv (oe:od:xs) (TChar '.':ys) = conv (xs++[Conct oe od]) ys
conv (o:xs) (TChar '*':ys) = conv (xs++[Star o]) ys
conv xs (TChar y:ys) = conv (xs++[Literal y]) ys
conv xs (TPlus:ys) = conv (xs++[Literal '+']) ys
conv xs (TConct:ys) = conv (xs++[Literal '.']) ys
conv xs (TTimes:ys) = conv (xs++[Literal '*']) ys
conv xs (TBackslash:ys) = conv (xs++[Literal '\\']) ys
conv xs (TLambda:ys) = conv (xs++[Lambda]) ys
conv xs (TNewline:ys) = conv (xs++[Literal '\n']) ys

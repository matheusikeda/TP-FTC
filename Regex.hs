module Regex where

data  Reg = Epsilon
          | Literal Char
          | Or Reg Reg
          | Conct Reg Reg
          | Star Reg
          deriving (Eq, Show)

type Tag = (String, Reg) 

literals :: Reg -> [Char]
literals Epsilon = []
literals (Literal ch) = [ch]
literals (Or r1 r2) = literals r2 ++ literals r1 ++ "+"
literals (Conct r1 r2) = literals r2 ++ literals r1 ++ "."
literals (Star r) = literals r ++ "*"

splits :: [a] -> [([a],[a])]
splits st = [splitAt n st | n <- [0 .. length st]]

frontSplits :: [a] -> [([a],[a])]
frontSplits st = [splitAt n st | n <- [1  .. length st]]

matches :: Reg -> String -> Bool
matches Epsilon st = (st == "")
matches (Literal ch) st = (st == [ch])
matches (Or r1 r2) st = matches r1 st || matches r2 st
matches (Conct r1 r2) st = or [matches r1 s1 && matches r2 s2 | (s1,s2) <- splits st]
matches (Star r) st = matches Epsilon st || or [matches r s1 && matches (Star r) s2 | (s1,s2) <- frontSplits st]

modelTags :: String -> String
modelTags t =  (fst (convToTags t)) ++ ": " ++ (literals $ snd (convToTags t))


convToTags :: String -> Tag
convToTags s = (a, conv [] (drop 2 b))
             where (a,b) = span (/= ':') s

conv :: [Reg] -> String -> Reg
conv [xs] [] = xs
conv (oe:od:xs) ('+':ys) = conv (Or oe od : xs) ys
conv (oe:od:xs) ('.':ys) = conv (Conct oe od : xs) ys
conv (o:xs) ('*':ys) = conv (Star o : xs) ys
conv xs (y:ys) = conv (Literal y : xs) ys


data  Reg = Epsilon
          | Literal Char
          | Or Reg Reg
          | Then Reg Reg
          | Star Reg
          deriving Eq

literals :: Reg -> [Char]
literals Epsilon = []
literals (Literal ch) = [ch]
literals (Or r1 r2) = literals r1 ++ literals r2
literals (Then r1 r2) = literals r1 ++ literals r2
literals (Star r) = literals r

splits :: [a] -> [([a],[a])]
splits st = [splitAt n st | n <- [0 .. length st]]

frontSplits :: [a] -> [([a],[a])]
frontSplits st = [splitAt n st | n <- [1 .. length st]]

matches :: Reg -> String -> Bool
matches Epsilon st = (st == "")
matches (Literal ch) st = (st == [ch])
matches (Or r1 r2) st = matches r1 st || matches r2 st
matches (Then r1 r2) st = or [matches r1 s1 && matches r2 s2 | (s1,s2) <- splits st]
matches (Star r) st = matches Epsilon st || or [matches r s1 && matches (Star r) s2 | (s1,s2) <- frontSplits st]

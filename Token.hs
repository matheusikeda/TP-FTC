module Token where

import qualified Data.Char as C

data Token = TChar Char
            |TNewline 
            |TBackslash
            |TPlus 
            |TConct 
            |TTimes 
            |TLambda
            deriving (Eq, Show) 

convToToken :: [Token] -> String -> [Token]
convToToken xs [] = xs
convToToken xs ('\\':'+':ys) = convToToken (xs++[TPlus]) ys
convToToken xs ('\\':'.':ys) = convToToken (xs++[TConct]) ys
convToToken xs ('\\':'*':ys) = convToToken (xs++[TTimes]) ys
convToToken xs ('\\':'l':ys) = convToToken (xs++[TLambda]) ys
convToToken xs ('\\':'n':ys) = convToToken (xs++[TNewline]) ys
convToToken xs ('\\':'\\':ys) = convToToken (xs++[TBackslash]) ys
convToToken xs (y:ys) = convToToken (xs ++ [TChar y]) ys

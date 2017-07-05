--Matheus Ikeda 14.1.8070
--Sabrina Fermano 13.1.8474
module Token where

import qualified Data.Char as C

data Token = TChar Char     --Define um tipo token
            |TNewline 
            |TBackslash
            |TPlus 
            |TConct 
            |TTimes 
            |TLambda
            deriving (Eq, Show) 

convToToken :: [Token] -> String -> [Token]  --Recebe uma lista inicialmente vazia e uma string e retorna uma lista com tokens
convToToken xs [] = xs
convToToken xs ('\\':'+':ys) = convToToken (xs++[TPlus]) ys
convToToken xs ('\\':'.':ys) = convToToken (xs++[TConct]) ys
convToToken xs ('\\':'*':ys) = convToToken (xs++[TTimes]) ys
convToToken xs ('\\':'l':ys) = convToToken (xs++[TLambda]) ys
convToToken xs ('\\':'n':ys) = convToToken (xs++[TNewline]) ys
convToToken xs ('\\':'\\':ys) = convToToken (xs++[TBackslash]) ys
convToToken xs (y:ys) = convToToken (xs ++ [TChar y]) ys
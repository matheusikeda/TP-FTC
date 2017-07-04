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
convToToken xs ('\\':'+':ys) = convToToken (TPlus:xs) ys
convToToken xs ('\\':'.':ys) = convToToken (TConct:xs) ys
convToToken xs ('\\':'*':ys) = convToToken (TTimes:xs) ys
convToToken xs ('\\':'l':ys) = convToToken (TLambda:xs) ys
convToToken xs ('\\':'n':ys) = convToToken (TNewline:xs) ys
convToToken xs ('\\':'\\':ys) = convToToken (TBackslash:xs) ys
convToToken xs (y:ys) = convToToken (TChar y:xs) ys

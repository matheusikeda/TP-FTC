module Automata where

import qualified Data.Set as Set
import Data.List hiding (union)
import Regex

instance Eq a => Eq (Set a) where
  (==) = eqSet
 
instance Ord a => Ord (Set a) where
  s1 <= s2 = flatten s1 <= flatten s2
  
instance Show a => Show (Set a) where
  show = showSet

--funcoes de conjunto
newtype Set a = SetI [a]

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f (SetI xs) = makeSet (map f xs)

card :: Set a -> Int
card (SetI xs) = length xs


eqSet :: Eq a => Set a -> Set a -> Bool
eqSet (SetI xs) (SetI ys) = (xs == ys)

showSet :: Show a => Set a -> String
showSet (SetI xs) = concat (map ((++"\n") . show) xs)

flatten :: Set a -> [a]
flatten (SetI xs) = xs

empty :: Set a
empty  = SetI []

sing :: a -> Set a
sing x = SetI [x]

union    :: Ord a => Set a -> Set a -> Set a
union (SetI xs) (SetI ys) = SetI (uni xs ys)

uni :: Ord a => [a] -> [a] -> [a]
uni [] ys = ys
uni xs [] = xs
uni (x:xs) (y:ys) 
  | x<y = x : uni xs (y:ys)
  | x==y = x : uni xs ys
  | otherwise = y : uni (x:xs) ys

makeSet :: Ord a => [a] -> Set a
makeSet = SetI . remDups . sort
          where
          remDups []     = []
          remDups [x]    = [x]
          remDups (x:y:xs) 
    	    | x < y 	= x : remDups (y:xs)
            | otherwise = remDups (y:xs)

setlimit :: Eq a => (Set a -> Set a) -> Set a -> Set a
setlimit f s
  | s==next	= s
  | otherwise   = setlimit f next
    where
    next = f s

--estrutura do automato
data Move a = Move a Char a 
              | Emove a a
          deriving (Eq,Ord,Show)
    
data Nfa a  = NFA (Set a)
                  (Set (Move a))
                  a 
                  (Set a)
         deriving (Eq,Show)

--regex2Nfa         
build :: Reg -> Nfa Int

build Lambda = NFA
	        (makeSet [0 .. 1])
	        (sing (Emove 0 1))
	        0
	        (sing 1)
build (Literal c)
	      = NFA
	        (makeSet [0 .. 1])
	        (sing (Move 0 c 1))
	        0
	        (sing 1)
build (Or r1 r2) = m_or (build r1) (build r2)
build (Conct r1 r2) = m_then (build r1) (build r2)
build (Star r) = m_star (build r)

m_or :: Nfa Int -> Nfa Int -> Nfa Int
m_or (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
  = NFA
    (states1' `union` states2' `union` newstates)
    (moves1' `union` moves2' `union` newmoves)
    0
    (sing (m1+m2+1))
    where
        m1 = card states1
        m2 = card states2
        states1' = mapSet (renumber 1) states1
        states2' = mapSet (renumber (m1+1)) states2
        newstates = makeSet [0,(m1+m2+1)]
        moves1'  = mapSet (renumber_move 1) moves1
        moves2'  = mapSet (renumber_move (m1+1)) moves2
        newmoves = makeSet [ Emove 0 1 , Emove 0 (m1+1) , Emove m1 (m1+m2+1) , Emove (m1+m2) (m1+m2+1) ]

m_then :: Nfa Int -> Nfa Int -> Nfa Int
m_then (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
  = NFA
    (union states1 states2')
    (union moves1 moves2')
	start1
	finish2'
	where
	    states2' = mapSet (renumber k) states2
	    moves2'  = mapSet (renumber_move k) moves2
	    finish2' = mapSet (renumber k) finish2
	    k = card states1 - 1

m_star :: Nfa Int -> Nfa Int 
m_star (NFA states moves start finish)
  = NFA
    (states' `union` newstates)
    (moves' `union` newmoves)
    0
    (sing (m+1))
    where
        m = card states
        states' = mapSet (renumber 1) states
        newstates = makeSet [ 0 , m+1 ]
        moves'  = mapSet (renumber_move 1) moves
        newmoves = makeSet [ Emove 0 1 , Emove m 1 , Emove 0 (m+1) , Emove m (m+1) ]

renumber :: Int -> Int -> Int
renumber n st = n + st

renumber_move :: Int -> Move Int -> Move Int
renumber_move k (Move s1 c s2) = Move (renumber k s1) c (renumber k s2)
renumber_move k (Emove s1 s2) = Emove (renumber k s1) (renumber k s2)    

m_or' :: Nfa Int -> Nfa Int -> Nfa Int
m_or' (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
  = NFA
    (states1' `union` states2' `union` newstates)
    (moves1' `union` moves2' `union` newmoves)
    0
    (sing (m1) `union` sing (m1+m2))
    where
        m1 = card states1
        m2 = card states2
        states1' = mapSet (renumber 1) states1
        states2' = mapSet (renumber (m1+1)) states2
        newstates = makeSet [0]
        moves1'  = mapSet (renumber_move 1) moves1
        moves2'  = mapSet (renumber_move (m1+1)) moves2
        newmoves = makeSet [ Emove 0 1 , Emove 0 (m1+1) ]

m_or'' :: Nfa Int -> Nfa Int -> Nfa Int
m_or'' (NFA states1 moves1 start1 finish1) (NFA states2 moves2 start2 finish2)
  = NFA
    (states1 `union` states2')
    (moves1 `union` moves2' `union` newmoves)
    0
    (finish1 `union` sing (m1+m2-1))
    where
        m1 = card states1
        m2 = card states2
        states2' = mapSet (renumber (m1)) states2
        moves2'  = mapSet (renumber_move (m1)) moves2
        newmoves = makeSet [ Emove 0 (m1) ]

closure :: Ord a => Nfa a -> Set a -> Set a
closure (NFA states moves start term)
      = setlimit add
	where
	add stateset = union stateset (makeSet accessible)
		       where
		       accessible
			 = [ s | x <- flatten stateset , 
				 Emove y s <- flatten moves ,
				 y==x ]

onemove :: Ord a => Nfa a -> Char -> Set a -> Set a
onemove (NFA states moves start term) c x
      = makeSet [ s | t <- flatten x , 
		      Move z d s <- flatten moves ,
		      z==t , c==d ]

onetrans :: Ord a => Nfa a -> Char -> Set a -> Set a
onetrans mach c x = closure mach (onemove mach c x)

startstate :: Nfa a -> a
startstate (NFA states moves start finish) = start

trans :: Ord a => Nfa a -> String -> Set a
trans mach str = foldl step startset str
		 where
		 step set ch = onetrans mach ch set
		 startset = closure mach (sing (startstate mach))

print_trans :: Nfa Int -> String -> [Char]
print_trans mach str = show (flatten (trans mach str))
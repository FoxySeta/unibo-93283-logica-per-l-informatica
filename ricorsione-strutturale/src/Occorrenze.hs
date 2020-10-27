module Occorrenze(
    occorrenze
) where

occorrenze :: (Num p, Eq t) => t -> [t] -> p -- Problema 6

{-
    Problema 6: data una lista l e un y, contare il numero di volte che y
    occorre in L.

    Es. occorrenze 3 (1 : 3 : 2 : 3 : 3 : []) = 3
    Soluzione: occorrenze y l
-}

occorrenze _ [] = 0
occorrenze a (x : l) = (if a == x then 1 else 0) + (occorrenze a l)
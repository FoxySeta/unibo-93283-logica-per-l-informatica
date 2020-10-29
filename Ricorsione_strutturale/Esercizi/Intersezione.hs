module Intersezione(
    intersezione,
    appartiene
) where

intersezione :: Eq a => [a] -> [a] -> [a] -- Problema 7
appartiene :: Eq t => t -> [t] -> Bool -- Problema 7.1

{-
    Problema 7: date due liste l1 e l2, restituire la lista degli elementi
    comuni a l1 e l2.

    Es. intersezione (1 : 3 : 4 : 6 : []) (3 : 6 : 2 : 5 : 1 : []) =
        (1 : 3 : 6 : [])
    Soluzione: intersezione l1 l2
-}

intersezione [] _ = []
intersezione (x : l1) l2 = if appartiene x l2 then
                           x : (intersezione l1 l2) else
                           intersezione l1 l2

{-
    Problema 7.1: dato x ed una lista l, restituire "vero" se e solo se x
                  appartiene ad l

    Es. appartiene 6 (3 : 6 : 2 : 5 : 1 : []) = True
    Soluzione: appartiene x l
-}

appartiene _ [] = False
appartiene x (y : l) = x == y || (appartiene x l)
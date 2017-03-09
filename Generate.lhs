> {-# LANGUAGE UnicodeSyntax #-}
>
> module Generate
> where
> -- import Unicode

> bools  ∷  [Bool]
> bools  =  pure False ++ pure True
>
> maybes  ∷  [elem] → [Maybe elem]
> maybes elems  =  pure Nothing ++ (pure Just <*> elems)

> data Suit  =  Spades | Hearts | Diamonds | Clubs
>     deriving(Show)
> data Rank  =  Faceless Integer | Jack | Queen | King
>     deriving(Show)
> data Card  =  Card Rank Suit | Joker
>     deriving(Show)

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)

lists bools 1
lists bools 2
trees (lists bools 2) 1
trees (lists bools 2) 2

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Anna Töskés s1005628

exercise 3.1
============

> suits :: [Suit]
> suits = pure Spades ++ pure Hearts ++ pure Diamonds ++ pure Clubs

> ranks :: [Integer] -> [Rank]
> ranks is = [Faceless i | i <- is] ++ pure Jack ++ pure Queen ++ pure King

ranks :: [Integer] -> [Rank]
ranks is = (pure Faceless <*> is) ++ pure Jack ++ pure Queen ++ pure King

> cards :: [Rank] -> [Suit] -> [Card]
> cards rs ss = [Card r s | r <- rs, s <- ss] ++ pure Joker

cards :: [Rank] -> [Suit] -> [Card]
cards r s = (pure Card <*> r <*> s) ++ pure Joker

exercise 3.2
============

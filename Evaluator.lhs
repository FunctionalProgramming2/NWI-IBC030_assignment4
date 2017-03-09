> {-# LANGUAGE UnicodeSyntax #-}
>
> module Evaluator
> where
> -- import Unicode

> infixl 6 :+:
> infixl 7 :*:
> infixr 1 :?:

> data Expr
>   =  Lit Integer    -- a literal
>   |  Expr :+: Expr  -- addition
>   |  Expr :*: Expr  -- multiplication
>   |  Div Expr Expr  -- integer division
>   |  Expr :?: Expr  -- non-deterministic choice
>   |  Var String     -- a variable

> evalA ∷ (Applicative f) ⇒ Expr → f Integer
> evalA (Lit i)      =  pure i
> evalA (e1 :+: e2)  =  pure (+)  <*> evalA e1 <*> evalA e2
> evalA (e1 :*: e2)  =  pure (*)  <*> evalA e1 <*> evalA e2
> evalA (Div e1 e2)  =  pure div  <*> evalA e1 <*> evalA e2

> toss  ∷  Expr
> toss  =  Lit 0 :?: Lit 1

< evalN ∷ Expr → [Integer]

evalN toss
evalN (toss :+: Lit 2 :*: toss)
evalN (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: (toss :+: Lit 2 :*: toss)))

< evalR ∷ Expr → [(String, Integer)] → Integer

evalR (Var "a" :+: Lit 1) [("a", 4711), ("b", 0815)]
evalR (Var "a" :*: Var "b") [("a", 4711), ("b", 0815)]
evalR (Var "a" :*: Var "c") [("a", 4711), ("b", 0815)]

--------------------------------------------------------------------------------
author: Hendrik Werner s4549775
author: Anna Töskés s1005628

exercise 1
==========

> evalN :: Expr -> [Integer]
> evalN (Lit i) = pure i
> evalN (e1 :+: e2) = pure (+) <*> evalN e1 <*> evalN e2
> evalN (e1 :*: e2) = pure (*) <*> evalN e1 <*> evalN e2
> evalN (Div e1 e2) = pure div <*> evalN e1 <*> evalN e2
> evalN (e1 :?: e2) = evalN e1 ++ evalN e2

exercise 2.1
============

> evalR :: Expr -> [(String, Integer)] -> Integer
> evalR (Lit i) = pure i
> evalR (e1 :+: e2) = pure (+) <*> evalR e1 <*> evalR e2
> evalR (e1 :*: e2) = pure (*) <*> evalR e1 <*> evalR e2
> evalR (Div e1 e2) = pure div <*> evalR e1 <*> evalR e2
> evalR (Var v) = lookup' v
>     where lookup' k kvs = case lookup k kvs of
>               Nothing -> 0
>               Just i -> i

exercise 2.2
============
(optional)

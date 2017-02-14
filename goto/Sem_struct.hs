module Sem_struct
where

import Prelude hiding (length, take, replicate)
import Data.Sequence
import AST

type State = Seq Value

enter :: Int -> State -> State
enter n = (>< replicate n (-1))

leave :: Int -> State -> State
leave n s = take (length s - n) s

step :: Expr -> State -> (Expr, State)
step (Const n)           s = (Const n, s)
step Skip                s = (Const undefined, s)
step (Var i := Const n)  s = (Const n, update i n s)
step (Var i := e)        s = let (e',s') = step e s in (Var i := e', s')
step (Val (Var i))       s = (Const (index s i), s)
step (If (Const 0) b c)  s = (c,s)
step (If (Const _) b c)  s = (b,s)

step (If a b c)          s = let (a',s') = step a s in (If a' b c, s')
step (While a b)         s = (If a (b:::While a b) Skip, s)
step (DyOp f (Const x) (Const y)) s 
                          = (Const (f x y), s)
step (DyOp f a b) s       = let (a',s')  = step a s -- teehees
                                (b',s'') = step b s' in (DyOp f a' b', s'')
step (UnOp f (Const n))  s = (Const (f n), s)
step (UnOp f a)          s = let (a',s') = step a s in (UnOp f a', s')
step (Const _ ::: b)     s = (b,s)
step (a ::: b)           s = let (a',s') = step a s in (a' ::: b, s')

-- this is ugly
step (Scope n (Const k)) s 
  | n < 0                 = (Const k, leave (-n) s)
step (Scope n a)         s 
  | n < 0                 = let (a',s') = step a s in (Scope n a', s')
  | otherwise             = (Scope (-n) a, enter n s)

exec :: Expr -> State -> [(Expr, State)]
exec = curry $ iterate (uncurry step)

sem :: Expr -> State -> Value
sem (Const n) = const n
sem e = uncurry sem . step e

eval :: Expr -> Value
eval e = sem e empty

eval' :: Expr -> [Expr]
eval' e = map fst $ exec e empty

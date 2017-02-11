module Sem_monadic
where

import Prelude hiding (length, take, replicate)
import Data.Sequence
import AST

type State = Seq Value

type ST = State->(State,Value)

val :: Value -> ST
val x s = (s, x)

(>:) :: ST -> (Value -> ST) -> ST
(>:) m f s = let (s',x) = m s in f x s'

sem :: Expr -> ST
sem Skip         = val undefined
sem (Const i)    = val i
sem (Var i := e) = sem e >: \x s->(update i x s, x)
sem (Val (Var i))= \s -> (s, index s i)
sem (If a b c)   = sem a >: \x->sem $ if x/=0 then b else c
sem (While a b)  = sem a >: \x->sem $ if x/=0 then b:::While a b else Skip
sem (DyOp f a b) = sem a >: \x->sem b >: \y-> val $ f x y
sem (UnOp f a)   = sem a >: \x->val $ f x
sem (a ::: b)    = sem a >: \_->sem b

sem (Scope n a)  = (sem a.enter) >: \x s -> (leave s, x)
  where enter s = s >< replicate n undefined
        leave s = take (length s - n) s

eval :: Expr -> Value
eval e = snd $ sem e empty

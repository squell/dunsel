import Prelude hiding (length, take, replicate)
import Data.Sequence
import AST

data State = State { stack :: Seq Value, result :: Value }

type ST = State -> State

val :: Value -> ST
val x s = State (stack s) x

put :: Int -> Value -> ST
put i x s = State (update i x (stack s)) x

get :: Int -> ST
get i s = State (stack s) (index (stack s) i)

enter :: Int -> ST
enter n s = State (stack s>< replicate n undefined) (result s)

leave :: Int -> ST
leave n s = State (take (length (stack s) - n) (stack s)) (result s)

(>:) :: ST -> (Value -> ST) -> ST
(>:) f g s = let s' = f s in g (result s') s'

sem :: Expr -> ST
sem Skip         = val undefined
sem (Const i)    = val i
sem (Var i := e) = sem e >: put i
sem (Val (Var i))= get i
sem (If a b c)   = sem a >: \x->sem $ if x/=0 then b else c
sem (While a b)  = sem a >: \x->sem $ if x/=0 then b:::While a b else Skip
sem (DyOp f a b) = sem a >: \x->sem b >: \y-> val $ f x y
sem (UnOp f a)   = sem a >: \x->val $ f x
sem (a ::: b)    = sem b . sem a

sem (Scope n a)  = leave n.sem a.enter n

import Prelude hiding (length, take, replicate)
import Data.Sequence
import Data.Maybe
import AST

data State = State { stack :: Seq Value, result :: Value, labels :: [(Int,ST)] }

type ST = State -> State
type Cont = ST -> ST

val :: Value -> ST
val x (State stack _ labels) = State stack x labels

put :: Int -> Value -> ST
put i x (State stack _ labels) = State (update i x stack) x labels

get :: Int -> ST
get i (State stack _ labels) = State stack (index stack i) labels

enter :: Int -> ST
enter n (State stack x labels) = State (stack>< replicate n undefined) x labels

leave :: Int -> ST
leave n (State stack x labels) = State (take (length stack - n) stack) x labels

(<@) :: State -> [(Int, ST)] -> State
(State stack x labels) <@ tag = State stack x (tag++labels)

(>:) :: Cont -> (Value->ST) -> Cont
--(>:) f g k = f (\st->k (g (result st) st))
(>:) f g = f >:: (\x->(.g x))
--
(>::) :: Cont -> (Value->Cont) -> Cont
(>::) f g k = f (\st->g (result st) k st)

sem' :: (Expr,Expr) -> Cont
sem' (active,passive) = \k st->sem active k st <@ labels (sem passive k st)

sem :: Expr -> Cont
sem Skip         = (.val undefined)
sem (Const i)    = (.val i)
sem (Var i := e) = sem e >: put i
sem (Val (Var i))= (.get i)
sem (If a b c)   = sem a >:: \x->sem' $ if x/=0 then (b,c) else (c,b)
sem (While a b)  = sem (If a (b:::While a b) Skip)
sem (DyOp f a b) = sem a >:: \x->sem b >: \y-> val $ f x y
sem (UnOp f a)   = sem a >: \x->val $ f x
sem (a ::: b)    = sem a . sem b

sem (Goto i)     = \k st->fromJust (lookup i (labels st)) st
sem (Label i)    = \k->(<@ [(i,k)]).k

-- this is too simplistic
sem (Scope n a)  = \k->sem a (k.leave n).enter n

labels :: Expr -> [Int]
labels (Var _ := e) = labels e
labels (If a b c)   = labels a ++ labels b ++ labels c
labels (While a b)  = labels a ++ labels b
labels (DyOp _ a b) = labels a ++ labels b
labels (UnOp _ a)   = labels a
labels (a ::: b)    = labels a ++ labels b
labels _ = []

module Sem_arrow
where

import Prelude hiding (length, take, replicate)
import Control.Arrow
import Data.Sequence
import AST

type State = Seq Value

type ST = State->(State,Value)

sem :: Expr -> ST
sem Skip         = id &&& undefined
sem (Const i)    = id &&& const i
sem (Var i := e) = (uncurry (flip (update i)) &&& snd) . sem e
sem (Val (Var i))= id &&& flip index i
sem (If a b c)   = uncurry (flip $ \x->if x/=0 then sem b else sem c).sem a
sem (While a b)  = sem (If a (b:::While a b) Skip)
sem (DyOp f a b) = (fst.fst &&& uncurry (flip f).first snd).first (sem b).sem a
sem (UnOp f a)   = (fst &&& f.snd).sem a
sem (a ::: b)    = sem b.fst.sem a

sem (Scope n a)  = (leave.fst &&& snd).sem a.enter
  where enter s = s >< replicate n undefined
        leave s = take (length s - n) s

eval :: Expr -> Value
eval e = snd $ sem e empty

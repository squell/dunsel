module Sem_ferret
where

import Prelude hiding (length, take, replicate)
import Data.Sequence hiding (filter)
import AST

type State = Seq Value

enter :: Int -> State -> State
enter n = (>< replicate n (-1))

leave :: Int -> State -> State
leave n s = take (length s - n) s

step :: (Int->Expr) -> Expr -> State -> (Expr, State)
step go = step
  where
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

    step ((a:::b):::c)       s = (a:::(b:::c), s)
    step (Goto k ::: b)      s = (Goto k, s)

    step (Const _ ::: b)     s = (b, s)
    step (a ::: b)           s = let (a',s') = step a s in (a' ::: b, s')

    step (Label _)           s = (Skip, s)
    step (Goto k)            s = (go k, s)

    -- this is ugly
    step (Scope n (Const k)) s 
      | n < 0                 = (Const k, leave (-n) s)
    step (Scope n a)         s 
      | n < 0                 = let (a',s') = step a s in (Scope n a', s')
      | otherwise             = (Scope (-n) a, enter n s)

ferret :: Expr -> Int -> Expr
ferret e n = until stop ferret e
  where
    stop (Label _ ::: _)      = True
    stop Skip                 = True
    stop x                    = False

    ferret (v := (Label k ::: e))
      | k == n                = Label k ::: v := e
    ferret (v := Skip)        = Skip
    ferret (v := e)           = v := ferret e

    ferret (If (Label k ::: z) b c)
      | k == n                = Label k ::: If z b c
    ferret (If _ (Label k:::z) c)
      | k == n                = Label k ::: z
    ferret (If _ _ (Label k:::z))
      | k == n                = Label k ::: z
    ferret (If Skip Skip c)   = c
    ferret (If Skip b c)      = If Skip (ferret b) c
    ferret (If a b c)         = If (ferret a) b c

    ferret (While a b)        = case (until stop ferret a,until stop ferret b) of
				  (Skip, Skip) -> Skip
				  _            -> If a (b:::While a b) Skip

    ferret (UnOp f (Label k:::z))
      | k == n                = Label k ::: UnOp f z
    ferret (UnOp f e)         = UnOp f (ferret e)

    ferret (DyOp f a b)       = case (until stop ferret a,until stop ferret b) of
				  (Label k ::: z, _) -> Label k ::: DyOp f z b
				  (_, Label k ::: z) -> Label k ::: DyOp f a z
				  _                  -> Skip

    ferret (Label k:::c)
      | k == n                = Label k ::: c
    ferret ((a:::b):::c)      = (a:::(b:::c))
    ferret (Skip:::a)         = ferret a
    ferret (a:::b)            = ferret a:::b

-- jumping across scopes not supported

{-
    ferret (Scope m (Label k:::z))
      | k == n                = Label k ::: Scope m z
    ferret (Scope m a)        = Scope m (ferret a)
-}
    ferret (Scope m a)        = a

    ferret (Label k)
      | k == n                = Label k ::: Skip
    ferret _                  = Skip

exec :: Expr -> State -> [(Expr, State)]
exec = \e -> (curry.iterate.uncurry.step.ferret) e e

sem :: Expr -> State -> Value
sem e s = case filter (done.fst) $ exec e s of (Const n,_):_ -> n
  where done (Const _) = True
        done _         = False

eval :: Expr -> Value
eval e = sem e empty

eval' :: Expr -> [Expr]
eval' e = map fst $ exec e empty

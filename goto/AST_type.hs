module AST_type
where

data Void = Void

data Expr t where
	Scope :: Int -> Expr t -> Expr t
	Skip :: Expr Void
	Const :: Int -> Expr Int
	Val :: Var -> Expr Int
	(:=) :: Var -> Expr Int -> Expr Int
	If :: Expr Int -> Expr u -> Expr u -> Expr u
	While :: Expr Int -> Expr t -> Expr Void
	DyOp :: (Int->Int->Int) -> Expr Int -> Expr Int -> Expr Int
	UnOp :: (Int->Int) -> Expr Int -> Expr Int
	(:::) :: Expr v -> Expr u -> Expr u
	Label :: Int -> Expr Void
	Goto :: Int -> Expr Void
	
newtype Var = Var Int

type Value = Int

infixr 7 :=
infixl 6 :::

module AST
where

data Expr = 
	Scope Int Expr |
	Skip |
	Const Value |
	Val Var |
	Var := Expr |
	If Expr Expr Expr |
	While Expr Expr |
	DyOp (Value->Value->Value) Expr Expr |
	UnOp (Value->Value) Expr |
	Expr ::: Expr |
	Label Int |
	Goto Int
	
newtype Var = Var Int 

type Value = Int

infixr 7 :=
infixl 6 :::

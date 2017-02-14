import AST
import Sem_monadic as M
import Sem_state as B
import Sem_cont as K
import Sem_arrow as A
import Sem_struct as S

wh1 = Scope 10 (
       Var 1 := Const 5 :::
       Var 2 := Const 1 :::
       While (Val (Var 1)) (
          Var 2 := DyOp (*) (Val (Var 1)) (Val (Var 2)) :::
          Var 1 := DyOp (-) (Val (Var 1)) (Const 1)
       ) :::
       Val (Var 2)
      )

wh2 = Scope 10 (
       Var 1 := Const 5 :::
       Var 2 := Const 1 :::
       Label 42 :::
       Var 2 := DyOp (*) (Val (Var 1)) (Val (Var 2)) :::
       If (Var 1 := DyOp (-) (Val (Var 1)) (Const 1)) (
         Goto 42
       ) Skip :::
       Val (Var 2)
      )

wh3 = Scope 10 (
       Var 1 := Const 5 :::
       Var 2 := Const 1 :::
       While (Const 1) (
          Var 2 := DyOp (*) (Val (Var 1)) (Val (Var 2)) :::
          If (Var 1 := DyOp (-) (Val (Var 1)) (Const 1)) Skip (
	    Goto 42
	  )
       ) :::
       Label 42 :::
       Val (Var 2)
      )

wh4 = Scope 10 (
       Var 1 := Const 5 :::
       Var 2 := Const 1 :::
       If (Const 0) (
         While (Const 1) (
            If (Var 1 := DyOp (-) (Val (Var 1)) (Const 1)) Skip (
	      Goto 42
	    ) :::
	    Label 5 :::
            Var 2 := DyOp (*) (Val (Var 1)) (Val (Var 2))
         )
       ) (Goto 5) :::
       Label 42 :::
       Val (Var 2)
      )

wh5 = Scope 10 (
       Var 1 := Const 5 :::
       Var 2 := Const 1 :::
       Goto 5 :::
       While (Const 1) (
          If (Var 1 := DyOp (-) (Val (Var 1)) (Const 1)) Skip (
	    Goto 42
	  ) :::
	  Label 5 :::
          Var 2 := DyOp (*) (Val (Var 1)) (Val (Var 2))
       ) :::
       Label 42 :::
       Val (Var 2)
      )

wh6 = If (Const 0) (
         While (Const 0) (Label 1)
      ) (
         While (Const 0) (Label 2) :::
	 Goto 1
      ) :::
      Const 120

pr1 = Scope 10 (
       Var 1 := Const 5 :::
       Var 2 := DyOp (+) (Val (Var 1)) (Const 1)
      )

pr2 = Scope 10 (
        Var 1 := Const 5 :::
        Goto 42 :::
	Var 1 := Const 3 :::
	Label 42 :::
        Var 2 := DyOp (+) (Val (Var 1)) (Const 1)
      )

pr3 = Scope 10 (
        Var 1 := Const 5 :::
        Goto 42 :::
	Var 1 := Const 3 :::
        Var 2 := DyOp (+) (Label 42 ::: Val (Var 1)) (Const 1)
      )

-- note the difference between eval and eval'
pr4 = Scope 10 (
        Var 1 := Const 5 :::
        Goto 42 :::
	Var 1 := Const 3 :::
        Var 2 := DyOp (+) (Val (Var 1)) (Label 42 ::: Const 1)
      )

sc1 = Goto 1 :::
      Scope 1 (
         Label 1 :::
	 Var 0 := Const 5 :::
	 Val (Var 0)
      )

sc2 = Scope 1 (
	 Var 0 := Const 5 :::
	 Goto 1
      ) :::
      Label 1 :::
      Val (Var 0)

sc3 = Scope 1 $
      Var 0 := Const 0 :::
      Label 1 :::
      Var 0 := DyOp(+) (Val (Var 0)) (Const 1) :::
      Goto 2 :::
      Scope 100 (
	Label 2 :::
        Var 0 := DyOp(+) (Val (Var 0)) (Const 10) :::
        Goto 3
      ) :::
      Label 3 :::
      Var 0 := DyOp(+) (Val (Var 0)) (Const 100) :::
      If (DyOp(-) (Val (Var 0)) (Const 222)) (
	  Goto 1
      ) Skip :::
      Val (Var 0)

inf1 = Label 5 ::: Scope 100 (
       Goto 5
      )

inf2 = Label 5 :::
       Scope 100 (
         Skip
      ) :::
       Goto 5

weird = Scope 10 $
        Var 0 := Const 5 :::
        Var 1 := (DyOp (+) (Val (Var 0)) (Label 1 ::: Const 1)) :::
	If (Val (Var 0)) (
           Var 0 := Const 0 :::
	   Goto 1
	) Skip :::
	Val (Var 1)

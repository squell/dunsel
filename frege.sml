(* "Kernel" starts here.
 * The single point of failure is in the abstract type thm
 *
 * NOTE: the simple logic has been extended with "bottom" and
 * a rule for
 *********************************************************
 *)

exception NonSequitur

datatype prop = Var of string | Arrow of prop * prop

val bot: prop = Var "_"
fun neg (p: prop) = Arrow (p, bot)

abstype thm = T of prop list * prop
with
    fun assume (p: prop): thm = T([p], p)

    fun intro_rule (p: prop) (T(hyps, conc): thm): thm =
      let
        val nhyps = List.filter (fn q => p<>q) hyps
      in
        T(nhyps, Arrow (p, conc))
      end

    fun elim_rule (T(G,pq): thm) (T(H,p): thm): thm =
      case pq of
        Arrow (p2,q) => if p=p2 then T(G@H, q) else raise NonSequitur
      | _ => raise NonSequitur

    fun dest_thm (T th: thm): prop list * prop = th

    fun exfalso (p: prop) (T(G,b): thm) =
      if b=Var "_" then T(G,p)
      else raise NonSequitur

    fun absurd_rule (T(G,nnp): thm) =
      case nnp of Arrow(Arrow(p, Var"_"), Var"_") => T(G,p)
                | _ => raise NonSequitur
end

(* Useful functions
 *****************************************************)

fun hyp t = #1(dest_thm t)
fun concl t = #2(dest_thm t)

fun isArrow (Arrow _) = true
  | isArrow _ = false

fun isVar (Var _) = true
  | isVar _ = false

fun ante  (Arrow(p,_))=p
  | ante x = x
fun consq (Arrow(_,q))=q
  | consq x = x

(* Pretty printer
 *******************************************)

fun prprop (Var s) = s
  | prprop (Arrow (Var s, b)) = concat [s, "->", prprop b]
  | prprop (Arrow (a, b)) = concat ["(", prprop a, ")", " -> ", prprop b]

(* mosml doesnt have String.concatWith... *)
fun concatWith s [] = ""
  | concatWith s (x::xs) = concat (x::List.map(fn t => s^t) xs)

fun prthm (T: thm) =
  let
    val (hyp, conc) = dest_thm T
    val hypStr = concatWith ", " (map prprop hyp)
  in
    concat [hypStr, if hypStr<>"" then " " else "", "|- ", prprop conc]
  end

(* "Tactics" system
 ******************************************
 *)

exception Mistake

type justifier = thm list -> thm

type goal = prop list * prop;
type goalstate = thm

type tactic = goal -> goal list * justifier

fun by (tac: tactic) (jtm: goalstate): goalstate =
  if null (hyp jtm) then raise Mistake else
  let
    val (gs',jf') = tac (hyp jtm)
    val n = length gs'
	val jtm' = jf' (map (assume o #2) gs')
  in
    print "Subthm: "; print (prthm jtm'); print "\n";
    print "Oldthm: "; print (prthm jtm ); print "\n";
    print "Newthm: "; print (prthm (elim_rule (intro_rule (concl jtm') jtm) jtm')); print "\n";
    (elim_rule (intro_rule (concl jtm') jtm) jtm')
  end

val assumption: tactic = fn (hs,p) =>
  case List.find(fn h => h = p) hs of
    SOME h => ([], fn [] => assume h)
  | NONE   => raise Mistake

val intro_tac: tactic = fn (hs, p) =>
  if isVar p then raise Mistake
  else ([(ante p::hs,consq p)], fn [tq] => intro_rule (ante p) tq)

fun elim_tac (p: prop): tactic = fn (hs, q) =>
  ([(hs,p), (hs, Arrow(p,q))], fn [tp,tpq] => elim_rule tpq tp)

val quodlibet: tactic = fn (hs, p) =>
  ([(hs,bot)], fn [tbot] => exfalso p tbot)

val absurd_tac: tactic = fn (hs, p) =>
  ([(neg p::hs,bot)], fn [tbot] => absurd_rule (intro_rule (neg p) tbot))

(* Interacting with the goal solver
 ***********************************)

fun prgoal (hs,g) =
  let
    val hypStr = concatWith "\n" (map prprop hs)
    val gStr = concat ["\n---\n", prprop g, "\n"]
  in
    hypStr ^ gStr
  end

val history : goalstate list ref = ref []

fun mkgoal p = (assume p)

fun g (p: prop) = history := [mkgoal p]

fun b () = history := tl (!history)

fun p () =
  let
    val (x,y) = dest_thm (hd (!history))
  in
    if null x then print "Proof completed, type qed().\n" else
    print (concatWith "\n[===========]\n\n" (rev (map prgoal x)));
    print "\nJustifier: ";
	print (prthm ((#2 o hd) (!history)));
    print "\n"
  end

fun e (tac: tactic) =
  let
    val x = hd (!history)
  in
    history := by tac x :: !history
  end

fun qed () =
  let val (tm) = hd (!history) in
    tm
  end handle _ => raise Mistake

(* Parser. Since it is not hard to do.
 **********************************************
 *)

exception Parser

fun ` formula =
  let
    fun lex (#"("::cs) [] = "(" ::lex cs []
      | lex (#"-"::cs) [] = "->"::lex (tl cs) []
      | lex (#")"::cs) [] = ")" ::lex cs []
      | lex (#"_"::cs) [] = "_" ::lex cs []
      | lex (#" "::cs) [] = lex cs []
      | lex (c::cs)   acc = if Char.isAlpha c then lex cs (c::acc)
                            else if null acc then raise Parser
                            else implode (rev acc)::lex (c::cs) []
      | lex []        acc = if null acc then [] else [implode (rev acc)]

     val tokens = ref (lex (explode formula) [])

     fun tok () = if null (!tokens) then NONE else let
       val t::ts = !tokens
     in
       tokens := ts; SOME t
     end

     fun get_formula () = let
       val lhs = case tok() of SOME "(" => get_formula()
                             | SOME x   => Var x
                             | _        => raise Parser
     in
       case tok() of NONE      => lhs
                   | SOME ")"  => lhs
                   | SOME "->" => Arrow(lhs, get_formula())
                   | _         => raise Parser
     end

  in
    get_formula ()
  end

(*****Example*****)

val X = Var "X"
val Y = Var "Y"
val Z = Var "Z"

val S =
  let
    val xpy = Arrow (X,Y)
    val ypz = Arrow (Y,Z)
    val tx = assume X
    val txpy = assume xpy
    val typz = assume ypz
    val ty = elim_rule txpy tx
    val tz = elim_rule typz ty
    val txpz = intro_rule X tz
  in
    intro_rule xpy (intro_rule ypz txpz)
  end

val K = intro_rule (Var "x") (intro_rule (Var "y") (assume (Var "x")))

(*******Example 2**********)

fun remdup (x::xs) = x::remdup (List.filter(fn y => y<>x) xs)
  | remdup [] = []

fun call (f::fs) x = call fs (f x)
  | call [] x = x

fun try f x = SOME (f x) handle _ => NONE

fun mktaut (T: thm) = call (map intro_rule (hyp T)) T

fun intros (T: thm) =
  case concl T of
    Arrow (p,q) => intros (elim_rule T (assume p))
  | _           => T

fun cleanup (T: thm): thm =
  let
    fun clean p tm = elim_rule (intro_rule p tm) (assume p)
    val env = map clean (remdup (hyp T))
  in
    call env T
  end

fun assumev s = assume (Var s)
fun assumevq s t = elim_rule (intro_rule (Var s) t) (assumev s)

val demo = intros (mktaut (assumevq "x" (mktaut (assumev "x"))))

fun simplify (T: thm): thm = (cleanup o intros) T

(*******Example 3: Pearce**********)
(*
val pearce: thm =
   (
   g (`"((P->Q)->P)->P");
   e intro_tac;
   e absurd_tac;
   e (elim_tac (`"P"));
   e (elim_tac (`"P->Q"));
   e intro_tac;
   e quodlibet;
   e (elim_tac (`"P"));
   e assumption;
   e assumption;
   e assumption;
   e assumption;
   qed()
   )
*)

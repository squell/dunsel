(* "Kernel" starts here.
 * The single point of failure is in the abstract type thm
 *
 * NOTE: the simple logic has been extended with "bottom" and
 * a single rule for double negation elimination. A very easy
 * way to make this theorem prover more interesting.
 *********************************************************
 *)

datatype prop = Var of string | Arrow of prop * prop

exception NonSequitur of prop list

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
        Arrow (p2,q) => if p=p2 then T(G@H, q) else raise NonSequitur[pq, p, q]
      | _ => raise NonSequitur[pq, p]

    fun dest_thm (T th: thm): prop list * prop = th

    fun absurd_rule (T(G,nnp): thm): thm =
      case nnp of Arrow(Arrow(p, Var"_"), Var"_") => T(G,p)
                | _ => raise NonSequitur[nnp,neg (neg (Var"?"))]
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
  | ante  p=p
fun consq (Arrow(_,q))=q 
  | consq p=p

fun call fs x = foldl (fn(f,x)=>f x) x fs

(* A |- _ => A |- Q .. which is not primitive 
  but careful not to mess up the hypotheses! *) 
fun exfalso (q: prop) (tb: thm): thm = let
  val tq = absurd_rule (intro_rule (neg q) (assume bot))
in
  elim_rule (intro_rule bot tq) tb
end
  
(* A |- p & B |- q => A,B-{p} |- q, if p present in B *)
fun cut_rule (tp:thm) (ptq:thm): thm = let 
  val p = concl tp
in
  if List.exists(fn h=>h=p) (hyp ptq) then 
    elim_rule (intro_rule p ptq) tp
  else ptq 
end

(* Extension to full propositional logic; since this relies on
 * the kernel, you don't need to understand this, IF you believe that
 * the _thm schemas capture the essence of and&or (type "map prthm (paranoid())")
 ********************************************************)

fun conj p q = (Arrow(Arrow(p,Arrow(q,Var"_")),Var"_"))
fun disj p q = (Arrow(Arrow(p,Var"_"), q))

fun isConj (Arrow(Arrow(p,Arrow(q,Var"_")),Var"_")) = true
  | isConj _ = false
  
fun isDisj (Arrow(Arrow(p,Var"_"), q)) = true
  | isDisj _ = false
  
fun isImpl p = not (isVar p orelse isConj p orelse isDisj p orelse (consq p = bot))

fun left (Arrow(Arrow(p,Var"_"), q)) = p
  | left (Arrow(Arrow(p,Arrow(q,Var"_")),Var"_")) = p
  | left (Arrow(p,q)) = p

fun right (Arrow(Arrow(p,Var"_"), q)) = q
  | right (Arrow(Arrow(p,Arrow(q,Var"_")),Var"_")) = q
  | right (Arrow(p,q)) = q

fun or_in1_thm p q = intro_rule p (intro_rule (neg q) (assume p))
fun or_in2_thm p q = intro_rule p (intro_rule (neg p) (exfalso q (elim_rule (assume (neg p)) (assume p))))
fun and_in_thm p q = intro_rule p (intro_rule q (intro_rule (Arrow(p,Arrow(q,bot))) (elim_rule (elim_rule (assume (Arrow(p,Arrow(q,bot)))) (assume p)) (assume q))))

fun and_ex1_thm (Arrow (Arrow(p,Arrow(q,Var"_")),Var"_")) = 
  let
    val t  = assume (Arrow (Arrow(p,Arrow(q,Var"_")),Var"_"))
    val np = assume (neg p)
    val nq = exfalso (neg q) (elim_rule np (assume p))
    val tb = elim_rule t (intro_rule p nq)
    val nnp = intro_rule (neg p) tb
  in
    intro_rule (concl t) (absurd_rule nnp)
  end

fun and_ex2_thm (Arrow (Arrow(p,Arrow(q,Var"_")),Var"_")) =
  let
    val t   = assume (Arrow (Arrow(p,Arrow(q,Var"_")),Var"_"))
    val nq  = assume (neg q)
    val pnq = intro_rule p nq
    val tb  = elim_rule t pnq
    val nnq = intro_rule (neg q) tb
  in
    intro_rule (concl t) (absurd_rule nnq)
  end

fun or_ex_thm (Arrow(Arrow(p,Var"_"), q)) z =
  let
    val t = assume (Arrow(Arrow(p,Var"_"), q))
    val pz = assume (Arrow(p,z))
    val qz = assume (Arrow(q,z))
    val nz = assume (neg z)
    val tb = elim_rule nz (elim_rule pz (assume p))
    val tq = elim_rule t (intro_rule p tb)
    val nnz = intro_rule (neg z) (elim_rule nz (elim_rule qz tq))
  in
    intro_rule (concl t) (intro_rule (Arrow(p,z)) (intro_rule (Arrow(q,z)) (absurd_rule nnz)))
  end

fun paranoid () = let val (x,y)=(Var"x",Var"y") in  
                    [and_in_thm x y, and_ex1_thm (conj x y), and_ex2_thm (conj x y),
                     or_in1_thm x y, or_in2_thm x y, or_ex_thm (disj x y) (Var"z")]
                  end

fun or_in1_rule q t = elim_rule (or_in1_thm (concl t) q) t
fun or_in2_rule q t = elim_rule (or_in2_thm (concl t) q) t
fun and_in_rule t u = elim_rule (and_in_thm (concl t) (concl u)) t

fun and_ex1_rule t = elim_rule (and_ex1_thm (concl t)) t
fun and_ex2_rule t = elim_rule (and_ex2_thm (concl t)) t

fun or_ex_rule pq pz qz = 
  let
    val th = or_ex_thm (concl pq) (consq (concl pz))
  in
    elim_rule (elim_rule (elim_rule th pq) pz) qz
  end  
  
(* Pretty printer
 * three modes:
 * 0 - don't be clever
 * 1 - logical connectives <- gives some surprising results
 * 2 - logical connectives (perhaps less surprising & less fun)
 *******************************************)

val format = ref 1

fun pretty b = format := b

fun prprop p = let
  fun rawpr (Var s) = s
    | rawpr (Arrow(a, b)) =  concat ["(", pr a, " -> ", pr b, ")"]
  and extpr (Arrow(Arrow (a, Var"_"), b)) = concat ["(", pr a, "|", pr b, ")"]
    | extpr (Arrow(Arrow (a, Arrow(b,Var"_")), Var"_")) = concat ["(", pr a, "&", pr b, ")"]
    | extpr (Arrow(a, Var"_")) = concat ["~", pr a]
    | extpr p = rawpr p
  and newpr (Arrow(Arrow(Arrow (a, Arrow(b,Var"_")), Var"_"),c)) = concat ["((", pr a, "&", pr b, ") -> ", pr c, ")"]
    | newpr (Arrow (Arrow(a,Var"_"), Var"_")) = concat ["~~", pr a]
    | newpr p = extpr p
  and pr p = List.nth([rawpr,extpr,newpr],!format) p
  val s = pr p
in
  if String.sub(s,0) = #"(" then String.substring(s,1,String.size s-2) else s
end

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
type goalstate = goal list * thm

type tactic = goal -> goal list * justifier

fun by (tac: tactic) ((gs, jt): goalstate): goalstate =
  if null gs then raise Mistake else
  let
    (* [a,b],c => a->b->c, a, b |- c*)
	fun mp (hyps, g) = let
	  fun mp' [] tm = tm
	    | mp' (h::hs) tm = mp' hs (elim_rule tm (assume h))
	in
	  mp' hyps (assume (foldr Arrow g hyps))
	end
    val (gs',jf') = tac (hd gs)
	val jt'' = jf' (map mp gs')
	val jt' = foldr (fn(x,y)=>intro_rule x y) jt'' (#1 (hd gs))
  in
    (gs' @ tl gs, cut_rule jt' jt)
  end
  
val assumption: tactic = fn (hs,p) =>
  case List.find(fn h => h = p) hs of
    SOME h => ([], fn [] => assume h)
  | NONE   => case List.find(fn p=>List.exists(fn q=>q=neg p) hs) hs of
			         SOME h => ([], fn[]=>exfalso p (elim_rule (assume (neg h)) (assume h)))
				   | NONE => raise Mistake

val intro_tac: tactic = fn (hs, p) =>
  if isVar p then raise Mistake
  else ([(ante p::hs,consq p)], fn [tq] => intro_rule (ante p) tq)

fun elim_tac (p: prop): tactic = fn (hs, q) =>
  ([(hs,p), (hs, Arrow(p,q))], fn [tp,tpq] => elim_rule tpq tp)

val quodlibet: tactic = fn (hs, p) =>
  ([(hs,bot)], fn [tbot] => exfalso p tbot)

val absurd_tac: tactic = fn (hs, p) =>
  ([(neg p::hs,bot)], fn [tbot] => absurd_rule (intro_rule (neg p) tbot))

(* Extension of tactics to full logic
 ***********************************)

val and_in_tac: tactic = fn (hs, z) =>
  let
    val p = left z
    val q = right z
  in
    ([(hs,p),(hs,q)], fn [tp,tq] => and_in_rule tp tq)
  end

fun and_ex1_tac (q:prop): tactic = fn (hs, p) =>
  ([(hs,conj p q)], fn [tconj] => and_ex1_rule tconj)

fun and_ex2_tac (p:prop): tactic = fn (hs, q) =>
  ([(hs,conj p q)], fn [tconj] => and_ex2_rule tconj)

val or_in1_tac: tactic = fn (hs, pq) =>
  ([(hs, right pq)], fn [th] => or_in1_rule (left pq) th)

val or_in2_tac: tactic = fn (hs, pq) =>
  ([(hs, left pq)], fn [th] => or_in2_rule (right pq) th)

fun or_ex_tac (pq:prop): tactic = fn (hs, z) =>
  let 
    val p = left pq
    val q = right pq
  in
    ([(hs,pq),(p::hs,z),(q::hs,z)], 
     fn [tpq,tpz,tqz] => or_ex_rule tpq (intro_rule p tpz) (intro_rule q tqz))
  end

(* Using tactics non-interactively
 ***********************************)

fun mkgoal p = ([([], p)], assume p)

fun PROOF (p: prop, ts: tactic list): thm = 
  let val ([],qed) = call (map by ts) (mkgoal p) in qed end

infix 5 PROOF

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

val implicit_p = ref (fn()=>())

fun g (p: prop) = (history := [mkgoal p]; !implicit_p())

fun b () = (history := tl (!history); !implicit_p())

fun p () =
  let
    val (x,y) = hd (!history)
  in
    if null x then print "Proof completed, type qed().\n" else
    print (concatWith "\n[===========]\n\n" (rev (map prgoal x)))
  end

fun e (tac: tactic) =
  let
    val x = hd (!history)
  in
    history := by tac x :: !history;
    !implicit_p()
  end

fun qed () =
  let val ([], f) = hd (!history) in
    f
  end handle NonSequitur x => (print "Tactical Error:\n"; 
                               print (concatWith "\n" (map prprop x));
                               print "***\n";
                               raise NonSequitur x)
           | Bind => (print "Unfinished Proof\n"; raise Mistake)

(* A more chatty mode *)
fun v b = implicit_p := (if b then fn()=>p() else fn()=>())

(* Parser. Since it is not hard to do. All BINARY connectives have 
 * the same precedence and associate to the right.
 * If you rely on implicit associativity you are up to no good anyway.
 *********************************************************************
 *)

exception Parser

fun ` formula =
  let
    fun lex (#"("::cs) [] = "(" ::lex cs []
      | lex (#"-"::cs) [] = "->"::lex (tl cs) []
      | lex (#")"::cs) [] = ")" ::lex cs []
      | lex (#"_"::cs) [] = "_" ::lex cs []
      | lex (#"~"::cs) [] = "~" ::lex cs []
      | lex (#"|"::cs) [] = "|" ::lex cs []
      | lex (#"&"::cs) [] = "&" ::lex cs []
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
       fun get_primary () = case tok() of SOME "(" => get_formula()
                                        | SOME "~" => Arrow(get_primary(), bot)
                                        | SOME x   => Var x
                                        | _        => raise Parser
       val lhs = get_primary()
     in
       case tok() of NONE      => lhs
                   | SOME ")"  => lhs
                   | SOME "->" => Arrow(lhs, get_formula())
                   | SOME "|"  => disj lhs (get_formula())
                   | SOME "&"  => conj lhs (get_formula())
                   | _         => raise Parser
     end

  in
    get_formula ()
  end

val g' = g o `
fun e' tac = e o tac o `

val assume' = assume o `
val intro_rule' = intro_rule o `
val exfalso' = exfalso o `

val or_in1_rule' = or_in1_rule o `
val or_in2_rule' = or_in2_rule o `

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

(*******Example 2: remove duplicate hyps**********)

fun remdup (x::xs) = x::remdup (List.filter(fn y => y<>x) xs)
  | remdup [] = []

fun try f x = SOME (f x) handle _ => NONE

fun mktaut (T: thm) = call (map intro_rule (hyp T)) T

fun mksequent (T: thm) =
  case concl T of
    Arrow (p,q) => mksequent (elim_rule T (assume p))
  | _           => T

fun cleanup (T: thm): thm =
  let
    fun clean p tm = elim_rule (intro_rule p tm) (assume p)
    val env = map clean (remdup (hyp T))
  in
    call env T
  end

fun simplify (T: thm): thm = (cleanup o mksequent) T

(*******Example 3: some generalized tactics*****)

fun intros (hs, p) = let
  fun antes p  = if isVar p then [] else ante p::antes (consq p)
  fun consqs p = if isVar p then p  else consqs (consq p)
  val hs' = antes p
in
  ([(hs@hs',consqs p)], fn [th] => foldr (fn(f,x)=>f x) th (map intro_rule hs'))
end

fun claim q (hs, p) =
  ([(q::hs,  p), (hs, q)], fn [tp, tq] => elim_rule (intro_rule q tp) tq)

fun apply (hs, p) =
  case List.filter(fn h => isArrow h andalso consq h = p) hs of
    [h]   => ([(hs,ante h)], fn [th] => elim_rule (assume h) th)
  | [a,b] => ([(hs,disj (ante a) (ante b))], fn [th] => or_ex_rule th (assume a) (assume b))

fun genapply (hs, p:prop) = let
  fun unpack (Var x) = ([],Var x)
    | unpack (Arrow(p,q)) = let val(ps,q')=unpack q in (p::ps,q') end
  fun prefixes (x::xs) = ([],x) :: map (fn (a,b)=>(x::a,b)) (prefixes xs)
    | prefixes [] = []
in
  case List.filter(fn (_,q) => p=q) (map unpack hs) of
    [(ps,q)]   => let
	                val tpq = call (map (fn x=>fn y=>elim_rule y (assume x)) ps) (assume (foldr Arrow q ps))
   	              in 
				    (map (fn (hs',q)=>(hs@hs',q)) (prefixes ps), foldr(fn(x,y)=>cut_rule x y) tpq)
				  end
end  
  
(* Should find all proofs that can be found by 50'000 monkeys. *)
val simple = let
  fun subsumes (hs, p) (hs', p') = p=p' andalso remdup (hs'@hs) = hs'
  val memo : goal list ref = ref []
  
  fun provided cond f = if cond then f else raise Mistake
  fun attempt tac g cont = let 
	val ([g'], jf) = tac g 
    val ([], jf') = cont g'	 
  in
	([], jf o (fn x=>[jf' x]))
  end
  
  fun iauto g = case g of (hs, p) => let
    val () = if List.exists (subsumes g) (!memo) then raise Mistake else memo := g::(!memo)
  in
    (*print "===[History]===\n"; app (fn s=>(print (prgoal s); print "+++\n")) (!memo);*)
    assumption g handle _ =>
	provided (isArrow p) attempt intros g iauto handle _ =>
	attempt apply g iauto handle _ => 
	provided (bot <> p) attempt quodlibet g iauto
  end
  fun cauto g = case g of (hs, p) =>
    iauto g handle _ => provided (p<>bot andalso List.all(fn h => h<>neg p) hs) attempt absurd_tac g cauto
in 
  fn g => (memo := []; cauto g) 
end 
   
(* a theorem to use intros and apply on *)

val demo_apply = `"(p->q)->(r->q)->p->q"

(*******Example 4: p\/~p is a triviality in this system.**)

fun tnd (p: prop): thm = intro_rule (neg p) (assume (neg p))

(*******Example 5: Peirce (simple logic only)**********)

val peirce: thm = `"((P->Q)->P)->P" PROOF [intro_tac, absurd_tac, elim_tac (`"P"), elim_tac (`"P->Q"),
                                           intro_tac, quodlibet, elim_tac (`"P"),
                                           assumption, assumption, assumption, assumption]

val peirce2: thm = `"((P->Q)->P)->P" PROOF [intros,absurd_tac,apply,apply,intros,quodlibet,apply,assumption]

(********Example 6: p->q \/ q->p******)

val pq_or_qp: thm = `"(p->q)|(q->p)"
                    PROOF
                    [ absurd_tac, elim_tac (`"p->q"), intro_tac, quodlibet, elim_tac (`"(p->q)|(q->p)"),
                      or_in1_tac, intro_tac, assumption, assumption, intro_tac, elim_tac (`"(p->q)|(q->p)"),
                      or_in2_tac, assumption, assumption ]

(* again this proof is much easier if you stop fighting the internal logic *)

val pq_or_qp2: thm = `"(p->q)|(q->p)" PROOF [intros, quodlibet, apply, intros, assumption]

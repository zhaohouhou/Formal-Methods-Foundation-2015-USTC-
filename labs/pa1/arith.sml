structure Arith: ARITH =
struct

datatype t 
  = True
  | False
  | If of t * t * t
  | Zero
  | Succ of t
  | Pred of t
  | IsZero of t

exception AddYourCodeHere
          
fun isNumber t =
    case t
     of Zero => true
      | Succ t' => isNumber t'
      | _ => false
             
exception NoRule
          
(* one-step evaluator *)
fun eval t =
    case t
     of If (True, t2, _) => t2
      | If (False, _, t3) => t3
      | If (t1, t2, t3) =>
        If (eval t1, t2, t3)
      | Succ t' => Succ (eval t')
      | Pred Zero => Zero
      | Pred (Succ t') =>
        if isNumber t'
        then t'
        else Pred (eval (Succ t'))
      | Pred t' => Pred (eval t')
      | IsZero Zero => True
      | IsZero (Succ t') =>
        if isNumber t'
        then False
        else IsZero (eval (Succ t'))
      | IsZero t' => IsZero (eval t')
      | _ => raise NoRule

fun pp t =
    case t
     of True => print "True"
      | False => print "False"
      | If (t1, t2, t3) =>
        (print "If("
       ; pp t1; print ", "; pp t2; print ", "
       ; pp t3; print ")")
      | Zero => print "Zero"
      | Succ t' =>
        (print "Succ("; pp t'; print ")")
      | Pred t' =>
        (print "Pred("; pp t'; print ")")
      | IsZero t' => 
        (print "IsZero("; pp t'; print ")")

fun evalAll t =
    (let val t' = (eval t)
         val _ = pp t'
         val _ = print "\n"
     in evalAll t'
     end) handle NoRule => t 

(* Your job: *)
fun evalBig t = raise AddYourCodeHere


end (* structure Arith *)

(* a unit test *)
val e = Arith.Pred (Arith.Succ 
                        (Arith.Pred Arith.Zero))

val _ = (Arith.pp e; print "\n")

val _ = Arith.evalAll e
        
val _ = Arith.evalBig e

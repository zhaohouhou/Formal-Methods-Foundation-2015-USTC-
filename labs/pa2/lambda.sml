structure Lambda: LAMBDA =
struct

datatype t 
  = Var of string
  | Abs of string * t
  | App of t * t
           
exception AddYourCodeHere
          
(* whether or not a term is a value. 
 * According to the definition, there is only
 * a unique value, the lambda abstraction.
 *)
fun isValue t =
    case t
     of Abs _ => true
      | _ => false
             
exception NoRule

(* generate fresh variables when called *)
val counter = ref 0
fun fresh () =
    let val n = !counter
        val _ = counter := n + 1
    in concat ["x_", Int.toString n]
    end

(* alpha converting an expression *)
(* [x|->f] e *)
fun alpha (x, f, e) =
    case e
     of Var y => 
        if x = y
        then Var f
        else e
      | Abs (z, e') =>
        if x = z
        then e
        else Abs (z, alpha (x, f, e'))
      | App (e1, e2) =>
        App (alpha (x, f, e1), alpha (x, f, e2))
        
(* [x |-> e1] e2 *)          
fun subst (x, e1, e2) =
    case e2
     of Var y =>
        if x = y
        then e1
        else e2
      | Abs (y, e3) =>
        if x = y	(*bound, remain the same*)
        then e2
        else let val f = fresh ()
             in  Abs (f, subst (x, e1, alpha (y, f, e3)))
             end      | App (e3, e4) =>
        App (subst (x, e1, e3), subst (x, e1, e4))

(* one-step evaluator *)
fun eval t =
    case t
     of Var _ => raise NoRule
      | Abs _ => raise NoRule
      | App (Abs (x, e1), e2) =>
        if isValue e2
        then subst (x, e2, e1) (* [x|->e2] e1 *)
        else App (Abs (x, e1), eval e2)
      | App (e1, e2) =>
        App (eval e1, e2)

fun pp t =
    case t
     of Var x => print x
      | Abs (x, e) => 
        (print "\\lambda "; print x; print ".("
       ; pp e; print ")")
      | App (e1, e2) =>
        (print "("; pp e1; print ") "; print "("
       ; pp e2; print ")")

fun evalAll t =
    (let val t' = (eval t)
         val _ = pp t'
         val _ = print "\n"
     in evalAll t'
     end) handle NoRule => t 

end (* structure Lambda *)

(* a unit test *)
val omega = Lambda.Abs ("x",
                        (Lambda.App (Lambda.Var "x", 
                                     Lambda.Var "x")))

val Omega = Lambda.App (omega, omega)

val _ = (Lambda.pp Omega; print "\n")

(*val _ = Lambda.evalAll Omega
  *)
      

structure Closure: CLOSURE =
struct

datatype t
  = Num of int
  | Var of string
  | Abs of string * t
  | App of t * t
  (* the implicit invariant here is that
   * the secnd "t" should always be a lambda.
   *)
  | Closure of env * t
     and env
       = T of string -> t
    
exception NoRule
exception BadExp

fun isValue t = 
    case t
     of Num _ => true
      | Closure _ => true
      | _ => false

val emptyEnv = T (fn _ => raise BadExp)

fun eval (T map, t): env * t = 
    case t
     of Num _ => raise NoRule
      | Var x => (T map, map x)
      | Abs _ =>
        (* maintain the invariant *)
        (emptyEnv, Closure (T map, t))
      | App (Closure (T map', 
                      t1),
             t2) =>
        if isValue t2
        then (case t1
               of Abs (x, t') =>
                  (T (fn z =>
                      if z=x
                      then t2
                      else map' z),
                   t')
                | _ => raise BadExp)
        else let val (env', t2') 
                   = eval (T map, t2) 
             in  (env',
                  App (Closure (T map', t1),
                  t2'))
             end
      | App (t1, t2) =>
        let val (env', t1') = eval (T map, t1)
        in  (env', App (t1', t2))
        end
      | Closure _ => raise NoRule

fun pp t =
    case t
     of Num n => print (Int.toString n)
      | Var x => print x
      | Abs (x, t) => 
        (print "\\lambda "
       ; print x
       ; print ".("
       ; pp t
       ; print ")")
      | App (t1, t2) =>
        (print "("
       ; pp t1
       ; print ") ("
       ; pp t2
       ; print ")")
      | Closure (env, t) =>
        (print "Closure(C, "
       ; pp t
       ; print ")")

fun evalAll (env, t): env * t = 
    (let val (env', t') = eval (env, t)
         val _ = pp t'
         val _ = print "\n"
     in evalAll (env', t')
     end) handle NoRule => (env, t)

val e = App (App (Abs ("x", 
                       Abs ("y",
                            Var "x")), 
                  Num 3), 
             Num 4)

val _ = pp e

val _ = evalAll (emptyEnv, e)


end


structure HigherOrder: HIGHER_ORDER =
struct

datatype t
  = Num of int
  | Abs of t -> t
  | App of t * t
       

exception NoRule

fun isValue t = 
    case t
     of App _ => false
      | _ => true

fun eval t =
    case t
     of Num _ => raise NoRule
      | Abs _ => raise NoRule
      | App (Abs f, t2) =>
        if isValue t2
        then f t2
        else App (Abs f, eval t2)
      | App (t1, t2) =>
        App (eval t1, t2)

fun pp t =
    case t
     of Num i => print (Int.toString i)
      | Abs f => print ("Abs")
      | App (t1, t2) =>
        (print "("
       ; pp t1
       ; print ") ("
       ; pp t2
       ; print ")")

fun evalAll t = 
    (let val t' = eval t
         val _ = pp t'
         val _ = print "\n"
     in evalAll t'
     end) handle NoRule => t




(* unit test *)
val e = App (App (Abs (fn x =>
                          Abs (fn y =>
                                  x)), 
                  Num 3), 
             Num 4)
        
val _ = pp e
        
val _ = evalAll e

end

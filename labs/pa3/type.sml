structure Type =
struct

datatype ty
  = Bool
  | Fun of ty * ty
           
datatype t
  = True
  | False
  | If of t * t * t
  | Var of string
  | Abs of string * ty * t
  | App of t * t
       

fun tyEquals (ty1, ty2) =
    case (ty1, ty2)
     of (Bool, Bool) => true
      | (Fun (s1, s2), Fun (s3, s4)) =>
        tyEquals (s1, s3) andalso
        tyEquals (s2, s4)
      | _ => false
        
exception TypeError

fun check (env, t): ty =
    case t
     of True => Bool
      | False => Bool
      | If (t1, t2, t3) =>
        (case check (env, t1)
         of Bool =>
            let val ty2 = check (env, t2)
                val ty3 = check (env, t3)
            in  if tyEquals (ty2, ty3)
                then ty2
                else raise TypeError
            end 
          | _ => raise TypeError)
      | Var x => env x
      | Abs (x, ty, t) =>
        Fun(ty, check (fn y => if x=y
                               then ty
                               else env y
                     , t))
      | App (t1, t2) =>
        let val ty1 = check (env, t1)
            val ty2 = check (env, t2)
        in  case ty1
             of Fun (ty1', ty2') =>
                if tyEquals (ty1', ty2)
                then ty2'
                else raise TypeError
              | _ => raise TypeError
        end

fun typeCheck t = check (fn x => raise TypeError, t)


(* unit test *)
val e = App (Abs ("x", Fun (Bool, Bool), Var "x")
           , (Abs ("x", Bool, Var "x")))
   
val e1 = App (Abs ("x", Bool, Var "x"), True)

val _ = typeCheck e

val _ = typeCheck e1

end


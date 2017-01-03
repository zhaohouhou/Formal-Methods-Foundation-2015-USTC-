structure Reference: REFERENCE =
struct

structure Type =
struct
datatype t
  = Bool
  | Unit
  | Fun of t * t
  | Ref of t

fun equals (t1, t2) =
    case (t1, t2)
     of (Bool, Bool) => true
      | (Fun (s1, s2), Fun (s3, s4)) =>
        equals (s1, s3) andalso
        equals (s2, s4)
      | (Ref s1, Ref s2) =>
        equals (s1, s2)
      | (Unit, Unit) => true
      | _ => false

fun toString t =
    case t
     of Bool => "bool"
      | Unit => "unit"
      | Fun (t1, t2) =>
        String.concat [toString t1
                     , " -> "
                     , toString t2]
      | Ref t =>
        String.concat [toString t
                     , " ref"]

end (* structure Type *)

(* we design a language with reference *)
datatype t
  = True
  | False
  | If of t * t * t
  | Var of string
  | Abs of string * Type.t * t
  | App of t * t
  | Ref of t         (* ref e *)
  | Deref of t       (* !e *)
  | Assign of t * t  (* t1 := t2 *)
  | Address of string (* l *)
  | Unit
        
exception TypeError

fun check (env, t): Type.t =
    case t
     of True => Type.Bool
      | False => Type.Bool
      | If (t1, t2, t3) =>
        (case check (env, t1)
         of Type.Bool =>
            let val ty2 = check (env, t2)
                val ty3 = check (env, t3)
            in  if Type.equals (ty2, ty3)
                then ty2
                else raise TypeError
            end 
          | _ => raise TypeError)
      | Var x => env x
      | Abs (x, ty, t) =>
        let val ty' = 
                check (fn y => if x=y
                               then ty
                               else env y
                     , t)
        in  Type.Fun (ty, ty')
        end
      | App (t1, t2) =>
        let val ty1 = check (env, t1)
            val ty2 = check (env, t2)
        in  case ty1
             of Type.Fun (ty1', ty2') =>
                if Type.equals (ty1', ty2)
                then ty2'
                else raise TypeError
              | _ => raise TypeError
        end
      | Ref t' => 
        let val ty = check (env, t')
        in  Type.Ref ty
        end
      | Deref t' =>
        let val ty = check (env, t')
        in  case ty 
             of Type.Ref ty => ty
              | _ => raise TypeError
        end
      | Assign (t1, t2) =>
        let val left = check (env, t1)
            val right = check (env, t2)
        in  case left
             of Type.Ref ty => 
                if Type.equals (ty, right)
                then Type.Unit
                else raise TypeError
              | _ => raise TypeError
        end
      | Unit => Type.Unit 
      | _ => raise TypeError


fun typeCheck t = check (fn x => raise TypeError, t)

(* to simplify the interface of the eval
 * function, we can make the heap global, 
 * instead of an argument to this function.
 *)
exception BadAddress

structure Heap =
struct

val counter = ref 0
fun fresh () =
    let val n = !counter
        val _ = counter := !counter + 1
    in  String.concat ["x_", Int.toString n]
    end
    
type heap = (string -> t) ref

val heap: heap = ref (fn _ => raise BadAddress)

fun alloc (t) = 
    let val newAddress = fresh ()
        val _ = heap := (fn y =>
                           if y = newAddress
                           then t
                           else (!heap) y)
    in  newAddress
    end

fun lookup (x) = (!heap) x

fun update (x, t) = 
    let val _ = heap := (fn y =>
                            if y=x
                            then t
                            else (!heap) y)
    in ()
    end
    

end (* structure Heap *)

fun isValue t =
    case t
     of True => true
      | False => true
      | Abs _ => true
      | Address _ => true
      | Unit => true
      | _ => false

(* I only show here the cases for the reference-related constructs
*)
exception NoRule
exception Todo

fun eval t =
    case t
     of Ref t' =>
        if isValue t'
        then Address (Heap.alloc t')
        else Ref (eval t')
      | Deref t' =>
        (case t'
          of Address x => Heap.lookup x
           | _ => Deref (eval t'))
      | Assign (t1, t2) =>
        (case t1
          of Address x =>
             if isValue t2
             then (Heap.update (x, t2); Unit)
             else Assign (t1, eval t2)
           | _ => Assign (eval t1, t2))
      | Address x => raise NoRule
      | Unit => raise NoRule
      | _ => raise Todo

fun pp t =
    case t
     of Ref t => (print "ref "; pp t)
      | Deref t => (print "!"; pp t)
      | Assign (t1, t2) =>
        (pp t1; print " := "; pp t2)
      | Address x => print x
      | Unit => print "()"
      | _ => raise Todo

fun evalAll t =
    (let val t' = (eval t)
         val _ = pp t'
         val _ = print "\n"
     in evalAll t'
     end) handle NoRule => t 

(* unit test *)

val t1 = Ref Unit
val ty1 = typeCheck (t1)
val _ = print (Type.toString ty1)

val t2 = Abs ("x", Type.Ref Type.Unit, Deref (Var "x"))

val ty2 = typeCheck t2
val _ = print (Type.toString ty2)

val t3 = App (t2, t1)
val ty3 = typeCheck t3
val _ = print (Type.toString ty3)

val _ = print "t4\n\n\n"
val t4 = Assign(Ref Unit, Deref (Ref Unit))
val _ = evalAll t4

end



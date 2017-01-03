open SMLofNJ

val queue: unit Cont.cont list ref = ref []
fun en k = queue := (!queue) @ [k]
fun out () =
    case !queue 
     of [] => raise Fail "failed\n"
      | x::xs =>
        let val _ = queue := xs
        in  x
        end

fun fork f =
    Cont.callcc (fn k =>(en k; f(0)))

fun yield () =
    Cont.callcc (fn k => (en k
                        ; Cont.throw (out ()) ()))

fun exit () =
    case !queue
     of [] => ()
      | x::xs =>
        (queue := xs
       ; Cont.throw x ())

fun f str n =
    let val _ = print (concat [str, " "
                             , Int.toString n
                             , "\n"])
        val _ = yield ()
    in  f str (n+1)
    end

val _ = fork (f ("f1-->"))
val _ = fork (f ("f2-->"))
val _ = exit ()

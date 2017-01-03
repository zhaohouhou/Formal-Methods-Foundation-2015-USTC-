signature HIGHER_ORDER =
sig

    datatype t
      = Num of int
      | Abs of t -> t
      | App of t * t

    val eval: t -> t
    val evalAll: t -> t
    val pp: t -> unit

end

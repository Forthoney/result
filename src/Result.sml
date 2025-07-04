signature RESULT =
sig
  exception Result
  datatype ('a, 'e) result = Ok of 'a | Err of 'e

  val getResult: ('a, 'e) result * 'a -> 'a
  val isOk: ('a, 'e) result -> bool
  val valOf: ('a, 'e) result -> 'a
  val errOf: ('a, 'e) result -> 'e

  val map: ('a -> 'c) -> ('a, 'e) result -> ('c, 'e) result
  val mapErr: ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result
  val join: (('a, 'e) result, 'e) result -> ('a, 'e) result
  val app: ('a -> unit) -> ('a, 'e) result -> unit
  val appErr: ('e -> unit) -> ('a, 'e) result -> unit
  val bind: ('a, 'e) result * ('a -> ('b, 'e) result) -> ('b, 'e) result
  val bindErr: ('a, 'e) result * ('e -> ('a, 'f) result) -> ('a, 'f) result
  val fold: ('a -> 'b * 'e -> 'b) -> ('a, 'e) result -> 'b

  val fromOption: 'e -> 'a option -> ('a, 'e) result
  val toOption: ('a, 'e) result -> 'a option
end

structure Result =
struct
  exception Result
  datatype ('a, 'b) result = Ok of 'a | Err of 'b

  fun getResult (Ok v, _) = v
    | getResult (_, default) = default

  fun isOk (Ok _) = true
    | isOk _ = false

  fun valOf (Ok v) = v
    | valOf (Err _) = raise Result

  fun errOf (Err e) = e
    | errOf (Ok _) = raise Result
    
  fun join (Ok v) = v
    | join (Err e) = Err e

  fun app f (Ok v) = f v
    | app _ _ = ()
  
  fun map f (Ok v) = Ok (f v)
    | map _ (Err e) = Err e

  fun mapErr f (Err e) = Err (f e)
    | mapErr _ (Ok v) = (Ok v)

  fun bind (Ok v, f) = f v
    | bind (Err e, _) = Err e

  fun bindErr (Err e, f) = f e
    | bindErr (Ok v, _) = Ok v

  fun fold (f, _) (Ok v) = f v
    | fold (_, g) (Err e) = g e

  fun fromOption _ (SOME v) = Ok v
    | fromOption err NONE = Err err

  fun toOption (Ok v) = SOME v
    | toOption (Err _) = NONE
end

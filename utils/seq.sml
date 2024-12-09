structure FinSeq =
struct

datatype 'a seq = Seq of 'a option * (unit -> 'a seq)

fun next (Seq (x, xf) : 'a seq) =
	xf ()

fun peek (Seq (x, _)) =
	x

fun cons (x : 'a option, f : unit -> 'a seq) : 'a seq =
	Seq (x, f)

fun foldl (f : 'a * 'b -> 'b) (acc : 'b) (Seq (NONE, _) : 'a seq) : 'b =
	acc
  | foldl f acc (Seq (SOME x, xf)) =
	foldl f (f (x, acc)) (xf ())

end

(*
structure F = FinSeq

val lst = List.tabulate (10, fn i => i)

fun lazy_list lst =
	case lst
	  of [] => F.cons (NONE, fn () => lazy_list [])
	   | x :: xs => F.cons (SOME x, fn () => lazy_list xs)

val a = lazy_list lst
*)

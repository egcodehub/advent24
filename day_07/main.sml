structure Main =
struct

structure L = LargeInt

fun read_input (lines : string list) : (L.int * L.int list) list = let
	val g = Option.valOf o L.fromString
	val f = fn s =>
		case String.tokens (fn c => c = #":") s
		  of res :: parts :: [] => (g res, List.map g (String.tokens (fn c => c = #" ") parts))
		   | _ => raise Fail "Invalid line format"
	in
    	List.map f lines
	end

fun valid_cal (result : L.int, numbers : L.int list) : bool = let
	fun aux (acc, []) =
		acc = result
	  | aux (acc, x :: xs) =
		(aux (x + acc, xs)) orelse (aux (x * acc, xs))
	in
		case numbers
		  of [] => raise Fail "Empty list"
		   | x :: xs => aux (x, xs)
	end

fun part_1 (cals : (L.int * L.int list) list) : L.int = let
	fun f (cal as (res, _), acc) =
		if valid_cal cal then
			acc + res
		else
			acc
	in
    	List.foldl f (Int.toLarge 0) cals
	end

fun main () = let
	val small_cals = read_input (Read.read_lines "small.txt")
	val large_cals = read_input (Read.read_lines "large.txt")

    val p1s = part_1 small_cals
	val _ = print ("Part 1 small expected 3749 and got: " ^ (L.toString p1s) ^ "\n")
    val p1l = part_1 large_cals
	val _ = print ("Part 1 large expected 2941973819040 and got: " ^ (L.toString p1l) ^ "\n")

(*
    val p2s = part_2 (small_grid_1, small_start)
	val _ = print ("Part 2 small expected 6 and got: " ^ (L.toString p2s) ^ "\n")
    val p2l = part_2 (large_grid_1, large_start)
	val _ = print ("Part 2 large expected 1976 and got: " ^ (L.toString p2l) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


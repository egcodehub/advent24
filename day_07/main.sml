structure Main =
struct

structure L = LargeInt

fun read_input (lines : string list) : (L.int * string list) list = let
	val g = Option.valOf o L.fromString
	val f = fn s =>
		case String.tokens (fn c => c = #":") s
		  of res :: parts :: [] => (g res, String.tokens (fn c => c = #" ") parts)
		   | _ => raise Fail "Invalid line format"
	in
    	List.map f lines
	end

fun valid_cal (result : L.int, numbers : string list) : bool = let
	val s_to_n = Option.valOf o L.fromString
	fun aux (acc, []) =
		acc = result
	  | aux (acc, x :: xs) = let
		val n = s_to_n x
		in
    		(aux (n + acc, xs)) orelse (aux (n * acc, xs))
		end
	in
		case numbers
		  of [] => raise Fail "Empty list"
		   | x :: xs => aux (s_to_n x, xs)
	end

fun part_1 (cals : (L.int * string list) list) : L.int = let
	fun f (cal as (res, _), acc) =
		if valid_cal cal then
			acc + res
		else
			acc
	in
    	List.foldl f (Int.toLarge 0) cals
	end

fun valid_cal_2 (result : L.int, numbers : string list) : bool = let
	val s_to_n = Option.valOf o L.fromString
	fun aux (acc, []) =
		acc = result
	  | aux (acc, x :: xs) = let
		val n = s_to_n x
		val t = (L.toString acc) ^ x
		val s = s_to_n t
		in
    		(aux (n + acc, xs)) orelse (aux (n * acc, xs)) orelse (aux (s, xs))
		end
	in
		case numbers
		  of [] => raise Fail "Empty list"
		   | x :: xs => aux (s_to_n x, xs)
	end

fun part_2 (cals : (L.int * string list) list) : L.int = let
	fun f (cal as (res, _), acc) =
		if valid_cal_2 cal then
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

    val p2s = part_2 small_cals
	val _ = print ("Part 2 small expected 11387 and got: " ^ (L.toString p2s) ^ "\n")
    val p2l = part_2 large_cals
	val _ = print ("Part 2 large expected 249943041417600 and got: " ^ (L.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


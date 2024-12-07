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

val s_to_n = Option.valOf o L.fromString

fun validate_1 (result, acc, lst) = let
	fun aux (acc, []) =
    	acc = result
      | aux (acc, x :: xs) = let
    	val n = s_to_n x
    	in
    		(aux (n + acc, xs)) orelse (aux (n * acc, xs))
    	end
	in
    	aux (acc, lst)
	end

fun validate_2 (result, acc, lst) = let
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
    	aux (acc, lst)
	end

fun valid_cal f (result : L.int, numbers : string list) : bool =
	case numbers
	  of [] => raise Fail "Empty list"
	   | x :: xs => f (result, s_to_n x, xs)

fun part_x f (cals : (L.int * string list) list) : L.int = let
	fun aux (cal as (res, _), acc) =
		if valid_cal f cal then
			acc + res
		else
			acc
	in
    	List.foldl aux (Int.toLarge 0) cals
	end

fun main () = let
	val small_cals = read_input (Read.read_lines "small.txt")
	val large_cals = read_input (Read.read_lines "large.txt")

    val p1s = part_x validate_1 small_cals
	val _ = print ("Part 1 small expected 3749 and got: " ^ (L.toString p1s) ^ "\n")
    val p1l = part_x validate_1 large_cals
	val _ = print ("Part 1 large expected 2941973819040 and got: " ^ (L.toString p1l) ^ "\n")

    val p2s = part_x validate_2 small_cals
	val _ = print ("Part 2 small expected 11387 and got: " ^ (L.toString p2s) ^ "\n")
    val p2l = part_x validate_2 large_cals
	val _ = print ("Part 2 large expected 249943041417600 and got: " ^ (L.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


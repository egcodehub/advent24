structure Main =
struct

structure L = LargeInt

fun read_input (lines : string list) : (string * string list) list = let
	val f = fn s =>
		case String.tokens (fn c => c = #":") s
		  of res :: parts :: [] => (res, String.tokens (fn c => c = #" ") parts)
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

fun valid_cal f (number : L.int, word : string, numbers : string list) : bool =
	case numbers
	  of [] => raise Fail "Empty list"
	   | x :: xs => f (result, s_to_n x, xs)

fun can_sub (acc, x) = let
	val s = acc - x
	in
    	if s < (L.toLarge 0) then NONE else SOME s
	end

fun can_div (acc, x) =
	if (d mod x) = (L.toLarge 0) then
		SOME (d div x)
	else
		NONE

fun can_con (acc, x) =
	if (String.size acc) < (String.size x) then
		NONE
	else if (String.extract (acc, (String.size acc) - (String.size x), NONE)) = x then
		SOME (String.extract (acc, 0, SOME ((String.size s1) - (String.size s2)))
	else
		NONE

fun valid_cal ops (number : L.int, word : string, numbers : string list) : bool = let
	fun aux (_, []) =
		raise Fail "Empty list"
	  | aux (acc, x :: []) =
		List.filter Option.isSome (List.map (fn f => f (acc, x)) ops)
	  | aux (acc, x :: xs) =
		case aux (acc, xs)
		  of [] => false
		   | ys => 
	in
		
	end

fun part_x f (cals : (string * string list) list) : L.int = let
	fun aux ((word, numbers), acc) = let
		val number = s_to_n word
		in
    		if valid_cal f (number, word, numbers) then
        		acc + number
    		else
    			acc
		end
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


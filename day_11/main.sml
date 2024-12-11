structure Main =
struct

structure L = LargeInt

fun read_stones (file_name : string) : L.int list =
	((List.map (Option.valOf o L.fromString)) o
	 (String.tokens (fn c => c = #" ")) o
	 Read.read_file)
	file_name

datatype 'a rule =
	Rejects
  | GivesOne of 'a
  | GivesTwo of 'a * 'a

fun rule_1 (i : L.int) : L.int rule =
	if i = (Int.toLarge 0) then
		GivesOne (Int.toLarge 1)
	else
		Rejects

fun count_digits (i : L.int) : int = let
	val ten = Int.toLarge 10
	fun aux (n, acc) =
		if n < ten then
			acc + 1
		else
			aux (n div ten, acc + 1)
	in
		aux (i, 0)
	end

fun rule_2 (i : L.int) : L.int rule = let
	val d = count_digits i
	in
    	if (d mod 2) = 0 then let
			val l = Int.toLarge (d div 2)
			val t = Real.toLargeInt IEEEReal.TO_ZERO (Math.pow (10.0, Real.fromLargeInt l))
    		val a = i div t
    		val b = i mod t
(*
			val _ = print ("Even digits: " ^ (L.toString i) ^ "\n")
			val _ = print ("Number of digits: " ^ (Int.toString d) ^ "\n")
			val _ = print ("Left part: " ^ (L.toString a) ^ "\n")
			val _ = print ("Right part: " ^ (L.toString b) ^ "\n")
			val _ = print "\n"
*)
    		in
        		GivesTwo (a, b)
    		end
		else
			Rejects
	end

fun rule_3 (i : L.int) : L.int rule =
	GivesOne (i * (Int.toLarge 2024))

fun first_non_reject ([], x) =
	NONE
  | first_non_reject (r :: rs, x) =
	case r x
	  of Rejects => first_non_reject (rs, x)
	   | res => SOME res

fun blink (rules, []) =
	[]
  | blink (rules : (L.int -> L.int rule) list, x :: xs : L.int list) : L.int list =
	case first_non_reject (rules, x)
	  of NONE => raise Fail "Didn't find rules for this stone"
	   | SOME res => (
		case res
		  of Rejects => raise Fail "Impossible case"
		   | GivesOne v => v :: (blink (rules, xs))
		   | GivesTwo (v1, v2) => v1 :: v2 :: (blink (rules, xs))
		)

fun blink_n (rules, stones, n) =
	if n <= 0 then
		stones
	else
		blink_n (rules, blink (rules, stones), n - 1)

fun part_1 (rules, stones : L.int list) : int = let
	val res = blink_n (rules, stones, 25)
(*
	val _ = print (MyList.to_string L.toString ("[", ", ", "]\n") res)
*)
	in
		List.length res
	end

fun main () = let
	val s_line = read_stones "small.txt"
	val l_line = read_stones "large.txt"
	val rules = [rule_1, rule_2, rule_3]

    val p1s = part_1 (rules, s_line)
	val _ = print ("Part 1 small expected 55312 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 (rules, l_line)
	val _ = print ("Part 1 large expected 220999 and got: " ^ (Int.toString p1l) ^ "\n")

(*
    val p2s = part_2 s_map
	val _ = print ("Part 2 small expected 81 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2s = part_2 l_map
	val _ = print ("Part 2 small expected 1651 and got: " ^ (Int.toString p2s) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


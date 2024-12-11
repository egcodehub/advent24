structure LargeDepthOrd : ORD_KEY =
struct

type t = LargeInt.int * int

fun cmp ((a, b), (c, d)) =
	case LargeInt.compare (a, c)
	  of General.EQUAL => Int.compare (b, d)
	   | res => res

fun eq (p1 : t, p2 : t) : bool =
	(cmp (p1, p2)) = General.EQUAL

end

structure Main =
struct

structure L = LargeInt

fun read_stones (file_name : string) : L.int list =
	((List.map (Option.valOf o L.fromString)) o
	 (String.tokens (fn c => c = #" ")) o
	 Read.read_file)
	file_name

fun rule_1 (i : L.int) : L.int list option =
	if i = (Int.toLarge 0) then
		SOME [Int.toLarge 1]
	else
		NONE

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

fun rule_2 (i : L.int) : L.int list option = let
	val d = count_digits i
	in
    	if (d mod 2) = 0 then let
			val l = Int.toLarge (d div 2)
			val t = Real.toLargeInt IEEEReal.TO_ZERO (Math.pow (10.0, Real.fromLargeInt l))
    		val a = i div t
    		val b = i mod t
    		in
        		SOME [a, b]
    		end
		else
			NONE
	end

fun rule_3 (i : L.int) : L.int list option =
	SOME [i * (Int.toLarge 2024)]

fun first_non_reject ([], x) =
	NONE
  | first_non_reject (r :: rs, x) =
	case r x
	  of NONE => first_non_reject (rs, x)
	   | res =>  res

structure D = Dictionary (LargeDepthOrd)

fun fast_blink (rules, stones, depth) : L.int = let
	val zero = Int.toLarge 0
	val one  = Int.toLarge 1
	val two  = Int.toLarge 2
	val memo : L.int D.dict ref = ref D.empty
    fun blink_stone (stone : L.int, d : int) =
    	case first_non_reject (rules, stone)
    	  of NONE => raise Fail "Number has no rules defined"
		   | SOME lst =>
			 if d = 1 then
    			 Int.toLarge (List.length lst)
			 else
    			 List.foldl (f_acc d) zero lst
	and f_acc d (v, acc) =
		case D.find ((v, d), !memo)
		  of NONE => let
			 val res = blink_stone (v, d - 1)
			 val _ = memo := D.insert (((v, d), res), !memo)
			 in
    			 res + acc
			 end
		   | SOME x => x + acc
	in
		List.foldl (fn (s, acc) => (blink_stone (s, depth)) + acc) zero stones
	end

fun part_x (stones : L.int list, n : int) : L.int = let
	val rules = [rule_1, rule_2, rule_3]
	in
		fast_blink (rules, stones, n)
	end

fun main () = let
	val s_line = read_stones "small.txt"
	val l_line = read_stones "large.txt"

    val p1s = part_x (s_line, 25)
	val _ = print ("Part 1 small expected 55312 and got: " ^ (L.toString p1s) ^ "\n")
    val p1l = part_x (l_line, 25)
	val _ = print ("Part 1 large expected 220999 and got: " ^ (L.toString p1l) ^ "\n")

    val p2l = part_x (l_line, 75)
	val _ = print ("Part 2 large expected 261936432123724 and got: " ^ (L.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


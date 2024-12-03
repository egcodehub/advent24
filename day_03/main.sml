structure Main =
struct

fun read_string (s, "") =
	SOME s
  | read_string ("", t) =
	NONE
  | read_string (s, t) = let
	val (sf, sr) = MyStr.first s
	val (tf, tr) = MyStr.first t
	in
    	if sf = tf then
    		read_string (sr, tr)
		else
    		NONE
	end

fun read_kind (f : char -> bool) ("" : string) : (string * string) option =
	NONE
  | read_kind f s = let
	fun aux ("", col) =
		SOME ("", col)
	  | aux (s, col) = let
		val (sf, sr) = MyStr.first s
		in
    		if f sf then
    			aux (sr, col ^ (str sf))
			else
    			SOME (s, col)
		end
	val (sf, sr) = MyStr.first s
	in
		if f sf then
			aux (sr, (str sf))
		else
			NONE
	end

fun read_mul "" =
	NONE
  | read_mul s =
	case read_string (s, "mul(")
	  of NONE => NONE
	   | SOME s0 => case read_kind Char.isDigit s0
	  of NONE => NONE
	   | SOME (s1, d1) => case read_string (s1, ",")
	  of NONE => NONE
	   | SOME s2 => case read_kind Char.isDigit s2
	  of NONE => NONE
	   | SOME (s3, d2) => case read_string (s3, ")")
	  of NONE => NONE
	   | SOME s4 => SOME ((d1, d2), s4)

fun get_numbers "" =
	[]
  | get_numbers s =
	case read_string (s, "mul(")
	  of NONE => get_numbers (String.extract (s, 1, NONE))
	   | SOME s0 => case read_kind Char.isDigit s0
	  of NONE => get_numbers (String.extract (s, 1, NONE))
	   | SOME (s1, d1) => case read_string (s1, ",")
	  of NONE => get_numbers (String.extract (s, 1, NONE))
	   | SOME s2 => case read_kind Char.isDigit s2
	  of NONE => get_numbers (String.extract (s, 1, NONE))
	   | SOME (s3, d2) => case read_string (s3, ")")
	  of NONE => get_numbers (String.extract (s, 1, NONE))
	   | SOME s4 => (d1, d2) :: (get_numbers s4)

fun part_1 s = let
	val nums = get_numbers s
	val f = fn ((x, y), acc) => let
		val a = Option.valOf (Int.fromString x)
		val b = Option.valOf (Int.fromString y)
    	in
        	(a * b) + acc
    	end
	in
    	List.foldl f 0 nums
	end

fun part_2 s = let
	fun enabled "" =
		[]
	  | enabled s = (
		case read_string (s, "do()")
		  of SOME s0 => enabled s0
		   | NONE => case read_string (s, "don't()")
		  of SOME s0 => disabled s0
		   | NONE => case read_mul s
		  of SOME (p, s0) => p :: (enabled s0)
		   | NONE => enabled (String.extract (s, 1, NONE))
		)
	and disabled "" =
		[]
	  | disabled s = (
		case read_string (s, "do()")
		  of SOME s0 => enabled s0
		   | NONE => case read_string (s, "don't()")
		  of SOME s0 => disabled s0
		   | NONE => case read_mul s
		  of SOME (_, s0) => disabled s0
		   | NONE => disabled (String.extract (s, 1, NONE))
		)
	val f = fn ((x, y), acc) => let
		val a = Option.valOf (Int.fromString x)
		val b = Option.valOf (Int.fromString y)
    	in
        	(a * b) + acc
    	end
	in
    	List.foldl f 0 (enabled s)
	end

fun main () = let
	val small_l = Read.read_file "small.txt"
	val large_l = Read.read_file "large.txt"
    val p1s = part_1 small_l
	val _ = print ("Part 1 small expected 161 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 large_l
	val _ = print ("Part 1 large expected 173517243 and got: " ^ (Int.toString p1l) ^ "\n")
    val p2s = part_2 small_l
	val _ = print ("Part 2 small expected 48 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 large_l
	val _ = print ("Part 2 large expected 100450138 and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


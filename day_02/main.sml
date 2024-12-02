structure Main =
struct

fun build_report s =
	List.map (Option.valOf o Int.fromString) (String.tokens Char.isSpace s)

fun check_report (prev, [], slope, max) =
	true
  | check_report (prev, x :: xs, slope, max) = let
	val dir = slope (prev, x)
	val dif = Int.abs (prev - x)
	in
    	if dir andalso dif <= max andalso dif > 0 then
        	check_report (x, xs, slope, max)
    	else
        	false
	end

fun is_safe_report (a :: b :: xs) =
	if a > b then
    	check_report (a, b :: xs, Int.>, 3)
	else
    	check_report (a, b :: xs, Int.<, 3)
  | is_safe_report _ =
	raise Fail "The report must have at least length 2"

fun part_1 reports =
	List.length (List.filter is_safe_report reports)

fun main () = let
	val small_l = List.map build_report (Read.read_lines "small.txt")
	val large_l = List.map build_report (Read.read_lines "large.txt")
    val p1s = part_1 small_l
	val _ = print ("Part 1 small expected 2 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 large_l
	val _ = print ("Part 1 large expected 224 and got: " ^ (Int.toString p1l) ^ "\n")
(*
    val p2s = part_2 small_l
	val _ = print ("Part 1 small expected 31 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 large_l
	val _ = print ("Part 1 large expected 21142653 and got: " ^ (Int.toString p2l) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


structure Main =
struct

fun build_report s =
	List.map (Option.valOf o Int.fromString) (String.tokens Char.isSpace s)

fun is_safe_report (lst, tol, d) = let
	fun valid_diff (x, y, d) = let
		val sub = Int.abs (x - y)
		in
			(sub <= d) andalso (sub > 0)
		end
	fun get_f (x, y) =
		if x < y then Int.< else Int.>
	fun step_1 ([], ps, e) =
		e <= tol
	  | step_1 (x :: xs, ps, e) =
		e <= tol andalso
		case (List.find (fn p => valid_diff (p, x, d)) (List.rev ps), ps)
		  of (NONE  , []) => step_1 (xs, [x], e)
		   | (NONE  , _ ) => step_1 (xs, x :: ps, e + 1)
		   | (SOME a, _ ) => step_2 (xs, x, e, get_f (a, x))
	and step_2 ([], p, e, f) =
		e <= tol
	  | step_2 (x :: xs, p, e, f) =
		e <= tol andalso
		if valid_diff (p, x, d) andalso f (p, x) then
    		step_2 (xs, x, e, f)
		else
    		step_2 (xs, p, e + 1, f)
	in
		step_1 (lst, [], 0)
	end

fun part_x (reports, tol) =
	List.length (List.filter (fn r => is_safe_report (r, tol, 3)) reports)

fun main () = let
	val small_l = List.map build_report (Read.read_lines "small.txt")
	val large_l = List.map build_report (Read.read_lines "large.txt")
    val p1s = part_x (small_l, 0)
	val _ = print ("Part 1 small expected 2 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_x (large_l, 0)
	val _ = print ("Part 1 large expected 224 and got: " ^ (Int.toString p1l) ^ "\n")
    val p2s = part_x (small_l, 1)
	val _ = print ("Part 2 small expected 4 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_x (large_l, 1)
	val _ = print ("Part 2 large expected (>287) and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


structure Main =
struct

structure A = Array2

fun create_grid (lines : string list) : char A.array =
	A.fromList (List.map String.explode lines)

fun find_guard (grid : char A.array) : int * int = let
	val (max_rows, max_cols) = A.dimensions grid
	fun aux (r, c) =
		if r >= max_rows then
			raise Fail "The input has no guard"
		else if c >= max_cols then
			aux (r + 1, 0)
		else if (A.sub (grid, r, c)) = #"^" then
			(r, c)
		else
			aux (r, c + 1)
	in
		aux (0, 0)
	end

fun turn_right dir =
	case dir
	  of ( 0,  1) => ( 1,  0)
	   | ( 1,  0) => ( 0, ~1)
	   | ( 0, ~1) => (~1,  0)
	   | (~1,  0) => ( 0,  1)
	   | _ => raise Fail "Invalid direction"

fun is_outside ((max_r, max_c), (r, c)) =
	(r < 0) orelse (c < 0) orelse (r >= max_r) orelse (c >= max_c)

fun is_obstacle (grid, (r, c)) =
	(A.sub (grid, r, c)) = #"#"

fun back_and_turn ((r, c), (dr, dc)) = let
	val (br, bc) = (r - dr, c - dc)
	val (tr, tc) = turn_right (dr, dc)
	val (nr, nc) = (br + tr, bc + tc)
	in
    	(nr, nc)
	end

fun step ((r, c), (dr, dc)) =
	(r + dr, c + dc)

(* DEBUG FUNCTION *)
fun print_p (x, y) =
	print ("(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")\n")

fun mark (grid, (r, c), acc) =
	case A.sub (grid, r, c)
	  of #"X" => acc
	   | _ => let
		 val _ = A.update (grid, r, c, #"X")
		 in
    		 acc + 1
		 end

fun part_1 (grid : char A.array) : int = let
	val start = find_guard grid
	val limit = A.dimensions grid
	fun walk (p, dir, acc) =
		if is_outside (limit, p) then
			acc
		else if is_obstacle (grid, p) then
			walk (back_and_turn (p, dir), turn_right dir, acc)
		else
			walk (step (p, dir), dir, mark (grid, p, acc))
	in
		walk (start, (~1, 0), 0)
	end

fun main () = let
	val small_grid = create_grid (Read.read_lines "small.txt")
	val large_grid = create_grid (Read.read_lines "large.txt")

    val p1s = part_1 small_grid
	val _ = print ("Part 1 small expected 41 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 large_grid
	val _ = print ("Part 1 large expected 5564 and got: " ^ (Int.toString p1l) ^ "\n")

(*
    val p2s = part_2 (ps_cmp, ps_lt, small_upd)
	val _ = print ("Part 2 small expected 123 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 (pl_cmp, pl_lt, large_upd)
	val _ = print ("Part 2 large expected 5331 and got: " ^ (Int.toString p2l) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


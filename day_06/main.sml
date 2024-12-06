structure PairsOrd : ORD_KEY =
struct

type t = (int * int) * (int * int)

fun cmp (((a1, b1), (c1, d1)), ((a2, b2), (c2, d2))) = let
	val a = Int.compare (a1, a2)
	val b = Int.compare (b1, b2)
	val c = Int.compare (c1, c2)
	val d = Int.compare (d1, d2)
	in
    	case a
		  of EQUAL => (
			 case b
			   of EQUAL => (
				  case c
					of EQUAL => d
					 | _ => c
				  )
				| _ => b
			 )
		   | _ => a
	end

fun eq (p1 : t, p2 : t) : bool =
	p1 = p2

end

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

structure S = Set (PairsOrd)

fun is_closed_loop (grid : char A.array, start : int * int, d : int * int) : bool = let
	val limit = A.dimensions grid
	fun aux (p, dir, seen) =
		if is_outside (limit, p) then
			false
		else if is_obstacle (grid, p) then let
			val switch = back_and_turn (p, dir)
			val n_dir  = turn_right dir
			in
				case S.find ((p, dir), seen)
				  of NONE => aux (switch, n_dir, S.insert ((p, dir), seen))
				   | SOME _ => true
			end
		else
			aux (step (p, dir), dir, seen)
	in
		aux (start, d, S.empty)
	end

fun part_2 (grid : char A.array, start : int * int) : int = let
	val limit = A.dimensions grid
    val _ = A.update (grid, (#1 start), (#2 start), #".")
	fun walk (p as (r, c), dir, acc) =
		if is_outside (limit, p) then
			acc
		else if is_obstacle (grid, p) then
			walk (back_and_turn (p, dir), turn_right dir, acc)
		else if (A.sub (grid, r, c)) = #"." then
			walk (step (p, dir), dir, acc)
		else let
			val _ = A.update (grid, r, c, #"#")
			val b = is_closed_loop (grid, start, (~1, 0))
			val n_acc = if b then acc + 1 else acc
			val _ = A.update (grid, r, c, #".")
			in
    			walk (step (p, dir), dir, n_acc)
    		end
	in
		walk (start, (~1, 0), 0)
	end

fun main () = let
	val small_grid_1 = create_grid (Read.read_lines "small.txt")
	val large_grid_1 = create_grid (Read.read_lines "large.txt")
	val small_start = find_guard small_grid_1
	val large_start = find_guard large_grid_1

    val p1s = part_1 small_grid_1
	val _ = print ("Part 1 small expected 41 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 large_grid_1
	val _ = print ("Part 1 large expected 5564 and got: " ^ (Int.toString p1l) ^ "\n")

    val p2s = part_2 (small_grid_1, small_start)
	val _ = print ("Part 2 small expected 6 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 (large_grid_1, large_start)
	val _ = print ("Part 2 large expected 1976 and got: " ^ (Int.toString p2l) ^ "\n")

	in
    	()
	end

end

(*
val _ = Main.main ()
*)


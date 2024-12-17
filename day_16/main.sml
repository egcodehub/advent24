structure Main =
struct

structure A = Array2

type coord = int * int

fun read_maze (file_name : string) : char A.array =
	(
		A.fromList o
		(List.map String.explode) o
		Read.read_lines
	) file_name

fun print_pt (x, y)	=
	"(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"

fun find_ends (maze : char A.array) : coord * coord = let
	val (max_row, max_col) = A.dimensions maze
	fun aux (row, col, s, e) =
		if row >= max_row then
			(s, e)
		else if col >= max_col then
			aux (row + 1, 0, s, e)
		else
			case (s, e, A.sub (maze, row, col))
			  of (NONE, _, #"S") => aux (row, col + 1, SOME (row, col), e)
			   | (_, NONE, #"E") => aux (row, col + 1, s, SOME (row, col))
			   | (_, _, c) =>
				 if c = #"#" orelse c = #"." then
    				 aux (row, col + 1, s, e)
				 else
    				 raise Fail ("Invalid character at: " ^ (print_pt (row, col)))
	in
		case aux (0, 0, NONE, NONE)
		  of (SOME s, SOME e) => (s, e)
		   | _ => raise Fail "Maze is missing start or end"
	end

fun in_bounds maze (row, col) = let
	val (max_row, max_col) = A.dimensions maze
	in
    	(row >= 0) andalso (row < max_row) andalso (col >= 0) andalso (col < max_col)
	end

fun step ((row, col), (d_row, d_col)) =
	(row + d_row, col + d_col)

fun negate (x, y) =
	(~x, ~y)

fun dot_product ((x1, y1), (x2, y2)) =
	(x1 * x2) + (y1 * y2)

fun find_steps (maze, p) : coord list = let
	val dirs = [(0, 1), (1, 0), (0, ~1), (~1, 0)]
	fun aux [] =
		[]
	  | aux (x :: xs) = let
		val (n_row, n_col) = step (p, x)
		in
			if in_bounds maze (n_row, n_col) then
    			case A.sub (maze, n_row, n_col)
				  of #"." => x :: (aux xs)
				   | #"E" => x :: (aux xs)
				   | _ => aux xs
			else
				aux xs
		end
	in
		aux dirs
	end

structure S = Set (CoordOrd)

fun find_min (this, min) =
	case (this, min)
	  of (NONE  , NONE  ) => NONE
	   | (SOME _, NONE  ) => this
	   | (NONE  , SOME _) => min
	   | (SOME (x, _), SOME (m, _)) =>
		 if x < m then
    		 this
		 else
    		 min

fun part_1 (maze : char A.array) : int = let
	fun weight_path (current as (row, col) : coord, seen : S.set, v : coord) : ((int * coord list) list) option =
		if (A.sub (maze, row, col)) = #"E" then
			SOME [(1000, [current])]
		else if Option.isSome (S.find (current, seen)) then
			NONE
		else let
			val next_vs : coord list = find_steps (maze, current)
			val n_seen = S.insert (current, seen)
			fun aux (n_v : coord) : (int * coord list) list option =
				case weight_path (step (current, n_v), n_seen, n_v)
				  of NONE => NONE
				   | SOME ps => let
					 val score =
    					 if (dot_product (v, n_v)) = 0 then 1001 else 1
					 val f = fn (s : int, xs : coord list) => (s + score, current :: xs)
    				 in
        				 SOME (List.map f ps)
					 end
			fun help (NONE : (int * coord list) list option, acc : (int * coord list) list) =
				acc
			  | help (SOME a, acc) =
				a @ acc
			in
				case next_vs
				  of [] => NONE
				   | _ => SOME (List.foldr help [] (List.map aux next_vs))
			end
	val (s, e) = find_ends maze

	val gg = fn (b, c) => if b then ">" ^ (print_pt c) ^ "<" else print_pt c
	val xs =
		case weight_path (s, S.empty, (1, 1))
		  of NONE => raise Fail "Couldn't reach the exit"
		   | SOME res => res
	fun hold ((s, _) : int * coord list, acc : int option) =
		case acc
		  of NONE => SOME s
		   | SOME a => SOME (Int.min (s, a))
	val sum = List.foldl hold NONE xs

(*
	fun pp (s, xs) = let
		val n = Int.toString s
		val ls = MyList.to_string print_pt ("[", ", ", "]\n") xs
		in
    		"Cost: " ^ n ^ "\nPath:\n" ^ ls
		end
	val _ = print (MyList.to_string pp ("---\n", "---\n", "---\n") xs)
*)
	in
		Option.valOf sum
	end

fun main () = let
	val s_maze = read_maze "small.txt"
    val p1s = part_1 s_maze
	val _ = print ("Part 1 small expected 7036 and got: " ^ (Int.toString p1s) ^ "\n")

	val m_maze = read_maze "medium.txt"
    val p1m = part_1 m_maze
	val _ = print ("Part 1 medium expected 11048 and got: " ^ (Int.toString p1m) ^ "\n")
(*
*)

(*
*)
	val l_maze = read_maze "large.txt"
    val p2l = part_1 l_maze
	val _ = print ("Part 2 large expected ??? and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


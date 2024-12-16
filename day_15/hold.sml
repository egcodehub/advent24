structure Main =
struct

type coord = int * int

datatype dir = U | D | R | L

structure LN = Set (LineOrd)

type robot = {
	pos : coord,
	steps : dir list
}

type warehouse = {
	boxes : LN.set,
	walls : LN.set
}

fun pair_str (x, y) =
	"(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"

fun parse_input (file_name : string) : robot * warehouse = let
	fun explore_line (row, col, robot, bs, ws, "") =
		(robot, bs, ws)
	  | explore_line (row, col, robot, bs, ws, s) =
		case MyStr.first s
		  of (#".", rest) =>
			 explore_line (row, col + 2, robot, bs, ws, rest)
		   | (#"O", rest) =>
			 explore_line (row, col + 2, robot, LN.insert (((row, col), 2), bs), ws, rest)
		   | (#"#", rest) =>
			 explore_line (row, col + 2, robot, bs, LN.insert (((row, col), 2), ws), rest)
		   | (#"@", rest) => (
			 case robot
			   of NONE => explore_line (row, col + 2, SOME (row, col), bs, ws, rest)
				| SOME _ => raise Fail ("Second robot at: " ^ (pair_str (row, col)))
			 )
		   | _ => raise Fail ("Invalid character at: " ^ (pair_str (row, col)))
	fun explore_warehouse (row, robot, bs, ws, []) =
		raise Fail "Input ended before the directions"
	  | explore_warehouse (row, robot, bs, ws, x :: xs) =
		case MyStr.first x
		  of (#"#", _) => let
			 val (n_robot, n_bs, n_ws) = explore_line (row, 0, robot, bs, ws, x)
			 in
    			 explore_warehouse (row + 1, n_robot, n_bs, n_ws, xs)
			 end
		   | _ => (
			 case robot
			   of NONE => raise Fail "The warehouse has no robot"
				| SOME r => (r, bs, ws, x :: xs)
			 )
	fun parse_directions [] =
		[]
	  | parse_directions ("" :: xs) =
		parse_directions xs
	  | parse_directions (s :: xs) =
		case MyStr.first s
		  of (#"<", rest) => L :: (parse_directions (rest :: xs))
		   | (#">", rest) => R :: (parse_directions (rest :: xs))
		   | (#"v", rest) => D :: (parse_directions (rest :: xs))
		   | (#"^", rest) => U :: (parse_directions (rest :: xs))
		   | _ => raise Fail "Invalid character found while parsing directions"
    val (robot, bs, ws, xs) = explore_warehouse (0, NONE, LN.empty, LN.empty, Read.read_lines file_name)
	val dirs = parse_directions xs
	in
		({ pos = robot, steps = dirs }, { boxes = bs, walls = ws })
	end

fun dir_to_vect d =
	case d
	  of U => (~1,  0)
	   | D => ( 1,  0)
	   | L => ( 0, ~1)
	   | R => ( 0,  1)

fun move ((x, y), (dx, dy)) =
	(x + dx, y + dy)

fun part_2 ({ pos, steps }, { boxes, walls }) = let
	fun shift_boxes (m, v, bs, walls) = let
		val p = move (m, v)
		in
    		case LN.find (p, walls)
			  of SOME _ => NONE
			   | NONE => (
				 case LN.find (p, bs)
				   of NONE => SOME (LN.insert (p, LN.remove (m, bs)))
					| SOME _ => (
					  case shift_boxes (p, v, bs, walls)
						of NONE => NONE
						 | SOME n_bs => SOME (LN.insert (p, LN.remove (m, n_bs)))
					  )
				 )
		end
	fun do_steps ([], (row, col), bs) =
		bs
	  | do_steps (step :: xs, (row, col), bs) = let
		val v = dir_to_vect step
		val m = move ((row, col), v)
(*
		val _ = print ("Moving the point " ^ (pair_str (row, col)) ^ " to " ^ (pair_str m) ^ "\n")
*)
		in
    		case LN.find (m, walls)
			  of SOME _ => do_steps (xs, (row, col), bs)
			   | NONE => (
				 case LN.find (m, bs)
				   of NONE => do_steps (xs, m, bs)
					| SOME _ => (
					  case shift_boxes (m, v, bs, walls)
						of NONE => do_steps (xs, (row, col), bs)
						 | SOME n_bs => do_steps (xs, m, n_bs)
					  )
				 )
		end
	val n_bs = do_steps (steps, pos, boxes)
	val g = fn (x, y) => "Box at: " ^ (pair_str (x, y))
	val _ = print (MyList.to_string g ("", "\n", "\n") (LN.to_list n_bs))
	in
    	List.foldl (fn ((x, y), acc) => ((100 * x) + y) + acc) 0 (LN.to_list n_bs)
	end

fun main () = let
	val small_input = parse_input "small.txt"
	val medium_input = parse_input "medium.txt"
	val large_input = parse_input "large.txt"

    val p1s = part_2 small_input
	val _ = print ("Part 2 small expected 2028 and got: " ^ (Int.toString p1s) ^ "\n")

    val p1m = part_2 medium_input
	val _ = print ("Part 2 medium expected 10092 and got: " ^ (Int.toString p1m) ^ "\n")

    val p1l = part_2 large_input
	val _ = print ("Part 2 large expected 1383666 and got: " ^ (Int.toString p1l) ^ "\n")

(*
    val p2l = part_2 (large_lines, (101, 103))
	val _ = print ("Part 2 large expected 8050 and got: " ^ (Int.toString p2l) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


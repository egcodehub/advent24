structure Main =
struct

type coord = int * int

datatype dir = U | D | R | L

structure D = Dictionary (IntOrd)

type robot = {
	pos : coord,
	steps : dir list
}

type warehouse = {
	boxes : int list D.dict,
	walls : int list D.dict
}

(* This is actually a debug function. *)
fun pair_str (x, y) =
	"(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"

fun parse_input (file_name : string, width) : robot * warehouse = let
	fun explore_line (col, "") =
		(NONE, [], [])
	  | explore_line (col, s) =
		case MyStr.first s
		  of (#".", rest) => let
			 val (r, b, w) = explore_line (col + width, rest)
			 in
    			 (r, b, w)
			 end
		   | (#"O", rest) => let
			 val (r, b, w) = explore_line (col + width, rest)
			 in
    			 (r, col :: b, w)
			 end
		   | (#"#", rest) => let
			 val (r, b, w) = explore_line (col + width, rest)
			 in
    			 (r, b, col :: w)
			 end
		   | (#"@", rest) => let
			 val (r, b, w) = explore_line (col + width, rest)
			 val n_r = case r of NONE => SOME col | SOME _ => raise Fail "Repeated robot"
			 in
    			 (n_r, b, w)
			 end
		   | _ => raise Fail "Invalid character found"
	fun explore_warehouse (row, robot, dbs, dws, []) =
		raise Fail "Input ended before the directions"
	  | explore_warehouse (row, robot, dbs, dws, x :: xs) =
		case MyStr.first x
		  of (#"#", _) => let
			 val (r, bs, ws) = explore_line (0, x)
			 val n_robot = case robot of NONE => (case r of NONE => NONE | SOME c => SOME (row, c)) | SOME _ => robot
			 val n_dbs = case bs of [] => dbs | _ => D.insert ((row, bs), dbs)
			 val n_dws = case ws of [] => dws | _ => D.insert ((row, ws), dws)
			 in
    			 explore_warehouse (row + 1, n_robot, n_dbs, n_dws, xs)
			 end
		   | _ => (
			 case robot
			   of NONE => raise Fail "The warehouse has no robot"
				| SOME r => (r, dbs, dws, x :: xs)
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
    val (robot, bs, ws, xs) = explore_warehouse (0, NONE, D.empty, D.empty, Read.read_lines file_name)
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

(* the point we want to check.
If we dont' find the row in the walls dictionary then there is no collision.

fun take_collision (obsts, (r, c), width) =
	case D.find (r, obsts)
	  of NONE => NONE
	   | SOME xs => let
		 fun aux [] =
			 NONE
		   | aux (x :: xs) =
			 if (c >= x) andalso (c + width - 1) < (x + 2) orelse (x >= c) andalso (c + width) > (x + 1) then
    			 SOME (x, xs)
			 else
    			 case aux xs
				   of NONE => NONE
					| SOME (a, ys) => SOME (a, x :: ys)
		 in
    		 aux xs
		 end
*)

fun take_collision (obsts, (r, c), width) =
	case D.find (r, obsts)
	  of NONE => NONE
	   | SOME xs => let
		 fun aux [] =
			 NONE
		   | aux (x :: xs) =
			 let
			 val _ = print ("Comparing c " ^ (pair_str (r, c)) ^ " and x " ^ (pair_str (r, x)) ^ "\n")
			 val g = Int.toString
			 val cs = g c
			 val ds = g x
			 val ws = g width
(*
			 val _ = print ("c = " ^ cs ^ "\n")
			 val _ = print ("x = " ^ ds ^ "\n")
			 val _ = print ("w = " ^ ws ^ "\n")
			 val _ = print (cs ^ " >= " ^ ds ^ " andalso " ^ (g (c + width - 1)) ^ " < " ^ (g (x + 2)) ^ "\n")
			 val _ = print ((g (c + width - 1)) ^ " >= " ^ ds ^ " andalso " ^ cs ^ " < " ^ (g (x + 2)) ^ "\n")
*)
			 in
			 if (c >= x) andalso c < (x + 2) orelse (c + width - 1) >= x andalso (c + width - 1) < (x + 2) then
    			 let
    			 val _ = print ("The point " ^ (pair_str (r, c)) ^ " +" ^ (Int.toString width) ^ " colides with " ^ (pair_str (r, x) ^ " +2\n"))
    			 in
    			 SOME ((r, x), xs)
    			 end
			 else
    			 case aux xs
				   of NONE => NONE
					| SOME (p, ys) => SOME (p, x :: ys)
			 end (* outs*)
		 in
    		 aux xs
		 end

(*
This kinda useless, no?
*)
fun displace_end (row : int, v : coord, start : int, width : int) : coord =
	if v = (0, ~1) then
		(row, start - 1)
	else if v = (0, 1) then
		(row, start + width)
	else
		raise Fail "Invalid vector for horizontal move"

fun take_mult_collision (obsts : int list D.dict, (r, c) : coord, w : int) : (coord list * int list) option =
	case D.find (r, obsts)
	  of NONE => NONE
	   | SOME xs => let
		 fun aux (_ : int, _ : int, [] : int list) : coord list * int list =
			 ([], [])
		   | aux (new_start, w, obj_start :: xs) = let
			 val (touch, no_touch) = aux (new_start, w, xs)
			 val new_in_obj = new_start >= obj_start andalso new_start < (obj_start + 2)
			 val obj_in_new = obj_start >= new_start andalso obj_start < (new_start + w)
			 val n_touch = if new_in_obj orelse obj_in_new then (r, obj_start) :: touch else touch
			 val n_no_touch = if new_in_obj orelse obj_in_new then no_touch else obj_start :: no_touch
			 in
    			 (n_touch, n_no_touch)
			 end
		 in
    		 case aux (c, w, xs)
			   of ([], _) => NONE
				| (ys, zs) => SOME (ys, zs)
		 end

fun insert (a, []) =
	[a]
  | insert (a, x :: xs) =
	if a = x then
		xs
	else
		x :: (insert (a, xs))

fun append_col (dict, k, v) =
	case D.find (k, dict)
	  of NONE => raise Fail "The key must exist"
	   | SOME xs => D.modify ((k, v :: xs), dict)

fun update_vertical (dict, (k, v)) =
	let
	val _ = print ("Updating the line with: " ^ (Int.toString k) ^ "\n")
	in
	case D.find (k, dict)
	  of NONE => D.insert ((k, [v]), dict)
	   | SOME xs => D.modify ((k, v :: xs), dict)
	end

fun part_2 (({ pos, steps }, { boxes, walls }), width) = let
    fun vertical_move (m as (r, _), v, bs, w) =
		case take_mult_collision (walls, m, w)
		  of SOME _ => (print "Just bumped into al wall\n";NONE)
		   | NONE => (
			 case take_mult_collision (bs, m, w)
			   of NONE => (print ("There is room for point: " ^ (pair_str m) ^ "\n");SOME bs)
				| SOME (hits, left) => let
				  fun aux ([], bs) =
					  SOME bs
					| aux (h :: hits, bs) = let
					  val n_m as (n_r, s_c) = move (h, v)
					  in
    					  case vertical_move (n_m, v, bs, 2)
    						of NONE => NONE
    						 | SOME nbs => aux (hits, update_vertical (nbs, n_m))
					  end
				  val _ =
    				  case hits
						of [] => print ("Didn't find any hits for " ^ (pair_str m) ^ "\n")
    					 | _ => print (MyList.to_string pair_str ("Found: ", ",", "\n") hits)
				  val _ =
    				  case left
						of [] => print ("It was all hits for " ^ (pair_str m) ^ "\n")
    					 | _ => print (MyList.to_string Int.toString ("Not hit ones: ", ",", "\n") left)
				  val n_left =
    				  case left
						of [] => D.remove (r, bs)
    					 | _ => D.modify ((r, left), bs)
(*
				  val n_left = case left of [] => D.remove (r, bs) | _ => D.insert ((r, left), bs)
*)
				  in
    				  aux (hits, n_left)
				  end
			 )
    fun horizontal_move (m as (r, _), v, bs, w) =
		case take_collision (walls, m, w)
		  of SOME _ => NONE
		   | NONE => (
			 case take_collision (bs, m, w)
			   of NONE => SOME bs
				| SOME (p, rest) => let
				  val n_m as (_, n_s) = move (p, v)
(*
				  val _ = print ("Hor colide with: " ^ (pair_str p) ^ "\n")
				  val _ = print ("Trying to shove in: " ^ (pair_str n_m) ^ "\n")
				  val _ = print "Showing rest after taking the colide from it:\n"
				  val _ = print (MyList.to_string Int.toString ("[", ", ", "]\n") rest)
				  val _ = print ("We will append: " ^ (Int.toString n_s) ^ "\n")
*)
				  in
    				  case horizontal_move (n_m, v, D.modify ((r, rest), bs), 2)
    					of NONE => NONE
    					 | SOME n_bs => SOME (append_col (n_bs, r, n_s))
				  end
			 )
(*
debugign tsuff move out of hre

*)
	fun esta (r, ys) = let
		val sy = MyList.to_string Int.toString ("[", ", ", "]") ys
		val all = " Boxes at line " ^ (Int.toString r) ^ ": " ^ sy
		in all end
	fun do_steps ((row, col), bs, []) =
		bs
	  | do_steps (pos, bs, x :: xs) = let
		val v = dir_to_vect x
		val m = move (pos, v)
    	val _ = print "Boxes before next step\n"
    	val _ = print (MyList.to_string esta ("", "\n", "\n") (D.to_list bs))
		val _ = print ("\nCurrently at: " ^ (pair_str pos) ^ "\n")
		val _ = print ("Trying to go to: " ^ (pair_str m) ^ "\n")
		val _ = print ("With vector: " ^ (pair_str v) ^ "\n")
		val a =
			if x = L orelse x = R then
    			horizontal_move (m, v, bs, 1)
			else
    			vertical_move (m, v, bs, 1)
		in
			case a
			  of NONE => do_steps (pos, bs, xs)
			   | SOME n_bs => do_steps (m, n_bs, xs)
		end
    val n_bs : (int * int list) list = D.to_list (do_steps (pos, boxes, steps))
	val f = fn ((x, ys), acc) => (List.foldl (fn (y, ycc) => (100 * x + y) + ycc) 0 ys) + acc

	val _ = print ("Robot position: " ^ (pair_str pos) ^ "\n")
	val _ = print "Boxes while\n"
	val _ = print (MyList.to_string esta ("", "\n", "\n") (D.to_list boxes))
	val _ = print "Boxes at the end\n"
	val _ = print (MyList.to_string esta ("", "\n", "\n") n_bs)

	in
    	List.foldl f 0 n_bs
	end

fun main () = let
(*
	val small_input_2 = parse_input ("small_second.txt", 2)
    val p2s = part_2 (small_input_2, 2)
	val _ = print ("Part 2 small expected 618 and got: " ^ (Int.toString p2s) ^ "\n")

*)
(*
	val medium_input_2 = parse_input ("medium.txt", 2)
    val p2s = part_2 (medium_input_2, 2)
	val _ = print ("Part 2 medium expected 9021 and got: " ^ (Int.toString p2s) ^ "\n")
*)
	val large_input_2 = parse_input ("large.txt", 2)
    val p2l = part_2 (large_input_2, 2)
	val _ = print ("Part 2 large expected 1412866 and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


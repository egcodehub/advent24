structure Main =
struct

structure A = Array2

structure S = Set (CoordOrd)
structure C = Dictionary (CharOrd)

type pos = int * int

(* DEBUGGING *)
(*
fun str_point (x, y) =
	"(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"
*)

fun parse_input (file_name : string) : (int * int) * S.set C.dict = let
	val txt = Read.read_file file_name
	val len = String.size txt
	fun aux (i, r, c, w, d) =
		if i >= len then
			((r, Int.max (c, w)), d)
		else
			case String.sub (txt, i)
			  of #"\n" => aux (i + 1, r + 1, 0, Int.max (c, w), d)
			   | a =>
				 if Char.isAlphaNum a then let
					 val new_d =
        				 case C.find (a, d)
    					   of NONE =>
    						  C.insert ((a, S.insert ((r, c), S.empty)), d)
    						| SOME s =>
    						  C.modify ((a, S.insert ((r, c), s)), d)
					 in
    					 aux (i + 1, r, c + 1, w, new_d)
					 end
				 else
    				 aux (i + 1, r, c + 1, w, d)
	in
        aux (0, 0, 0, 0, C.empty)
	end

(* Debugging function. *)
(*
fun print_points (d : S.set C.dict) : int = let
	val l : (char * S.set) list = C.to_list d
	fun aux (c, ps) : string = let
		val fst : string = "(" ^ (str c) ^ ", "
		val p = fn (x, y) => "(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"
		val ss : string = MyList.to_string p ("[", ", ", "]") (S.to_list ps)
    	in
        	fst ^ ss ^ ")"
    	end
	val _ = print (MyList.to_string aux ("[", ", ", "]\n") l)
	in
    	0
	end
*)

fun in_bounds ((max_r, max_c), (r, c)) =
	(r >= 0) andalso (c >= 0) andalso (r < max_r) andalso (c < max_c)

fun two_antinodes (p1, p2) = let
	val (x1, y1) = p1
	val (x2, y2) = p2
	val (dx, dy) = (x2 - x1, y2 - y1)
(*
	val _ = print ("Diff is: " ^ (str_point (dx, dy)) ^ "\n")
*)
	val an_p1 = (x1 - dx, y1 - dy)
	val an_p2 = (x2 + dx, y2 + dy)
	in
    	(an_p1, an_p2)
	end

fun find_antinodes dims (pivot, [], acc) =
	acc
  | find_antinodes dims (pivot, x :: xs, acc) =	let
	val (a1, a2) = two_antinodes (pivot, x)
(*
	val _ = print ("First pivot: " ^ (str_point pivot) ^ "\n")
	val _ = print ("Second pivot: " ^ (str_point x) ^ "\n")
	val _ = print ("First antinode: " ^ (str_point a1) ^ "\n")
	val _ = print ("Second antinode: " ^ (str_point a2) ^ "\n")
	val _ = print "\n"
*)
	val acc0 = if in_bounds (dims, a1) then S.insert (a1, acc) else acc
	val acc1 = if in_bounds (dims, a2) then S.insert (a2, acc0) else acc0
	in
    	find_antinodes dims (pivot, xs, acc1)
	end

fun add_antinodes (dims : int * int) (ps : pos list, acc : S.set) : S.set =
	case ps
	  of [] => acc
	   | x :: [] => acc
	   | x :: xs => add_antinodes dims (xs, find_antinodes dims (x, xs, acc))

fun part_1 (dims : int * int, d : S.set C.dict) : int = let
	val f = fn ((_, ps), acc) => add_antinodes dims (S.to_list ps, acc)
	val antinodes = List.foldl f S.empty (C.to_list d)
(*
	val _ = print (MyList.to_string str_point ("[", ", ", "]\n") (S.to_list antinodes))
	val _ = print ("Dimensions: " ^ (str_point dims) ^ "\n")
*)
	in
    	S.size antinodes
	end

fun main () = let
	val small_ant = parse_input "small.txt"
	val large_ant = parse_input "large.txt"

    val p1s = part_1 small_ant
	val _ = print ("Part 1 small expected 14 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 large_ant
	val _ = print ("Part 1 large expected 398 and got: " ^ (Int.toString p1l) ^ "\n")

(*
    val p2s = part_2 (small_grid_1, small_start)
	val _ = print ("Part 2 small expected 6 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 (large_grid_1, large_start)
	val _ = print ("Part 2 large expected 1976 and got: " ^ (Int.toString p2l) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


structure Main =
struct

structure S = Set (CoordOrd)
structure C = Dictionary (CharOrd)

type coord = int * int

fun parse_input (file_name : string) : coord * S.set C.dict = let
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

fun in_bounds ((max_r, max_c) : coord, (r, c) : coord) : bool =
	(r >= 0) andalso (c >= 0) andalso (r < max_r) andalso (c < max_c)

structure FS = FinSeq

fun test_1 (cond : coord -> bool) (i : int) (to_dir : coord -> coord) ((r, c) : coord) : coord FS.seq = let
	val next = to_dir (r, c)
	in
    	if i > 0 andalso cond next then
        	FS.cons (SOME next, fn () => test_1 cond (i - 1) to_dir next)
    	else
        	FS.cons (NONE, fn () => test_1 cond i to_dir (r, c))
	end

fun test_2 (cond : coord -> bool) (to_dir : coord -> coord) ((r, c) : coord) : coord FS.seq = let
	val next = to_dir (r, c)
	in
    	if cond next then
        	FS.cons (SOME next, fn () => test_2 cond to_dir next)
    	else
        	FS.cons (NONE, fn () => test_2 cond to_dir (r, c))
	end

fun get_dirs ((x1, y1), (x2, y2)) = let
	val (dx, dy) = (x2 - x1, y2 - y1)
	in
		(fn (x, y) => (x - dx, y - dy), fn (x, y) => (x + dx, y + dy))
	end

fun find_antinodes (test : (coord -> coord) -> coord -> coord FS.seq) (pivot : coord, [] : coord list, acc : S.set) : S.set =
	acc
  | find_antinodes test (pivot, x :: xs, acc0) = let
	val (d1, d2) = get_dirs (pivot, x)
	(* Starting at the opposite point for each antenna allows counting the current one. *)
	val acc1 = FS.foldl S.insert acc0 (test d1 x)
	val acc2 = FS.foldl S.insert acc1 (test d2 pivot)
	in
    	find_antinodes test (pivot, xs, acc2)
	end

fun add_antinodes (test : (coord -> coord) -> coord -> coord FS.seq) (ps : coord list, acc : S.set) : S.set =
	case ps
	  of [] => acc
	   | x :: [] => acc
	   | x :: xs => add_antinodes test (xs, find_antinodes test (x, xs, acc))

fun part_x (test : (coord -> coord) -> coord -> coord FS.seq) (d : S.set C.dict) : int = let
	val f = fn ((_ : char, ps : S.set), acc) => add_antinodes test (S.to_list ps, acc)
	val antinodes = List.foldl f S.empty (C.to_list d)
	in
    	S.size antinodes
	end

fun main () = let
	val (s_dims, small_ant) = parse_input "small.txt"
	val (l_dims, large_ant) = parse_input "large.txt"

    val p1s = part_x (test_1 (fn p => in_bounds (s_dims, p)) 1) small_ant
	val _ = print ("Part 1 small expected 14 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_x (test_1 (fn p => in_bounds (l_dims, p)) 1) large_ant
	val _ = print ("Part 1 large expected 398 and got: " ^ (Int.toString p1l) ^ "\n")

    val p2s = part_x (test_2 (fn p => in_bounds (s_dims, p))) small_ant
	val _ = print ("Part 2 small expected 34 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_x (test_2 (fn p => in_bounds (l_dims, p))) large_ant
	val _ = print ("Part 2 large expected 1333 and got: " ^ (Int.toString p2l) ^ "\n")

	in
    	()
	end

end

(*
val _ = Main.main ()
*)


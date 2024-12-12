structure Main =
struct

structure A = Array2

fun read_garden (file_name : string) : (char * bool) A.array =
	(A.fromList o
	 (List.map (fn xs => List.map (fn x => (x, false)) xs)) o
	 (List.map String.explode) o
	 Read.read_lines)
	file_name

fun in_bounds ((max_r, max_c), (r, c)) =
	(r >= 0) andalso (c >= 0) andalso (r < max_r) andalso (c < max_c)

fun step ((x, y), (dx, dy)) =
	(x + dx, y + dy)

(* MORE DEBUGGING STUFF MAKE SURE TO TAKE IT OFF FORM THE FINAL THING. *)
fun sp (x, y) =
	"(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"

fun pull_thread (garden : (char * bool) A.array, start : int * int) : char * int * int = let
	val dims = A.dimensions garden
	val dirs = [(1, 0), (0, 1), (~1, 0), (0, ~1)]
	fun pick (og, v) (d, (acc, w)) = let
		val p as (x, y) = step (og, d)
		in
    		if in_bounds (dims, p) then let
        		val (a, b) = A.sub (garden, x, y)
        		in
            		if a = v then
    					if b then
    						(acc, w)
						else
							(p :: acc, w)
					else
    					(acc, w + 1)
        		end
    		else
        		(acc, w + 1)
		end
	fun search ([], (t, area, perim)) =
		(t, area, perim)
	  | search ((x as (r, c)) :: xs, (t, area, perim)) = let
		val (l, b) = A.sub (garden, r, c)
		val _ = A.update (garden, r, c, (l, true))
		val (ys, w) = List.foldl (pick (x, l)) (xs, 0) dirs
		in
			if b then
				search (ys, (t, area, perim))
			else
        		search (ys, (t, area + 1, perim + w))
		end
	val (c1, c2) = start
	val (l, _) = A.sub (garden, c1, c2)
	in
    	search ([start], (l, 0, 0))
	end

(* Debugging function. To remove *)
fun str_pair (c : char, x : int, y : int) =
	"(" ^ (str c) ^ ", " ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"

fun part_1 (garden : (char * bool) A.array) : int = let
	val (max_r, max_c) = A.dimensions garden
	fun scan (r, c, acc : (char * int * int) list) =
		if r >= max_r then
    		acc
		else if c >= max_c then
			scan (r + 1, 0, acc)
		else if (#2 (A.sub (garden, r, c))) then
    		scan (r, c + 1, acc)
		else
			scan (r, c + 1, (pull_thread (garden, (r, c))) :: acc)
	in
    	List.foldl (fn ((_, a, b), acc) => (a * b) + acc) 0 (scan (0, 0, []))
	end

fun main () = let
	val s_garden = read_garden "small.txt"
	val l_garden = read_garden "large.txt"

    val p1s = part_1 s_garden
	val _ = print ("Part 1 small expected 1930 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 l_garden
	val _ = print ("Part 1 large expected 1421958 and got: " ^ (Int.toString p1l) ^ "\n")
(*
*)

	in
    	()
	end

end

(*
val _ = Main.main ()
*)


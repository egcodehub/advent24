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

fun count_verts (g : (char * bool) A.array, code : char, pos) : int = let
	val dims = A.dimensions g
	val around = [[(~1, ~1), (~1, 0), (~1, 1)], [(0, ~1), (0, 0), (0, 1)], [(1, ~1), (1, 0), (1, 1)]]
	fun is_family (r, c) (dr, dc) = let
		val (nr, nc) = step ((r, c), (dr, dc))
		in
    		in_bounds (dims, (nr, nc)) andalso (#1 (A.sub (g, nr, nc))) = code
		end
	val t : bool A.array = A.fromList (List.map (List.map (fn xs => is_family pos xs)) around)
	fun find_corner (lt, lb, rt, rb) : int =
		if (not lb andalso not rt) orelse (not lt andalso lb andalso rt) orelse (not lt andalso not lb andalso not rt) then
			1
		else
			0
	val q1 = find_corner (A.sub (t, 0, 0), A.sub (t, 1, 0), A.sub (t, 0, 1), A.sub (t, 1, 1))
	val q2 = find_corner (A.sub (t, 0, 2), A.sub (t, 1, 2), A.sub (t, 0, 1), A.sub (t, 1, 1))
	val q3 = find_corner (A.sub (t, 2, 2), A.sub (t, 2, 1), A.sub (t, 1, 2), A.sub (t, 1, 1))
	val q4 = find_corner (A.sub (t, 2, 0), A.sub (t, 1, 0), A.sub (t, 2, 1), A.sub (t, 1, 1))
	in
    	q1 + q2 + q3 + q4
	end

structure H = Heap (CoordOrd)

fun pull_thread (garden : (char * bool) A.array, (start_row, start_col) : int * int) : char * int * int * int = let
	val dims = A.dimensions garden
	val dirs = [(1, 0), (0, 1), (~1, 0), (0, ~1)]
	fun pick (og, v) (d, (acc, w)) = let
		val next as (x, y) = step (og, d)
		in
    		if in_bounds (dims, next) then let
        		val (a, b) = A.sub (garden, x, y)
        		in
            		if a = v then
    					if b then
    						(acc, w)
						else
							(H.push (next, acc), w)
					else
    					(acc, w + 1)
        		end
    		else
        		(acc, w + 1)
		end
	fun search (xs, (code, area, perim, verts)) =
		case H.pop xs
		  of NONE => (code, area, perim, verts)
		   | SOME (point as (row, col), ys) => let
			 val _ = A.update (garden, row, col, (code, true))
			 val (zs, walls) = List.foldl (pick (point, code)) (ys, 0) dirs
			 val corners = count_verts (garden, code, point)
			 in
    			 search (zs, (code, area + 1, perim + walls, verts + corners))
			 end
	val (letter, _) = A.sub (garden, start_row, start_col)
	in
    	search (H.push ((start_row, start_col), H.empty), (letter, 0, 0, 0))
	end

fun part_x (garden : (char * bool) A.array) : (char * int * int * int) list = let
	val (max_r, max_c) = A.dimensions garden
	fun scan (r, c, acc : (char * int * int * int) list) =
		if r >= max_r then
    		acc
		else if c >= max_c then
			scan (r + 1, 0, acc)
		else if (#2 (A.sub (garden, r, c))) then
    		scan (r, c + 1, acc)
		else
			scan (r, c + 1, (pull_thread (garden, (r, c))) :: acc)
	in
        scan (0, 0, [])
	end

fun main () = let
	val s_garden = read_garden "small.txt"
	val l_garden = read_garden "large.txt"

	val s_lst = part_x s_garden
	val l_lst = part_x l_garden

	val f_1 = fn ((c, x, y, z), acc) => (x * y) + acc
	val f_2 = fn ((c, x, y, z), acc) => (x * z) + acc

	val p1s = List.foldl f_1 0 s_lst
	val p1l = List.foldl f_1 0 l_lst

	val p2s = List.foldl f_2 0 s_lst
	val p2l = List.foldl f_2 0 l_lst

	val _ = print ("Part 1 small expected 1930 and got: " ^ (Int.toString p1s) ^ "\n")
	val _ = print ("Part 1 large expected 1421958 and got: " ^ (Int.toString p1l) ^ "\n")

	val _ = print ("Part 2 small expected 1206 and got: " ^ (Int.toString p2s) ^ "\n")
	val _ = print ("Part 2 large expected 885394 and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


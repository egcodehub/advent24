structure Main =
struct

structure A = Array2

fun create_grid (lines : string list) =
	A.fromList (List.map String.explode lines)

fun does_fit ((ox, oy), (mx, my), (dx, dy), l) = let
	val ex = ox + ((l - 1) * dx)
	val ey = oy + ((l - 1) * dy)
	val bx= (ex >= 0) andalso (ex < mx)
	val by= (ey >= 0) andalso (ey < my)
	in
		bx andalso by
	end

fun scout (grid, origen, word) = let
	val dirs = [(1, 0), (1, 1), (0, 1), (~1, 1), (~1, 0), (~1, ~1), (0, ~1), (1, ~1)]
	val len = String.size word
	val wrd = String.explode word
	fun runs ([], _, _) =
		true
	  | runs (z :: zs, (x, y), (dx, dy)) =
		if (A.sub (grid, x, y)) = z then
			runs (zs, (x + dx, y + dy), (dx, dy))
		else
    		false
	fun f (step : int * int, n : int) : int =
		if does_fit (origen, A.dimensions grid, step, len) then
    		if runs (wrd, origen, step) then n + 1 else n
		else
    		n
	in
    	List.foldl f 0 dirs
	end

fun search (grid : char A.array, word : string) : int = let
	val (rows, cols) = A.dimensions grid
	val (first, _) = MyStr.first word
	fun aux (i, j) =
		if i >= rows then
			aux (0, j + 1)
		else if j >= cols then
			0
		else if (A.sub (grid, i, j)) = first then
			(scout (grid, (i, j), word)) + (aux (i + 1, j))
		else
			aux (i + 1, j)
	in
    	aux (0, 0)
	end

fun part_1 (grid : char A.array) : int =
	search (grid, "XMAS")

fun main () = let
	val small_l = create_grid (Read.read_lines "small.txt")
	val large_l = create_grid (Read.read_lines "large.txt")
    val p1s = part_1 small_l
	val _ = print ("Part 1 small expected 18 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 large_l
	val _ = print ("Part 1 large expected 2569 and got: " ^ (Int.toString p1l) ^ "\n")
(*
    val p2s = part_2 small_l
	val _ = print ("Part 2 small expected 48 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 large_l
	val _ = print ("Part 2 large expected 100450138 and got: " ^ (Int.toString p2l) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


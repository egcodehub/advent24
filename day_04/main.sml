structure Main =
struct

structure A = Array2

fun create_grid (lines : string list) =
	A.fromList (List.map String.explode lines)

type pair = int * int

fun does_fit ((ox, oy) : pair, (mx, my) : pair, (dx, dy) : pair, l : int) : bool = let
	val ex = ox + ((l - 1) * dx)
	val ey = oy + ((l - 1) * dy)
	val bx = (ex >= 0) andalso (ex < mx)
	val by = (ey >= 0) andalso (ey < my)
	in
		bx andalso by
	end

fun word_in_dir ([] : char list, _ : char A.array, _ : pair, _ : pair) : bool =
	true
  | word_in_dir (z :: zs, grid, (x, y), step as (dx, dy)) = let
	val c = A.sub (grid, x, y)
	val (nx, ny) = (x + dx, y + dy)
	in
    	(c = z) andalso (word_in_dir (zs, grid, (nx, ny), step))
	end

fun check_all_dirs (grid : char A.array, origen : pair, word : string) : int = let
	val dirs = [(1, 0), (1, 1), (0, 1), (~1, 1), (~1, 0), (~1, ~1), (0, ~1), (1, ~1)]
	val len = String.size word
	val dim = A.dimensions grid
	val wrd = String.explode word
	fun f (step : int * int, n : int) : int =
		if does_fit (origen, dim, step, len) then
    		if word_in_dir (wrd, grid, origen, step) then n + 1 else n
		else
    		n
	in
    	List.foldl f 0 dirs
	end

fun word_all_dirs (grid : char A.array, word : string) : int = let
	val (rows, cols) = A.dimensions grid
	val (first, _) = MyStr.first word
	fun aux (i, j) =
		if i >= rows then
			aux (0, j + 1)
		else if j >= cols then
			0
		else let
			val c = A.sub (grid, i, j)
			val n = if c = first then check_all_dirs (grid, (i, j), word) else 0
			in
				n + (aux (i + 1, j))
			end
	in
    	aux (0, 0)
	end

fun part_1 (grid : char A.array) : int =
	word_all_dirs (grid, "XMAS")

fun check_window (grid : char A.array, (x, y) : int * int, side : int, word : string) : int = let
	val nw = String.explode word
	val rw = List.rev nw
	val d1 = ( 1, 1)
	val d2 = (~1, 1)
	val o1 = (x, y)
	val o2 = (x + side - 1, y)
	val c1 = (word_in_dir (nw, grid, o1, d1)) orelse (word_in_dir (rw, grid, o1, d1))
	val c2 = (word_in_dir (nw, grid, o2, d2)) orelse (word_in_dir (rw, grid, o2, d2))
	in
    	if c1 andalso c2 then 1 else 0
	end

fun cross_search (grid : char A.array, word : string) : int = let
	val side = String.size word
	val (rows, cols) = A.dimensions grid
	val (first, _) = MyStr.first word
	val (last, _ ) = MyStr.last  word
	fun aux (i, j) =
		if (j + side - 1) >= cols then
			0
		else if (i + side - 1) >= rows then
			aux (0, j + 1)
		else let
			val c = A.sub (grid, i, j)
			val b = (c = first) orelse (c = last)
			val n = if b then check_window (grid, (i, j), side, word) else 0
			in
				n + (aux (i + 1, j))
			end
	in
    	aux (0, 0)
	end

fun part_2 (grid : char A.array) : int =
	cross_search (grid, "MAS")

fun main () = let
	val small_l = create_grid (Read.read_lines "small.txt")
	val large_l = create_grid (Read.read_lines "large.txt")
    val p1s = part_1 small_l
	val _ = print ("Part 1 small expected 18 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 large_l
	val _ = print ("Part 1 large expected 2569 and got: " ^ (Int.toString p1l) ^ "\n")
    val p2s = part_2 small_l
	val _ = print ("Part 2 small expected 9 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 large_l
	val _ = print ("Part 2 large expected 1998 and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


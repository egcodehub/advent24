structure Main =
struct

structure A = Array2

fun read_map (file_name : string) : int A.array = let
	val f = fn c => (Char.ord c) - (Char.ord #"0")
	in
    	A.fromList (((List.map (List.map f)) o (List.map String.explode) o Read.read_lines) file_name)
	end

fun is_start i =
	i = 0

type coord = int * int
type path = coord list

(* DEBUG FUNCTION *)
fun str_point (x, y) =
	"(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")"

fun find_paths (map : int A.array, f, init) = let
	(* It's useless to optimize by not going down the path we just came from. *)
	val dirs = [(1, 0), (0, ~1), (~1, 0), (0, 1)]
	val (max_row, max_col) = A.dimensions map
	fun in_bounds (r, c) =
		(r >= 0) andalso (c >= 0) andalso (r < max_row) andalso (c < max_col)
	fun step (d_row, d_col) (row, col) = let
		val n_pos = (row + d_row, col + d_col)
		in
    		if in_bounds n_pos then
    			SOME n_pos
			else
    			NONE
		end
	fun search_paths (NONE : (int * int) option, _ : int option) : path list =
		[]
	  | search_paths (SOME (r, c), prev) = let
		val v = A.sub (map, r, c)
		in
			if (not (Option.isSome prev) andalso v = 0) orelse (Option.isSome prev andalso v = ((Option.valOf prev) + 1)) then let
				val l1 = List.map (fn d => search_paths (step d (r, c), SOME v)) dirs
				in
					case List.filter (not o List.null) l1
			 		  of [] => [[(r, c)]]
					   | paths => List.map (fn ps => (r, c) :: ps) (List.concat paths)
				end
			else
    			[]
		end
	fun check (r, c, _, acc) = f ((search_paths (SOME (r, c), NONE)), acc)
    val range = { base = map, row = 0, col = 0, nrows = NONE, ncols = NONE }
	in
		A.foldi A.RowMajor check init range
	end

structure S = Set (CoordOrd)

fun tts (lst : path list, acc : int) : int = let
	val len_ten = List.filter (fn xs => (List.length xs) = 10) lst
	in
    	(S.size (S.from_list (List.map List.last len_ten))) + acc
	end

fun part_1 (map : int A.array) : int =
	find_paths (map, tts, 0)

fun main () = let
	val s_map = read_map "small.txt"
	val l_map = read_map "large.txt"

    val p1s = part_1 s_map
	val _ = print ("Part 1 small expected 36 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 l_map
	val _ = print ("Part 1 large expected 744 and got: " ^ (Int.toString p1l) ^ "\n")

(*
    val p2s = part_2 s_map
	val _ = print ("Part 2 small expected 2858 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2s = part_2 l_map
	val _ = print ("Part 2 small expected 6323761685944 and got: " ^ (Int.toString p2s) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


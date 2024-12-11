structure IntIntLst : ORD_KEY =
struct

type t = (int * int) list

fun cmp ([], []) =
	General.EQUAL
  | cmp ([], ys) =
	General.LESS
  | cmp (xs, []) =
    General.GREATER
  | cmp ((a, b) :: xs, (c, d) :: ys) =
	if a < c then
		General.LESS
	else if a > c then
		General.GREATER
	else if b < d then
		General.LESS
	else if b > d then
		General.GREATER
	else
		cmp (xs, ys)

fun eq (l1, l2) =
	(cmp (l1, l2)) = General.EQUAL

end

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

fun find_paths (map : int A.array) = let
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
			case prev
			  of NONE =>
				 if v = 0 then let
					 val _ = print ("Paths from " ^ (str_point (r, c)) ^ "\n")
    				 val l1 : path list list = List.map (fn d => search_paths (step d (r, c), SOME v)) dirs

					 val pp = fn ps => MyList.to_string str_point ("[", ", ", "]\n") ps
					 val _ = print "Printing paths:\n"
					 val _ = List.foldl (fn (pl, _) => List.foldl (fn (b, _) => print (pp b)) () pl) () l1
					 val _ = List.map (fn pl => List.map pp pl) l1
					 val _ = print "-------\n"

					 val l2 : path list =
    					 case List.filter (not o List.null) l1
				 		   of [] => [[(r, c)]]
							| paths => let
							  val short = List.concat paths
							  val _ = print ("We got " ^ (Int.toString (List.length short)) ^ " paths\n")
							  val _ = List.foldl (fn (x, _) => print (pp x)) () short
    						  val res = List.map (fn ps => (r, c) :: ps) (List.concat paths)
							  val _ = List.foldl (fn (x, _) => print (pp x)) () res
							  in
    							  res
							  end
(*
					 val l2 : path list list = List.map (List.map (fn ps => (r, c) :: ps)) l1
*)
    				 in
    					 l2
    				 end
				 else
    				 []
			   | SOME p =>
				 if v = (p + 1) then let
    				 val l1 : path list list = List.map (fn d => search_paths (step d (r, c), SOME v)) dirs
					 val l2 : path list =
    					 case List.filter (not o List.null) l1
				 		   of [] => [[(r, c)]]
							| pths =>
							  List.map (fn p => (r, c) :: p) (List.concat pths)
    				 in
    					 l2
    				 end
(*
    				 val l1 : path list list = List.map (fn d => search_paths (step d (r, c), SOME v)) dirs
					 val l2 : path list list =
    					 case List.filter (not o List.null) l1
				 		   of [] => [[[(r, c)]]]
							| pths => List.map (fn q => List.map (fn p => (r, c) :: p) q) pths
    				 in
    					 List.concat l2
    				 end
*)
				 else
    				 []
		end
	fun check (r, c, _, acc) = (search_paths (SOME (r, c), NONE)) @ acc
    val range = { base = map, row = 0, col = 0, nrows = NONE, ncols = NONE }
	in
		A.foldi A.RowMajor check [] range
	end

structure S = Set (IntIntLst)

fun part_1 (map : int A.array) : int = let
	val _ = ()
	val paths : path list = find_paths map
	val s = S.from_list (List.filter (fn ps => (List.length ps) = 10) paths)
	in
		S.size s
	end

fun main () = let
	val s_map = read_map "small.txt"
	val l_map = read_map "large.txt"

    val p1s = part_1 s_map
	val _ = print ("Part 1 small expected 36 and got: " ^ (Int.toString p1s) ^ "\n")
(*
    val p1l = part_1 l_map
	val _ = print ("Part 1 large expected 6301895872542 and got: " ^ (Int.toString p1l) ^ "\n")

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


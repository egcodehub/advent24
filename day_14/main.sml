structure Main =
struct

fun read_info (file_name : string) : ((int * int) * (int * int)) list = let
	val not_in_num = fn c => (c <> #"-") andalso not (Char.isDigit c)
	val s_to_n = Option.valOf o Int.fromString
	val get_numbers = fn s =>
		case List.map s_to_n (String.tokens not_in_num s)
		  of px :: py :: vx :: vy :: [] => ((px, py), (vx, vy))
		   | _ => raise Fail "Invalid line format"
	in
    	List.map get_numbers (Read.read_lines file_name)
	end

fun shift_point (((px, py), (vx, vy)), times, (width, height)) = let
	val new_px = (px + vx * times) mod width
	val new_py = (py + vy * times) mod height
	in
    	(new_px, new_py)
	end

datatype quadrant = Q1 | Q2 | Q3 | Q4 | NB

fun find_quadrant ((x, y), (width, height)) = let
	val left_w = (width div 2) - 1
	val top_h = (height div 2) - 1
	val right_w = width - left_w - 1
	val bot_h = height - top_h - 1
	in
		if x <= left_w andalso y <= top_h then
			Q1
		else if x <= left_w andalso y >= bot_h then
			Q3
		else if x >= right_w andalso y <= top_h then
			Q2
		else if x >= right_w andalso y >= bot_h then
			Q4
		else
			NB
	end

fun count_quadrants (dims, times) (info, qs as (q1, q2, q3, q4)) =
	case find_quadrant (shift_point (info, times, dims), dims)
	  of Q1 => (q1 + 1, q2, q3, q4)
	   | Q2 => (q1, q2 + 1, q3, q4)
	   | Q3 => (q1, q2, q3 + 1, q4)
	   | Q4 => (q1, q2, q3, q4 + 1)
	   | NB => qs

fun part_1 (lines : ((int * int) * (int * int)) list, dims, times) : int = let
	val g = Int.toString
	val f = fn ((a, b), (c, d)) => "p: (" ^ (g a) ^ ", " ^ (g b) ^ ") | v: (" ^ (g c) ^ ", " ^ (g d) ^ ")"
    val (q1, q2, q3, q4) = List.foldl (count_quadrants (dims, times)) (0, 0, 0, 0) lines
	in
		q1 * q2 * q3 * q4
	end

structure A = Array2

fun part_2 (lines, (rows, cols)) = let
	val canvas = A.array (rows, cols, #" ")
	fun update (arr, robots) = let
		val _ = A.modify A.RowMajor (fn x => #" ") arr
		in
			List.foldl (fn ((x, y), _) => A.update (arr, x, y, #"#")) () robots
		end
	fun draw (arr, (rows, cols)) = let
		fun aux (i, j) =
			if i >= rows then
				()
			else if j >= cols then let
				val _ = print "|\n|"
				in
    				aux (i + 1, 0)
				end
			else let
				val _ = print (str (A.sub (arr, i, j)))
				in
    				aux (i, j + 1)
				end
		val _ = aux (0, 0)
		val _ = print "\n"
		in
			()
		end
	fun loop (arr, i) : int = let
		val _ = update (arr, List.map (fn l => shift_point (l, i, (rows, cols))) lines)
		val _ = draw (arr, (rows, cols))
		val _ = print ("State at step " ^ (Int.toString i) ^ "\n")
		val _ = print ("  - Next: n/enter\n  - Prev: p\n  - +103: a\n  - -103: d\n  - Solution: s\n  - Quit: q\n> ")
		val input = Option.valOf (TextIO.inputLine TextIO.stdIn)
		val _ = print ("Input issued: " ^ input ^ "\n")
		in
			if input = "n\n" then
				loop (arr, i + 1)
			else if input = "p\n" then
				loop (arr, i - 1)
			else if input = "a\n" then
				loop (arr, i + 103)
			else if input = "d\n" then
				loop (arr, i - 103)
			else if input = "z\n" then
				loop (arr, 0)
			else if input = "s\n" then
				loop (arr, 8050)
			else if input = "\n" then
				loop (arr, i + 1)
			else if input = "q\n" then
				i
			else let
				val _ = print "Invalid action, try again.\n"
				in
					loop (arr, i)
				end
		end
	in
		loop (canvas, 0)
	end

fun main () = let
	val small_lines = read_info "small.txt"
	val large_lines = read_info "large.txt"

    val p1s = part_1 (small_lines, (11, 7), 100)
	val _ = print ("Part 1 small expected 12 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 (large_lines, (101, 103), 100)
	val _ = print ("Part 1 large expected 215987200 and got: " ^ (Int.toString p1l) ^ "\n")

    val p2l = part_2 (large_lines, (101, 103))
	val _ = print ("Part 2 large expected 8050 and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


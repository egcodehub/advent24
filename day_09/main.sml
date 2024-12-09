structure Main =
struct

type file = int * int * int
type space = int * int

fun parse_input (file_name : string) : file list * space list = let
	val line = Read.read_file file_name
	val len  = String.size line
	val c_to_i = fn c => (Char.ord c) - (Char.ord #"0")
	fun aux (i, off, files) =
		if i >= (len - 1) then
			(files, [])
		else let
			val width = c_to_i (String.sub (line, i))
			in
    			if i mod 2 = 0 then
    				aux (i + 1, off + width, (i div 2, off, width) :: files)
				else let
            		val (fls, frees) = aux (i + 1, off + width, files)
            		in
                		(fls, (off, width) :: frees)
            		end
			end
	in
    	aux (0, 0, [])
	end

(*
1. The file fits and there is space left
SOME space left
(SOME (file fit), NONE (file left))

2. The file is the size of the space.
NONE space left
NONE file left
(SOME (file fit), NONE (file left))

3. The file doesn't fit.
NONE Space left
(SOME (file fit), SOME file left)


When fl_len < sp_len:
sp_start = 4
sp_len = 5

fl_id = 40
fl_start = 48
fl_len = 3

*)

(* Space left, file fit, file left. *)
fun put_file_in_space ((fl_id, fl_start, fl_len), (sp_start, sp_len)) : space option * (file option * file option) =
	if fl_len < sp_len then
		(SOME (sp_start + fl_len, sp_len - fl_len), (SOME (fl_id, sp_start, fl_len), NONE))
	else if fl_len = sp_len then
		(NONE, (SOME (fl_id, sp_start, fl_len), NONE))
	else
		if sp_len = 0 then
			(NONE, (NONE, SOME (fl_id, fl_start, fl_len)))
		else
			(NONE, (SOME (fl_id, sp_start, sp_len), SOME (fl_id, fl_start, fl_len - sp_len)))

fun fit_files ([], spaces) =
	([], [])
  | fit_files (files, []) =
	([], files)
  | fit_files ((fl as (fl_id, fl_start, fl_len)) :: files, (sp as (sp_start, sp_len)) :: spaces) =
	if fl_start < sp_start then
    	([], fl :: files)
	else
    	case put_file_in_space (fl, sp)
    	  of (NONE, (NONE, SOME fl0)) => fit_files (fl0 :: files, spaces)
    	   | (NONE, (SOME fit_fl, NONE)) => let
    		 val (news, rest) = fit_files (files, spaces)
    		 in
        		 (fit_fl :: news, rest)
    		 end
    	   | (NONE, (SOME fit_fl, SOME rest_fl)) => let
    		 val (news, rest) = fit_files (rest_fl :: files, spaces)
    		 in
        		 (fit_fl :: news, rest)
    		 end
    	   | (SOME sp0, (SOME fit_fl, NONE)) => let
    		 val (news, rest) = fit_files (files, sp0 :: spaces)
    		 in
        		 (fit_fl :: news, rest)
    		 end
    	   | _ => raise Fail "Impossible case."

fun sum_equal_block (id, start, len) = let
	fun aux (i, sum) =
    	if i <= 0 then
    		sum
		else
    		aux (i - 1, sum + (id * (start + len - i)))
	in
		aux (len, 0)
	end

fun part_1 (files : file list, spaces : space list) = let
(*
	val _ = print ("There are " ^ (Int.toString (List.length files)) ^ " files\n")
	val _ = print ("There are " ^ (Int.toString (List.length spaces)) ^ " spaces\n")
	val f = fn (id, start, len) => "File " ^ (Int.toString id) ^ ": " ^ (Int.toString start) ^ " + " ^ (Int.toString len)
	val g = fn (start, len) => "Space: " ^ (Int.toString start) ^ " + " ^ (Int.toString len)
	val _ = print (MyList.to_string f ("All files:\n", "\n", "\n\n") files)
	val _ = print (MyList.to_string g ("All spaces:\n", "\n", "\n\n") spaces)
*)
	val (fit, rest) = fit_files (files, spaces)
(*
	val ee = fn ((_, a, b), (_, c, d)) => case Int.compare (a, c) of EQUAL => Int.compare (b, d) | l => l
	val lst = MyList.merge_sort ee (fit @ List.rev (rest))
	val _ = print "-----------------------\n"
	val _ = print (MyList.to_string f ("All files:\n", "\n", "\n\n") lst)
*)
	in
    	List.foldl (fn (x, acc) => (sum_equal_block x) + acc) 0 (fit @ rest)
	end

fun main () = let
	val s_map = parse_input "small.txt"
	val l_map = parse_input "large.txt"

    val p1s = part_1 s_map
	val _ = print ("Part 1 small expected 1928 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 l_map
	val _ = print ("Part 1 large expected 6301895872542 and got: " ^ (Int.toString p1l) ^ "\n")

(*
    val p2s = part_2 (test_2 (fn p => in_bounds (s_dims, p))) small_ant
	val _ = print ("Part 2 small expected 34 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 (test_2 (fn p => in_bounds (l_dims, p))) large_ant
	val _ = print ("Part 2 large expected 1333 and got: " ^ (Int.toString p2l) ^ "\n")
*)

	in
    	()
	end

end

(*
val _ = Main.main ()
*)


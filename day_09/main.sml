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

fun fit_blocks ([], spaces) =
	([], [])
  | fit_blocks (files, []) =
	([], files)
  | fit_blocks ((fl as (fl_id, fl_start, fl_len)) :: files, (sp as (sp_start, sp_len)) :: spaces) =
	if fl_start < sp_start then
    	([], fl :: files)
	else
    	case put_file_in_space (fl, sp)
    	  of (NONE, (NONE, SOME fl0)) => fit_blocks (fl0 :: files, spaces)
    	   | (NONE, (SOME fit_fl, NONE)) => let
    		 val (moved_files, still_files) = fit_blocks (files, spaces)
    		 in
        		 (fit_fl :: moved_files, still_files)
    		 end
    	   | (NONE, (SOME fit_fl, SOME rest_fl)) => let
    		 val (moved_files, still_files) = fit_blocks (rest_fl :: files, spaces)
    		 in
        		 (fit_fl :: moved_files, still_files)
    		 end
    	   | (SOME left_sp, (SOME fit_fl, NONE)) => let
    		 val (moved_files, still_files) = fit_blocks (files, left_sp :: spaces)
    		 in
        		 (fit_fl :: moved_files, still_files)
    		 end
    	   | _ => raise Fail "Impossible case."

fun sum_equal_block (fl_id, fl_start, fl_len) = let
	val zero = Int.toLarge 0
	val one  = Int.toLarge 1
	val id   = Int.toLarge fl_id
	val start = Int.toLarge fl_start
	val len   = Int.toLarge fl_len
	fun aux (i, sum) =
    	if i <= zero then
    		sum
		else
    		aux (i - one, sum + (id * (start + len - i)))
	in
		aux (len, zero)
	end

fun part_1 (files : file list, spaces : space list) = let
	val (fit, rest) = fit_blocks (files, spaces)
	val adder = List.foldl (fn (x, acc) => (sum_equal_block x) + acc) (Int.toLarge 0)
	in
    	(adder fit) + (adder rest)
	end

fun fit_file_in_space (_ : file, [] : space list) : (file * space list) option =
	NONE
  | fit_file_in_space (file as (_, fl_start, fl_len), (sp as (sp_start, sp_len)) :: spaces) =
	if fl_start < sp_start then
		NONE
	else
		case put_file_in_space (file, sp)
		  of (NONE, (SOME fit, NONE)) => SOME (fit, spaces)
		   | (SOME left, (SOME fit, NONE)) => SOME (fit, left :: spaces)
		   | _ => (
			 case fit_file_in_space (file, spaces)
			   of NONE => NONE
				| SOME (f, rest) => SOME (f, sp :: rest)
			 )

fun fit_files ([], _) =
	([], [])
  | fit_files (files, []) =
	([], files)
  | fit_files ((fl as (fl_id, fl_start, fl_len)) :: files, (sp as (sp_start, sp_len)) :: spaces) =
	if fl_start < sp_start then
    	([], fl :: files)
	else
		case fit_file_in_space (fl, sp :: spaces)
		  of NONE => let
			 val (moved_files, still_files) = fit_files (files, sp :: spaces)
			 in
    			 (moved_files, fl :: still_files)
			 end
		   | SOME (n_fl, n_spaces) => let
			 val (moved_files, still_files) = fit_files (files, n_spaces)
			 in
    			 (n_fl :: moved_files, still_files)
			 end

fun part_2 (files : file list, spaces : space list) = let
    val (fit, rest) = fit_files (files, spaces)
	val adder = List.foldl (fn (x, acc) => (sum_equal_block x) + acc) (Int.toLarge 0)
	in
    	(adder fit) + (adder rest)
	end

fun main () = let
	val s_map = parse_input "small.txt"
	val l_map = parse_input "large.txt"

    val p1s = part_1 s_map
	val _ = print ("Part 1 small expected 1928 and got: " ^ (LargeInt.toString p1s) ^ "\n")
    val p1l = part_1 l_map
	val _ = print ("Part 1 large expected 6301895872542 and got: " ^ (LargeInt.toString p1l) ^ "\n")

    val p2s = part_2 s_map
	val _ = print ("Part 2 small expected 2858 and got: " ^ (LargeInt.toString p2s) ^ "\n")
    val p2s = part_2 l_map
	val _ = print ("Part 2 small expected 6323761685944 and got: " ^ (LargeInt.toString p2s) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


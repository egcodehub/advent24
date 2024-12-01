structure Main =
struct

fun build_lists [] =
	([], [])
  | build_lists (x :: xs) =
	case String.tokens Char.isSpace x
	  of l :: r :: [] => let
		 val (rest_l, rest_r) = build_lists xs
		 val num_l = Option.valOf (Int.fromString l)
		 val num_r = Option.valOf (Int.fromString r)
		 in
    		 (num_l :: rest_l, num_r :: rest_r)
		 end
	   | _ => raise Fail "Lines contain more than two words"

fun part_1 file_name = let
	val lines = Read.read_lines file_name
	val (l, r) = build_lists lines
	val sort_l = Lists.merge_sort Int.compare l
	val sort_r = Lists.merge_sort Int.compare r
	val single = ListPair.map (fn (a, b) => Int.abs (a - b)) (sort_l, sort_r)
	in
		List.foldl Int.+ 0 single
	end

structure D = Dictionary (IntOrd)

fun sum_freqs (left, right) = let
	fun count_times (a, d, lst) =
		case D.find (a, d)
		  of NONE => let
			 val f = fn (n, (cnt, ns)) => if n = a then (cnt + 1, ns) else (cnt, n :: ns)
			 val (cnt, n_lst) = List.foldl f (0, []) lst
			 val n_d = D.insert ((a, cnt), d)
			 in
    			 (cnt, n_d, n_lst)
			 end
		   | SOME cnt => (cnt, d, lst)
	fun update_number (num, (acc, dict, lst)) = let
		val (count, n_dict, n_lst) = count_times (num, dict, lst)
		in
    		(acc + num * count, n_dict, n_lst)
		end
	val (sum, _, _) = List.foldl (fn x => update_number x) (0, D.empty, right) left
	in
		sum
	end

fun part_2 file_name = let
	val lines = Read.read_lines file_name
	val (l, r) = build_lists lines
	in
    	sum_freqs (l, r)
	end

fun main () = let
    val p1s = part_1 "small.txt"
	val _ = print ("Part 1 small expected 11 and got : " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 "large.txt"
	val _ = print ("Part 1 large expected 2285373 and got : " ^ (Int.toString p1l) ^ "\n")
    val p2s = part_2 "small.txt"
	val _ = print ("Part 1 small expected 31 and got : " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 "large.txt"
	val _ = print ("Part 1 large expected 21142653 and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)

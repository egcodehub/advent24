structure Main =
struct

(* REMEMBER: We use a foldl, so the lines end up reversed. *)
fun build_lists lst = let
	val f = fn (s, (left, right)) =>
		case String.tokens Char.isSpace s
		  of l :: r :: [] => let
    		 val nl = Option.valOf (Int.fromString l)
    		 val nr = Option.valOf (Int.fromString r)
    		 in
        		 (nl :: left, nr :: right)
    		 end
    	   | _ => raise Fail "Lines contain more than two words"
	in
    	List.foldl f ([], []) lst
	end

fun part_1 (l, r) = let
	val sort_l = MyList.merge_sort Int.compare l
	val sort_r = MyList.merge_sort Int.compare r
	in
		ListPair.foldl (fn (a, b, c) => c + (Int.abs (a - b))) 0 (sort_l, sort_r)
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

fun part_2 lines =
	sum_freqs lines

fun main () = let
	val small_l = build_lists (Read.read_lines "small.txt")
	val large_l = build_lists (Read.read_lines "large.txt")
    val p1s = part_1 small_l
	val _ = print ("Part 1 small expected 11 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 large_l
	val _ = print ("Part 1 large expected 2285373 and got: " ^ (Int.toString p1l) ^ "\n")
    val p2s = part_2 small_l
	val _ = print ("Part 1 small expected 31 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 large_l
	val _ = print ("Part 1 large expected 21142653 and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


structure Main =
struct

structure V = Vector

fun prepare_input (lines : string list) : (int * int) list * int V.vector list = let
	val is_bar = fn c => c = #"|"
	val is_com = fn c => c = #","
	val s_to_n = Option.valOf o Int.fromString
	fun aux [] =
		raise Fail "Input missing update lines"
	  | aux (x :: xs) =
		case String.tokens is_bar x
		  of left :: right :: [] => let
			 val (rest, updates) = aux xs
			 val num_l = s_to_n left
			 val num_r = s_to_n right
			 in
    			 ((num_l, num_r) :: rest, updates)
			 end
		   | _ => ([], List.map (fn s => V.fromList (List.map s_to_n (String.tokens is_com s))) (x :: xs))
	in
    	aux lines
	end

(* Dictionary with keys of type int. *)
structure D = Dictionary (IntOrd)
structure S = Set (IntOrd)

type info = S.set * S.set

fun add_ordering ((left, right) : int * int, scale0 : info D.dict) : info D.dict = let
	val e =	S.empty
	val scale1 =
		case D.find (left, scale0)
		  of NONE => D.insert ((left, (e, S.insert (right, e))), scale0)
		   | SOME (l, r) => D.modify ((left, (l, S.insert (right, r))), scale0)
	val scale2 =
		case D.find (right, scale1)
		  of NONE => D.insert ((right, (S.insert (left, e), e)), scale1)
		   | SOME (l, r) => D.modify ((right, (S.insert (left, l), r)), scale1)
	in
    	scale2
	end

fun build_order (pairs : (int * int) list) : info D.dict =
	List.foldl add_ordering D.empty pairs

fun is_correct_order (v : int V.vector, d : info D.dict) : bool = let
	fun searcher (v, i, is_end, cond, step) =
		if is_end i then
			true
		else let
			val b = cond (V.sub (v, i))
			in
    			b andalso (searcher (v, step i, is_end, cond, step))
			end
	fun is_at_side (d : info D.dict, elem : int, select) (x : int) : bool =
		Option.isSome (S.find (x, select (D.lookup (elem, d))))
	val is_at_left  = fn e => is_at_side (d, e, fn (l, _) => l)
	val is_at_right = fn e => is_at_side (d, e, fn (_, r) => r)
	val at_bot = fn x => x < 0
	val at_top = fn x => x >= (V.length v)
	fun apply_searcher i =
		if at_top i then
			true
		else let
			val e = V.sub (v, i)
			val l = searcher (v, i - 1, at_bot, is_at_left e, fn x => x - 1)
			val r = searcher (v, i + 1, at_top, is_at_right e, fn x => x + 1)
			in
				(l andalso r) andalso (apply_searcher (i + 1))
			end
	in
		apply_searcher 0
	end

(* DEBUGGING INFORMATION *)
fun show_dict dict = let
	fun ts (n, (l, r)) = let
		val sl = S.to_list l
		val sr = S.to_list r
		val f = MyList.to_string Int.toString ("[", ", ", "]")
		in
    		(f sl) ^ " >>>>> " ^ (Int.toString n) ^ " <<<<< " ^ (f sr) ^ "\n"
		end
	val l = MyList.to_string ts ("Summary:\n", "\n", "-------\n") (D.to_list dict)
	in
    	print l
	end

fun part_1 (dict : info D.dict, updates : (int V.vector) list) : int = let
	val f = fn (nums, acc) =>
		if is_correct_order (nums, dict) then let
			val i = (V.length nums) div 2
			val m = V.sub (nums, i)
			in
    			acc + m
			end
		else
			acc
	in
		List.foldl f 0 updates
	end

(*
fun part_2 () =
	
*)

fun main () = let
	val (small_pairs, small_upd) = prepare_input (Read.read_lines "small.txt")
	val (large_pairs, large_upd) = prepare_input (Read.read_lines "large.txt")
	val small_ord = build_order small_pairs
	val large_ord = build_order large_pairs
    val p1s = part_1 (small_ord, small_upd)
	val _ = print ("Part 1 small expected 143 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 (large_ord, large_upd)
	val _ = print ("Part 1 large expected 5588 and got: " ^ (Int.toString p1l) ^ "\n")
(*
    val p2s = part_2 small_l
	val _ = print ("Part 2 small expected 9 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 large_l
	val _ = print ("Part 2 large expected 1998 and got: " ^ (Int.toString p2l) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


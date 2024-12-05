structure Main =
struct

fun prepare_input (lines : string list) : (int * int) list * int list list = let
	val is_bar = fn c => c = #"|"
	val is_com = fn c => c = #","
	val s_to_n = Option.valOf o Int.fromString
	fun parse_rules ([], _) =
		raise Fail "Input missing update lines"
	  | parse_rules (x :: xs, (a, b)) =
		case String.tokens is_bar x
		  of left :: right :: [] => parse_rules (xs, ((s_to_n left, s_to_n right) :: a, b))
		   | _ => parse_updates (x :: xs, (a, b))
	and parse_updates ([], res) =
		res
	  | parse_updates (x :: xs, (a, b)) =
		parse_updates (xs, (a, (List.map s_to_n (String.tokens is_com x)) :: b))
	in
    	parse_rules (lines, ([], []))
	end

(*
fun prepare_input (lines : string list) : (int * int) list * int list list = let
	val is_bar = fn c => c = #"|"
	val is_com = fn c => c = #","
	val s_to_n = Option.valOf o Int.fromString
	fun parse_rules [] =
		raise Fail "Input missing update lines"
	  | parse_rules (x :: xs) =
		case String.tokens is_bar x
		  of left :: right :: [] => let
			 val (rules, ups) = parse_rules xs
			 in
    			 ((s_to_n left, s_to_n right) :: rules, ups)
			 end
		   | _ => parse_updates (x :: xs)
	and parse_updates [] =
		([], [])
	  | parse_updates (x :: xs) = let
		val nums = List.map s_to_n (String.tokens is_com x)
		val (rules, ups) = parse_updates xs
		in
    		(rules, nums :: ups)
		end
	in
    	parse_rules lines
	end
*)

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

fun is_correct_order (test : int * int -> bool) (lst : int list) : bool = let
	fun aux [] =
		true
	  | aux (x :: []) =
		true
	  | aux (x :: y :: xs) =
		(test (x, y)) andalso (aux (y :: xs))
	in
    	aux lst
	end

fun part_1 (test : int * int -> bool, updates : int list list) : int = let
	val f = fn (nums, acc) => let
		val mid   = (List.length nums) div 2
		val n_mid = List.nth (nums, mid)
		val n_acc = if is_correct_order test nums then acc + n_mid else acc
		in
    		n_acc
		end
	in
		List.foldl f 0 updates
	end

(* In d we keep symetrical information. What we know of a we also know of b. *)
fun cmp (d : info D.dict) (a : int, b : int) : General.order =
	if a = b then
		General.EQUAL
	else
    	case D.find (a, d)
    	  of NONE => raise Fail ("Undefined number: " ^ (Int.toString a))
    	   | SOME (l, r) => (
    		 case S.find (b, l)
			   of SOME x => General.GREATER
				| NONE => General.LESS
    		 )

fun lt d (a, b) =
	case cmp d (a, b)
	  of General.LESS => true
	   | _ => false

fun part_2 (cmp : int * int -> General.order, lt : int * int -> bool, updates : int list list) : int = let
	val wrong  = List.filter (fn ns => not (is_correct_order lt ns)) updates
	val sorted = List.map (MyList.merge_sort cmp) wrong
	val middle = fn xs => List.nth (xs, ((List.length xs) div 2))
	in
    	List.foldl (fn (ns, acc) => (middle ns) + acc) 0 sorted
	end

fun main () = let
	val (small_pairs, small_upd) = prepare_input (Read.read_lines "small.txt")
	val (large_pairs, large_upd) = prepare_input (Read.read_lines "large.txt")
	val small_ord = build_order small_pairs
	val large_ord = build_order large_pairs
	val ps_lt = lt small_ord
	val pl_lt = lt large_ord
	val ps_cmp = cmp small_ord
	val pl_cmp = cmp large_ord

    val p1s = part_1 (ps_lt, small_upd)
	val _ = print ("Part 1 small expected 143 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 (pl_lt, large_upd)
	val _ = print ("Part 1 large expected 5588 and got: " ^ (Int.toString p1l) ^ "\n")

    val p2s = part_2 (ps_cmp, ps_lt, small_upd)
	val _ = print ("Part 2 small expected 123 and got: " ^ (Int.toString p2s) ^ "\n")
    val p2l = part_2 (pl_cmp, pl_lt, large_upd)
	val _ = print ("Part 2 large expected 5331 and got: " ^ (Int.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


structure Main =
struct

fun build_report (s : string) : int list =
	List.map (Option.valOf o Int.fromString) (String.tokens Char.isSpace s)

datatype res =
	Good
  | One of int list
  | Two of int list * int list

fun check_report ([] : int list, d : int, t : int) : bool =
	true
  | check_report (num :: nums, d, t) = let
	fun aux ([], _, _) =
		Good
	  | aux (x :: xs, p, f) =
		if p = x then let
(*
			val _ = print ("Equal\n")
			val _ = print ("Both prev and curr are: " ^ (Int.toString p) ^ "\n")
			val _ = print ("Rest: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") xs))
*)
			in
			One xs
			end
		else if (Int.abs (p - x)) > d then let
(*
			val _ = print ("Gap Too Big\n")
			val _ = print ("Prev: " ^ (Int.toString p) ^ "\n")
			val _ = print ("Curr: " ^ (Int.toString x) ^ "\n")
			val _ = print ("Rest: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") xs))
*)
			in
			Two (x :: xs, xs)
			end
		else if f (p, x) then let
(*
			val _ = print ("Good boy\n")
			val _ = print ("Prev: " ^ (Int.toString p) ^ "\n")
			val _ = print ("Curr: " ^ (Int.toString x) ^ "\n")
			val _ = print ("Rest: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") xs))
*)
			in
			case aux (xs, x, f)
			  of Good => Good
			   | One ys => One (x :: ys)
			   | Two (ys, zs) =>
				 if (List.length ys) > (List.length zs) then let
(*
        			 val _ = print ("Combo 1: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") ys))
        			 val _ = print ("Combo 2: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") zs))
*)
					 in
    				 Two (ys, x :: zs)
					 end
				 else
    				 Two (x :: ys, x :: zs)
			end
		else let
(*
			val _ = print ("Direction change\n")
			val _ = print ("Prev: " ^ (Int.toString p) ^ "\n")
			val _ = print ("Curr: " ^ (Int.toString x) ^ "\n")
			val _ = print ("Rest: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") xs))
*)
			in
    		Two (x :: xs, xs)
			end
	val check =
		case nums
		  of [] => Good
		   | x :: xs => (
			 case aux (x :: xs, num, if num < x then Int.< else Int.>)
			   of Good => Good
				| One ys => One (num :: ys)
				| Two (ys, zs) =>
				  if (List.length ys) > (List.length zs) then
    				  Two (ys, num :: zs)
				  else
    				  Two (num :: ys, num :: zs)
			 )
	in
    	case check
		  of Good => true
		   | One xs => let
(*
			 val _ = print ("We got TWO back!\n")
			 val _ = print ("One: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") xs))
*)
			 in
			 if t <= 0 then
    			 false
			 else
    			 check_report (xs, d, t - 1)
			 end
		   | Two (xs, ys) => let
(*
			 val _ = print ("We got TWO back!\n")
			 val _ = print ("Start: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") (num :: nums)))
			 val _ = print ("First: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") xs))
			 val _ = print ("Second: " ^ (MyList.to_string Int.toString ("[", ", ", "]\n") ys))
*)
			 in
			 if t <= 0 then
    			 false
			 else
    			 check_report (xs, d, t - 1) orelse check_report (ys, d, t - 1)
			 end
	end

fun part_x (reports : int list list, tol : int) : int =
	List.length (List.filter (fn r => check_report (r, 3, tol)) reports)

fun main () = let
	val small_l = List.map build_report (Read.read_lines "small.txt")
	val large_l = List.map build_report (Read.read_lines "large.txt")
(*
    val p1s = part_x (small_l, 0)
	val _ = print ("Part 1 small expected 2 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_x (large_l, 0)
	val _ = print ("Part 1 large expected 224 and got: " ^ (Int.toString p1l) ^ "\n")
*)
    val p2s = part_x (small_l, 1)
	val _ = print ("Part 2 small expected 4 and got: " ^ (Int.toString p2s) ^ "\n")
(*
    val p2l = part_x (large_l, 1)
	val _ = print ("Part 2 large expected (>289) and got: " ^ (Int.toString p2l) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


structure Main =
struct

structure L = LargeInt

val s_to_n = Option.valOf o L.fromString

fun parse_line (s : string) : L.int * L.int =
	case String.tokens (not o Char.isDigit) s
	  of num1 :: num2 :: [] => (s_to_n num1, s_to_n num2)
	   | _ => raise Fail "Found a line that doesn't have only two numbers"

type claw_info = {
	a : L.int * L.int,
	b : L.int * L.int,
	p : L.int * L.int
}

fun parse_machine (button_a :: button_b :: prize :: [] : string list) : claw_info = {
		a = parse_line button_a,
		b = parse_line button_b,
		p = parse_line prize
	}
  | parse_machine _ =
	raise Fail "Bad input length"

fun parse_machines (file_name : string) : claw_info list = let
	fun aux [] =
		[]
	  | aux xs = let
		val (three, rest) = MyList.split (3, xs)
		in
    		(parse_machine three) :: (aux rest)
		end
	in
		aux (Read.read_lines file_name)
	end

structure CO : COEFICIENT = Coeficient (struct structure I = L end)
structure E = Equation2d (struct structure C = CO structure I = L end)

fun find_cost ({a = (ax, ay), b = (bx, by), p = (px, py)} : claw_info, factor) : L.int = let
	val eq1 = E.new (CO.new (ax, NONE), CO.new (bx, NONE), CO.new (px + factor, NONE))
	val eq2 = E.new (CO.new (ay, NONE), CO.new (by, NONE), CO.new (py + factor, NONE))
	in
		case E.solve (eq1, eq2)
		  of NONE => (Int.toLarge 0)
		   | SOME (x, y) =>
			 if CO.is_full x andalso CO.is_full y then let
    			 val three = Int.toLarge 3
				 val one = Int.toLarge 1
				 val res_x = CO.get_int x
				 val res_y = CO.get_int y
				 in
    				 (three * res_x) + (one * res_y)
				 end
			 else
				 (Int.toLarge 0)
	end

fun part_x (infos : claw_info list, factor : L.int) : L.int =
	List.foldl (fn (i, acc) => (find_cost (i, factor)) + acc) (Int.toLarge 0) infos

fun main () = let
	val s_machine = parse_machines "small.txt"
	val l_machine = parse_machines "large.txt"

    val p1s = part_x (s_machine, Int.toLarge 0)
	val _ = print ("Part 1 small expected 480 and got: " ^ (L.toString p1s) ^ "\n")
    val p1l = part_x (l_machine, Int.toLarge 0)
	val _ = print ("Part 1 large expected 31623 and got: " ^ (L.toString p1l) ^ "\n")

    val p2l = part_x (l_machine, (Option.valOf o L.fromString) "10000000000000")
	val _ = print ("Part 2 large expected 93209116744825 and got: " ^ (L.toString p2l) ^ "\n")
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


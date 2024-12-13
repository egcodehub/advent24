signature COEFICIENT =
sig
	structure I : INTEGER
	type coef
	val eq : coef * coef -> bool
	val new : I.int * I.int option -> coef
	val add : coef * coef -> coef
	val sub : coef * coef -> coef
	val prod : coef * coef -> coef
	val quot : coef * coef -> coef
	val scale : I.int * coef -> coef
	val from_int : int -> coef
	val is_full : coef -> bool
	val get_int : coef -> I.int
	val to_string : coef -> string
end

functor Coeficient (I : INTEGER) : COEFICIENT =
struct

structure I = I

datatype coef =
	Full of I.int
  | Frac of I.int * I.int

fun to_string (Full x) =
	I.toString x
  | to_string (Frac (x, y)) =
	(I.toString x) ^ "/" ^ (I.toString y)

val zero = I.fromInt 0
val one = I.fromInt 1

fun gcd (a, b) =
	if b = zero then
    	I.abs a
	else
    	gcd (b, I.mod (a, b))

fun simplify (Full x) =
	Full x
  | simplify (Frac (x, y)) = let
	val common = gcd (x, y)
	val xd = I.div (x, common)
	val yd = I.div (y, common)
	in
		if xd = zero then
			Full zero
		else if yd = one then
			Full xd
		else
        	Frac (if I.sameSign (xd, yd) then xd else I.~ (I.abs xd), I.abs yd)
	end

fun new (x, NONE) =
	Full x
  | new (x, SOME y) =
	simplify (Frac (x, y))

fun eq (Full x1, Full x2) =
	x1 = x2
  | eq (Frac (x1, y1), Frac (x2, y2)) =
	(x1 = x2) andalso (y1 = y2)
  | eq _ =
	false

(*
fun cmp (c1, c2) =
	if (eq (c1, c2)) then
		General.EQUAL
	else let
	val r1 = to_real c1
	val r2 = to_real c2
	in
    	if r1 > r2 then
			General.GREATER
		else
			General.LESS
	end
*)

fun as_aux f (Full x1, Full x2) =
	as_aux f (Frac (x1, one), Frac (x2, one))
  | as_aux f (Full x1, Frac p2) =
	as_aux f (Frac (x1, one), Frac p2)
  | as_aux f (Frac p1, Full x2) =
	as_aux f (Frac p1, Frac (x2, one))
  | as_aux f (Frac (x1, y1), Frac (x2, y2)) =
	simplify (Frac (f (I.* (x1, y2), I.* (x2, y1)), I.* (y1, y2)))

fun add (c1, c2) =
	as_aux (op I.+) (c1, c2)

fun sub (c1, c2) =
	as_aux (op I.-) (c1, c2)

fun scale (n, Full x) =
	Full (I.* (n, x))
  | scale (n, Frac (x, y)) =
	simplify (Frac (I.* (x, n), y))

fun prod (Full x1, Full x2) =
	Full (I.* (x1, x2))
  | prod (Full x1, Frac p2) =
	prod (Frac (x1, one), Frac p2)
  | prod (Frac p1, Full x2) =
	prod (Frac p1, Frac (x2, one))
  | prod (Frac (x1, y1), Frac (x2, y2)) =
	simplify (Frac (I.* (x1, x2), I.* (y1, y2)))

fun quot (Full x1, Full x2) =
	quot (Frac (x1, one), Frac (x2, one))
  | quot (Full x1, Frac p2) =
	quot (Frac (x1, one), Frac p2)
  | quot (Frac p1, Full x2) =
	quot (Frac p1, Frac (x2, one))
  | quot (Frac (x1, y1), Frac (x2, y2)) =
	simplify (Frac (I.* (x1, y2), I.* (y1, x2)))

fun from_int i =
	new (I.fromInt i, NONE)

fun same_int (i, Full x1) =
	i = x1
  | same_int _ =
	false

fun is_full (Full _) =
	true
  | is_full _ =
	false

fun get_int (Full x) =
	x
  | get_int _ =
	raise Fail "Not a full number"

end

functor Equation2d (C : COEFICIENT) =
struct

type eq2d = C.coef * C.coef * C.coef

fun new (c1, c2, c3) =
	(c1, c2, c3)

fun from_ints (a, b, c) =
	(C.new (a, NONE), C.new (b, NONE), C.new (c, NONE))

fun as_aux f ((a1, a2, a3), (b1, b2, b3)) =
	(f (a1, b1), f (a2, b2), f (a3, b3))

fun add (eq1, eq2) =
	as_aux C.add (eq1, eq2)

fun sub (eq1, eq2) =
	as_aux C.sub (eq1, eq2)

fun scale (n, (c1, c2, c3)) =
	(C.prod (n, c1), C.prod (n, c2), C.prod (n, c3))

val zero = C.from_int 0
val one = C.from_int 1
val two = C.from_int 2

fun is_invalid (c1, c2, c3) =
	C.eq (zero, c1) andalso C.eq (zero, c2) andalso not (C.eq (zero, c3))

fun is_infinite (c1, c2, c3) =
	C.eq (zero, c1) andalso C.eq (zero, c2) andalso C.eq (zero, c3)

fun to_string (c1, c2, c3) =
	(C.to_string c1) ^ " " ^ (C.to_string c2) ^ " | " ^ (C.to_string c3)

fun solve (eq1 as (a1, a2, a3), eq2 as (b1, b2, b3)) : (C.coef * C.coef) option =
(*
	let
	val _ = print "Solving iteration:\n"
	val _ = print ("Eq1: " ^ (to_string eq1) ^ "\n")
	val _ = print ("Eq2: " ^ (to_string eq2) ^ "\n")
	val _ = print "\n"
	in
*)
	if is_infinite eq1 orelse is_infinite eq2 then
		raise Fail "Infinite solutions"
	else if is_invalid eq1 orelse is_invalid eq2 then
		NONE
	else if C.eq (zero, a1) andalso not (C.eq (zero, b1)) then
		solve (eq2, eq1)
	else if not (C.eq (zero, b1)) then
(*
		let
		val e = C.quot (b1, a1)
		val es = scale (e, eq1)
		val _ = print ("R1 factor: " ^ (C.to_string e) ^ "\n")
		val _ = print ("R1 scaled: " ^ (to_string es) ^ "\n")
		val _ = print ("R2 subtracted: " ^ (to_string (sub (eq2, es))) ^ "\n")
		in
    		solve (eq1, sub (eq2, scale (C.quot (b1, a1), eq1)))
		end
*)
		solve (eq1, sub (eq2, scale (C.quot (b1, a1), eq1)))
	else if not (C.eq (zero, a2)) then
		solve (sub (eq1, scale (C.quot (a2, b2), eq2)), eq2)
	else if not (C.eq (one, b2)) then
		solve (eq1, scale (C.quot (one, b2), eq2))
	else if not (C.eq (two, a1)) then
		solve (scale (C.quot (one, a1), eq1), eq2)
	else
		SOME (a3, b3)
(*
	end
*)

end

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

structure CO = Coeficient (L)
structure E = Equation2d (CO)

fun smallest ({a = (ax, ay), b = (bx, by), p = (px, py)} : claw_info) : L.int = let
	val eq1 = E.new (CO.new (ax, NONE), CO.new (bx, NONE), CO.new (px, NONE))
	val eq2 = E.new (CO.new (ay, NONE), CO.new (by, NONE), CO.new (py, NONE))
	in
		case E.solve (eq1, eq2)
		  of NONE => (Int.toLarge 0)
		   | SOME (x, y) =>
			 if CO.is_full x andalso CO.is_full y then let
				 val three = Int.toLarge 3
				 val one = Int.toLarge 1
(*
    			 val _ = print ("Eq1: " ^ (E.to_string eq1) ^ "\n")
    			 val _ = print ("Eq2: " ^ (E.to_string eq2) ^ "\n")
    			 val bb = (py*ax-px*ay) div (by*ax-bx*ay)
    			 val aa = (px-bb*bx) div ax
				 val _ = print ("My x: " ^ (Int.toString res_x) ^ "\n")
				 val _ = print ("My y: " ^ (Int.toString res_y) ^ "\n")
				 val _ = print ("aa: " ^ (Int.toString aa) ^ "\n")
				 val _ = print ("bb: " ^ (Int.toString bb) ^ "\n")
				 val _ = print "\n"
				 val _ = if res_x <> aa orelse res_y <> bb then raise Fail "" else ()
*)
				 val res_x = CO.get_int x
				 val res_y = CO.get_int y
				in
				 (three * res_x) + (one * res_y)
				end
			 else
				 0
	end

fun part_x (infos : claw_info list, factor : L.int) : L.int =
	List.foldl (fn (i, acc) => (smallest i) + acc) (Int.toLarge 0) infos

fun main () = let
	val s_line = parse_machines "small.txt"
	val l_line = parse_machines "large.txt"

    val p1s = part_x (s_line, Int.toLarge 0)
	val _ = print ("Part 1 small expected 480 and got: " ^ (L.toString p1s) ^ "\n")
    val p1l = part_x (l_line, Int.toLarge 0)
	val _ = print ("Part 1 large expected 31623 and got: " ^ (L.toString p1l) ^ "\n")

(*
    val p2l = part_1 l_line
	val _ = print ("Part 2 large expected 261936432123724 and got: " ^ (Int.toString p2l) ^ "\n")
*)
	in
    	()
	end

end

(*
val _ = Main.main ()
*)


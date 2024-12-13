signature COEFICIENT =
sig
	type coef
	val new : int * int option -> coef
	val add : coef * coef -> coef
	val sub : coef * coef -> coef
	val prod : coef * coef -> coef
	val quot : coef * coef -> coef
	val scale : int * coef -> coef
	val same_int : int * coef -> bool
	val is_full : coef -> bool
	val get_int : coef -> int
	val to_string : coef -> string
end

structure Coeficient : COEFICIENT =
struct

datatype coef =
	Full of int
  | Frac of int * int

fun new (x, NONE) =
	Full x
  | new (x, SOME y) =
	Frac (x, y)

fun to_real (Full x) : real =
	Real.fromInt x
  | to_real (Frac (x, y)) : real =
	(Real.fromInt x) / (Real.fromInt y)

fun eq (Full x1, Full x2) =
	x1 = x2
  | eq (Frac (x1, y1), Frac (x2, y2)) =
	(x1 = x2) andalso (y1 = y2)
  | eq _ =
	false

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

fun gcd (a, 0) =
	Int.abs a
  | gcd (a, b) =
	gcd (b, a mod b)

fun simplify (Full x) =
	Full x
  | simplify (Frac (x, y)) = let
	val common = gcd (x, y)
	val xd = x div common
	val yd = y div common
	in
		if xd = 0 then
			Full 0
		else if yd = 1 then
			Full xd
		else
        	Frac (if xd div yd < 0 then ~ xd else Int.abs xd, Int.abs yd)
	end

fun to_frac (Full x) =
	Frac (x, 1)
  | to_frac fr =
	fr

fun as_aux f (Full x1, Full x2) =
	as_aux f (Frac (x1, 1), Frac (x2, 1))
  | as_aux f (Full x1, Frac p2) =
	as_aux f (Frac (x1, 1), Frac p2)
  | as_aux f (Frac p1, Full x2) =
	as_aux f (Frac p1, Frac (x2, 1))
  | as_aux f (Frac (x1, y1), Frac (x2, y2)) =
	simplify (Frac (f (x1 * y2, x2 * y1), y1 * y2))

fun add (c1, c2) =
	as_aux (op +) (c1, c2)

fun sub (c1, c2) =
	as_aux (op -) (c1, c2)

fun scale (n, Full x) =
	Full (n * x)
  | scale (n, Frac (x, y)) =
	simplify (Frac (x * n, y))

fun prod (Full x1, Full x2) =
	Full (x1 * x2)
  | prod (Full x1, Frac p2) =
	prod (Frac (x1, 1), Frac p2)
  | prod (Frac p1, Full x2) =
	prod (Frac p1, Frac (x2, 1))
  | prod (Frac (x1, y1), Frac (x2, y2)) =
	simplify (Frac (x1 * x2, y1 * y2))

fun quot (Full x1, Full x2) =
	quot (Frac (x1, 1), Frac (x2, 1))
  | quot (Full x1, Frac p2) =
	quot (Frac (x1, 1), Frac p2)
  | quot (Frac p1, Full x2) =
	quot (Frac p1, Frac (x2, 1))
  | quot (Frac (x1, y1), Frac (x2, y2)) =
	simplify (Frac (x1 * y2, y1 * x2))

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

fun to_string (Full x) =
	Int.toString x
  | to_string (Frac (x, y)) =
	(Int.toString x) ^ "/" ^ (Int.toString y)

end

structure Equation2d =
struct

structure C = Coeficient

type eq2d = C.coef * C.coef * C.coef

fun new (c1, c2, c3) =
	(c1, c2, c3)

fun as_aux f ((a1, a2, a3), (b1, b2, b3)) =
	(f (a1, b1), f (a2, b2), f (a3, b3))

fun add (eq1, eq2) =
	as_aux C.add (eq1, eq2)

fun sub (eq1, eq2) =
	as_aux C.sub (eq1, eq2)

fun scale (n, (c1, c2, c3)) =
	(C.prod (n, c1), C.prod (n, c2), C.prod (n, c3))

fun get_x (c1, _, _) =
	c1

fun get_y (_, c2, _) =
	c2

fun get_indep (_, _, c3) =
	c3

fun is_invalid (c1, c2, c3) =
	C.same_int (0, c1) andalso C.same_int (0, c2) andalso not (C.same_int (0, c3))

fun is_infinite (c1, c2, c3) =
	C.same_int (0, c1) andalso C.same_int (0, c2) andalso C.same_int (0, c3)

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
	else if not (C.same_int (0, b1)) then
		solve (eq1, sub (eq2, scale (C.quot (b1, a1), eq1)))
	else if not (C.same_int (0, a2)) then
		solve (sub (eq1, scale (C.quot (a2, b2), eq2)), eq2)
	else if not (C.same_int (1, b2)) then
		solve (eq1, scale (C.quot (C.new (1, NONE), b2), eq2))
	else if not (C.same_int (1, a1)) then
		solve (scale (C.quot (C.new (1, NONE), a1), eq1), eq2)
	else
		SOME (a3, b3)
(*
	end
*)

end

structure Main =
struct

val s_to_n = Option.valOf o Int.fromString

fun parse_line (s : string) : int * int =
	case String.tokens (not o Char.isDigit) s
	  of num1 :: num2 :: [] => (s_to_n num1, s_to_n num2)
	   | _ => raise Fail "Found a line that doesn't have only two numbers"

type claw_info = {
	a : int * int,
	b : int * int,
	p : int * int
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

structure C = Coeficient
structure E = Equation2d

fun smallest ({a = (ax, ay), b = (bx, by), p = (px, py)} : claw_info) : int = let
	val eq1 = E.new (C.new (ax, NONE), C.new (bx, NONE), C.new (px, NONE))
	val eq2 = E.new (C.new (ay, NONE), C.new (by, NONE), C.new (py, NONE))
	in
		case E.solve (eq1, eq2)
		  of NONE => 0
		   | SOME (x, y) => let
(*
			 val _ = print ("X=" ^ (C.to_string x) ^ ", Y=" ^ (C.to_string y) ^ "\n")
			 val _ = print ()
*)
			 in
    			 if C.is_full x andalso C.is_full y then
    				 (3 * (C.get_int x)) + (1 * (C.get_int y))
				 else
    				 0
			 end
	end

fun part_1 (infos : claw_info list) : int =
	List.foldl (fn (i, acc) => (smallest i) + acc) 0 infos

fun main () = let
	val s_line = parse_machines "small.txt"
	val l_line = parse_machines "large.txt"
(*
*)

    val p1s = part_1 s_line
	val _ = print ("Part 1 small expected 480 and got: " ^ (Int.toString p1s) ^ "\n")
    val p1l = part_1 l_line
	val _ = print ("Part 1 large expected <35595 and got: " ^ (Int.toString p1l) ^ "\n")
                                       (* >30337 *)

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


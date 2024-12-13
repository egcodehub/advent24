signature COEFICIENT =
sig
	type coef
	val eq : coef * coef -> bool
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

fun new (x, NONE) =
	Full x
  | new (x, SOME y) =
	simplify (Frac (x, y))

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
	let
	val _ = print "Solving iteration:\n"
	val _ = print ("Eq1: " ^ (to_string eq1) ^ "\n")
	val _ = print ("Eq2: " ^ (to_string eq2) ^ "\n")
	val _ = print "\n"
	in
	if is_infinite eq1 orelse is_infinite eq2 then
		raise Fail "Infinite solutions"
	else if is_invalid eq1 orelse is_invalid eq2 then
		NONE
	else if C.same_int (0, a1) andalso not (C.same_int (0, b1)) then
		solve (eq2, eq1)
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
	end

end

structure C = Coeficient
structure E = Equation2d

val t01 = E.from_ints (2, 3, 6)
val t02 = E.from_ints (1, ~1, 1)
val s01 =
	case E.solve (t01, t02)
	  of NONE => false
	   | SOME (s01a, s01b) => C.eq (s01a, C.new (9, SOME 5)) andalso C.eq (s01b, C.new (4, SOME 5))

val t03 = E.from_ints (1, 2, 5)
val t04 = E.from_ints (3, ~1, 4)
val s02 =
	case E.solve (t03, t04)
	  of NONE => false
	   | SOME (s02a, s02b) => C.eq (s02a, C.new (13, SOME 7)) andalso C.eq (s02b, C.new (11, SOME 7))

val t05 = E.from_ints (2, 3, 6)
val t06 = E.from_ints (2, 3, 12)
val s03 =
	case E.solve (t05, t06)
	  of NONE => true
	   | SOME _ => false

val t07 = E.from_ints (4, 2, 8)
val t08 = E.from_ints (2, 1, 4)
val s04 = (
	case E.solve (t07, t08)
	  of NONE => false
	   | SOME _ => false
	) handle _ => true

val t09 = E.from_ints (3, 5, 7)
val t10 = E.from_ints (3, 5, 9)
val s05 =
	case E.solve (t09, t10)
	  of NONE => true
	   | SOME _ => false

val t11 = E.from_ints (0, 2, 4)
val t12 = E.from_ints (3, 0, 6)
val s06 =
	case E.solve (t11, t12)
	  of NONE => false
	   | SOME (s06a, s06b) => C.eq (s06a, C.new (2, NONE)) andalso C.eq (s06b, C.new (2, NONE))

val t13 = E.from_ints (0, 0, 0)
val t14 = E.from_ints (1, 1, 1)
val s07 = (
	case E.solve (t13, t14)
	  of NONE => false
	   | SOME _ => false
	) handle _ => true

val t15 = E.from_ints (1, 1, 3)
val t16 = E.from_ints (2, 2, 6)
val s08 = (
	case E.solve (t15, t16)
	  of NONE => false
	   | SOME _ => false
	) handle _ => true

val result = s01 andalso s02 andalso s03 andalso s04 andalso s05 andalso s06 andalso s07
			 andalso s08


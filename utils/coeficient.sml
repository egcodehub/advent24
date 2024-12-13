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

functor Coeficient (Arg : sig structure I : INTEGER end) : COEFICIENT =
struct

structure I = Arg.I

datatype coef =
	Full of I.int
  | Frac of I.int * I.int

fun to_string (Full x) =
	I.toString x
  | to_string (Frac (x, y)) =
	(I.toString x) ^ "/" ^ (I.toString y)

val zero = I.fromInt 0
val one = I.fromInt 1

fun int_eq (a, b) =
	(I.compare (a, b)) = General.EQUAL

fun gcd (a, b) =
	if int_eq (b, zero) then
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
		if int_eq (xd, zero) then
			Full zero
		else if int_eq (yd, one) then
			Full xd
		else
        	Frac (if I.sameSign (xd, yd) then xd else I.~ (I.abs xd), I.abs yd)
	end

fun new (x, NONE) =
	Full x
  | new (x, SOME y) =
	simplify (Frac (x, y))

fun eq (Full x1, Full x2) =
	int_eq (x1, x2)
  | eq (Frac (x1, y1), Frac (x2, y2)) =
	(int_eq (x1, x2)) andalso (int_eq (y1, y2))
  | eq _ =
	false

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
	int_eq (i, x1)
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


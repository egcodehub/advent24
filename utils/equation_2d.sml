functor Equation2d (Arg : sig structure C : COEFICIENT structure I : INTEGER end) :
sig
	structure C : COEFICIENT
	type eq2d
	val new : C.coef * C.coef * C.coef -> eq2d
	val solve : eq2d * eq2d -> (C.coef * C.coef) option
end =
struct

structure C = Arg.C
structure I = Arg.I

type eq2d = C.coef * C.coef * C.coef

fun new (c1, c2, c3) =
	(c1, c2, c3)

(*
fun from_ints (a, b, c) =
	(C.new (a, NONE), C.new (b, NONE), C.new (c, NONE))
*)

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
	if is_infinite eq1 orelse is_infinite eq2 then
		raise Fail "Infinite solutions"
	else if is_invalid eq1 orelse is_invalid eq2 then
		NONE
	else if C.eq (zero, a1) andalso not (C.eq (zero, b1)) then
		solve (eq2, eq1)
	else if not (C.eq (zero, b1)) then
		solve (eq1, sub (eq2, scale (C.quot (b1, a1), eq1)))
	else if not (C.eq (zero, a2)) then
		solve (sub (eq1, scale (C.quot (a2, b2), eq2)), eq2)
	else if not (C.eq (one, b2)) then
		solve (eq1, scale (C.quot (one, b2), eq2))
	else if not (C.eq (one, a1)) then
		solve (scale (C.quot (one, a1), eq1), eq2)
	else
		SOME (a3, b3)

end


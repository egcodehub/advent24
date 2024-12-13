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

val result =
	s01 andalso
	s02 andalso
	s03 andalso
	s04 andalso
	s05 andalso
	s06 andalso
	s07 andalso
	s08


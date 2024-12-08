structure CoordOrd : ORD_KEY =
struct

type t = int * int

fun cmp ((x1, y1), (x2, y2)) =
	case Int.compare (x1, x2)
	  of General.EQUAL => Int.compare (y1, y2)
	   | result => result

fun eq (p1 : t, p2 : t) : bool =
	(cmp (p1, p2)) = General.EQUAL

end

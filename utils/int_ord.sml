structure IntOrd : ORD_KEY =
struct

type t = int

val cmp = Int.compare

fun eq (i1 : int, i2 : int) : bool =
	(Int.compare (i1, i2)) = General.EQUAL

end

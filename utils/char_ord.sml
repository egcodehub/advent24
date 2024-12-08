structure CharOrd : ORD_KEY =
struct

type t = char

val cmp = Char.compare

fun eq (c1 : t, c2 : t) : bool =
	(Char.compare (c1, c2)) = General.EQUAL

end

signature MY_STR =
sig

	exception Empty
	val first : string -> char * string
	val last : string -> char * string

end

structure MyStrImpl =
struct

fun first "" =
	raise Empty
  | first s =
	(String.sub (s, 0), String.extract (s, 1, NONE))

fun last "" =
	raise Empty
  | last s = let
	val len = String.size s
	val lst = String.sub (s, len - 1)
	val frt = String.extract (s, 0, SOME (len - 1))
	in
    	(lst, frt)
	end

end

structure MyStr : MY_STR = MyStrImpl


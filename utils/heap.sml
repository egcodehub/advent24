functor Heap (Key : ORD_KEY) : HEAP =
struct

type elem = Key.t

datatype heap =
	Empty
  | Node of heap * elem * int * heap

val empty = Empty

fun is_empty Empty =
	true
  | is_empty _ =
	false

fun rank Empty =
	0
  | rank (Node (_, _, r, _)) =
	r

fun make (l, x, r) =
	if rank l >= rank r then
    	Node (l, x, (rank r) + 1, r)
	else
    	Node (r, x, (rank l) + 1, l)

fun merge (h, Empty) =
	h
  | merge (Empty, h) =
	h
  | merge (h1 as Node (l1, x1, _, r1), h2 as Node (l2, x2, _, r2)) =
	if (Key.cmp (x1, x2)) <> General.GREATER then
    	make (l1, x1, merge (r1, h2))
	else
    	make (l2, x2, merge (h1, r2))

fun member (_, Empty) =
	false
  | member (a, Node (l, x, _, r)) =
	(Key.cmp (a, x) = General.EQUAL) orelse (member (a, l)) orelse (member (a, r))

fun peek Empty =
	NONE
  | peek (Node (_, x, _, _)) =
	SOME x

fun push (x, h) =
	if member (x, h) then
		h
	else
		merge (Node (Empty, x, 1, Empty), h)

fun pop Empty =
	NONE
  | pop (Node (l, x, _, r)) =
	SOME (x, merge (l, r))

fun from_list lst =
	List.foldl push empty lst

fun to_list h1 =
	case pop h1
	  of NONE => []
	   | SOME (x, h2) => x :: (to_list h2)

end


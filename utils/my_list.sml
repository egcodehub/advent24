signature MY_LIST =
sig

	val split : int * 'a list -> 'a list * 'a list
	val merge_sort : ('a * 'a -> General.order) -> 'a list -> 'a list
	val to_string : ('a -> string) -> (string * string * string) -> 'a list -> string

end

structure MyListImpl =
struct

fun split (n, lst) = let
	fun aux (i, xs) =
		if i = 0 then
			([], xs)
		else
			case xs
			  of [] => raise Fail "Out of bounds"
			   | x :: xs => let
				 val (l, r) = aux (i - 1, xs)
				 in
    				 (x :: l, r)
				 end
	in
    	if n > (List.length lst) then
        	raise Fail "Index too big"
    	else if n < 0 then
        	raise Fail "Index too small"
    	else
			aux (n, lst)
	end

fun merge _ ([], ys) =
	ys
  | merge _ (xs, []) =
	xs
  | merge test (x ::xs, y :: ys) =
	case test (x, y)
	  of General.LESS => x :: (merge test (xs, y :: ys))
	   | _ => y :: (merge test (x :: xs, ys))

fun merge_sort _ [] =
	[]
  | merge_sort test (x :: []) =
	[x]
  | merge_sort test (xs) = let
	val (left, right) = split ((List.length xs) div 2, xs)
	in
    	merge test (merge_sort test left, merge_sort test right)
	end

fun to_string to_s (a, b, c) lst = let
	fun aux [] =
		c
	  | aux (x :: []) =
		(to_s x) ^ c
	  | aux (x :: xs) =
		(to_s x) ^ b ^ (aux xs)
	in
    	a ^ (aux lst)
	end

end

structure MyList : MY_LIST = MyListImpl


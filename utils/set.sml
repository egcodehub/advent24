functor Set (Key : ORD_KEY) : SET =
struct

type elem = Key.t

datatype set =
	Empty
  | Node of set * elem * int * set

exception Set

val empty = Empty

fun size Empty =
	0
  | size (Node (_, _, s, _)) =
	s

fun insert (v : elem, Empty : set) : set =
	Node (Empty, v, 1, Empty)
  | insert (v, t as Node (left, v0, _, right)) =
	case Key.cmp (v, v0)
	  of General.LESS => let
		 val left0 = insert (v, left)
		 val s0 = (size left0) + (size right) + 1
		 in
    		 Node (left0, v0, s0, right)
		 end
	   | General.GREATER => let
		 val right0 = insert (v, right)
		 val s0 = (size left) + (size right0) + 1
		 in
    		 Node (left, v0, s0, right0)
		 end
	   | General.EQUAL => t

fun lookup (v : elem, Empty : set) : elem =
	raise Set
  | lookup (v, Node (left, v0, _, right)) =
	case Key.cmp (v, v0)
	  of General.LESS => lookup (v, left)
	   | General.GREATER => lookup (v, right)
	   | General.EQUAL => v0

fun find (v : elem, Empty : set) : elem option =
	NONE
  | find (v, Node (left, v0, _, right)) =
	case Key.cmp (v, v0)
	  of General.LESS => find (v, left)
	   | General.GREATER => find (v, right)
	   | General.EQUAL => SOME v0

fun modify (old : elem, new : elem, Empty : set) : set =
	Empty
  | modify (old, new, Node (left, current, s, right)) =
	case Key.cmp (old, current)
	  of General.LESS => Node (modify (old, new, left), current, s, right)
	   | General.GREATER => Node (left, current, s, modify (old, new, right))
	   | General.EQUAL => Node (left, new, s, right)

fun remove_min Empty =
	raise Set
  | remove_min (Node (Empty, v0, s, right)) =
	(right, v0)
  | remove_min (Node (left, v0, _, right)) = let
	val (left0, v1) = remove_min left
	val s0 = (size left0) + (size right) + 1
	in
    	(Node (left0, v0, s0, right), v1)
	end

fun remove_max Empty =
	raise Set
  | remove_max (Node (left, v0, s, Empty)) =
	(left, v0)
  | remove_max (Node (left, v0, _, right)) = let
	val (right0, v1) = remove_max right
	val s0 = (size left) + (size right0) + 1
	in
    	(Node (left, v0, s0, right0), v1)
	end

fun delete Empty =
	raise Set
  | delete (Node (Empty, _, _, Empty)) =
	Empty
  | delete (Node (left, _, _, Empty)) =
	left
  | delete (Node (Empty, _, _, right)) =
	right
  | delete (Node (left, _, _, right)) =
	if (size left) > (size right) then let
		(* Remove the max. *)
    	val (left0, v1) = remove_max left
		val s0 = (size left0) + (size right) + 1
    	in
        	Node (left0, v1, s0, right)
    	end
	else let
		(* Remove the min. *)
    	val (right0, v1) = remove_min right
		val s0 = (size left) + (size right0) + 1
    	in
        	Node (left, v1, s0, right0)
    	end

fun remove (v : elem, Empty : set) : set =
	raise Set
  | remove (v, t as Node (left, v0, _, right)) =
	case Key.cmp (v, v0)
	  of General.LESS => let
		 val left0 = remove (v, left)
		 val s0 = (size left0) + (size right) + 1
		 in
    		 Node (left0, v0, s0, right)
		 end
	   | General.GREATER => let
		 val right0 = remove (v, right)
		 val s0 = (size left) + (size right0) + 1
		 in
    		 Node (left, v0, s0, right0)
		 end
	   | General.EQUAL => delete t

fun from_list lst = let
	fun aux [] =
    	Empty
      | aux (x :: xs) =
    	insert (x, aux xs)
	in
		aux (List.rev lst)
	end

fun to_list Empty =
	[]
  | to_list (Node (left, p0, _, right)) = let
	val ls = to_list left
	val rs = to_list right
	in
		ls @ [p0] @ rs
	end

end


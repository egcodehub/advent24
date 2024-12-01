functor Dictionary (Key : ORD_KEY) : DICTIONARY =
struct

type key = Key.t

datatype 'a dict =
	Empty
  | Node of 'a dict * (key * 'a) * int * 'a dict

exception Dict

val empty = Empty

fun is_empty Empty =
	true
  | is_empty _ =
	false

fun size Empty =
	0
  | size (Node (_, _, s, _)) =
	s

fun insert (p : key * 'a, Empty : 'a dict) : 'a dict =
	Node (Empty, p, 1, Empty)
  | insert (p as (k, _), t as Node (left, p0 as (k0, _), _, right)) =
	case Key.cmp (k, k0)
	  of General.LESS => let
		 val left0 = insert (p, left)
		 val s0 = (size left0) + (size right) + 1
		 in
    		 Node (left0, p0, s0, right)
		 end
	   | General.GREATER => let
		 val right0 = insert (p, right)
		 val s0 = (size left) + (size right0) + 1
		 in
    		 Node (left, p0, s0, right0)
		 end
	   | General.EQUAL => t

fun lookup (k : key, Empty : 'a dict) : 'a =
	raise Dict
  | lookup (k, Node (left, (k0, v0), _, right)) =
	case Key.cmp (k, k0)
	  of General.LESS => lookup (k, left)
	   | General.GREATER => lookup (k, right)
	   | General.EQUAL => v0

fun find (k : key, Empty : 'a dict) : 'a option =
	NONE
  | find (k, Node (left, (k0, v0), _, right)) =
	case Key.cmp (k, k0)
	  of General.LESS => find (k, left)
	   | General.GREATER => find (k, right)
	   | General.EQUAL => SOME v0

fun member (k : key, Empty : 'a dict) : bool =
	false
  | member (k, Node (left, (k0, v0), _, right)) =
	case Key.cmp (k, k0)
	  of General.LESS => member (k, left)
	   | General.GREATER => member (k, right)
	   | General.EQUAL => true

fun modify ((k, v) : key * 'a, Empty : 'a dict) : 'a dict =
	Empty
  | modify (p as (k, v), Node (left, p0 as (k0, v0), s, right)) =
	case Key.cmp (k, k0)
	  of General.LESS => Node (modify (p, left), p0, s, right)
	   | General.GREATER => Node (left, p0, s, modify (p, right))
	   | General.EQUAL => Node (left, (k0, v), s, right)

fun remove_min Empty =
	raise Dict
  | remove_min (Node (Empty, p, s, right)) =
	(right, p)
  | remove_min (Node (left, p, _, right)) = let
	val (left0, p0) = remove_min left
	val s0 = (size left0) + (size right) + 1
	in
    	(Node (left0, p, s0, right), p0)
	end

fun remove_max Empty =
	raise Dict
  | remove_max (Node (left, p, s, Empty)) =
	(left, p)
  | remove_max (Node (left, p, _, right)) = let
	val (right0, p0) = remove_max right
	val s0 = (size left) + (size right0) + 1
	in
    	(Node (left, p, s0, right0), p0)
	end

fun delete Empty =
	raise Dict
  | delete (Node (Empty, _, _, Empty)) =
	Empty
  | delete (Node (left, _, _, Empty)) =
	left
  | delete (Node (Empty, _, _, right)) =
	right
  | delete (Node (left, _, _, right)) =
	if (size left) > (size right) then let
		(* Remove the max. *)
    	val (left0, p0) = remove_max left
		val s0 = (size left0) + (size right) + 1
    	in
        	Node (left0, p0, s0, right)
    	end
	else let
		(* Remove the min. *)
    	val (right0, p0) = remove_min right
		val s0 = (size left) + (size right0) + 1
    	in
        	Node (left, p0, s0, right0)
    	end

fun remove (k : key, Empty : 'a dict) : 'a dict =
	Empty
  | remove (k, t as Node (left, p0 as (k0, v0), _, right)) =
	case Key.cmp (k, k0)
	  of General.LESS => let
		 val left0 = remove (k, left)
		 val s0 = (size left0) + (size right) + 1
		 in
    		 Node (left0, p0, s0, right)
		 end
	   | General.GREATER => let
		 val right0 = remove (k, right)
		 val s0 = (size left) + (size right0) + 1
		 in
    		 Node (left, p0, s0, right0)
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

fun root Empty =
	raise Dict
  | root (Node (_, p, _, _)) =
	p

fun map _ Empty =
	Empty
  | map f (Node (left, (k0, v0), s, right)) =
	Node (map f left, (k0, f v0), s, map f right)

fun fold f start Empty =
	start
  | fold f start (Node (left, (k0, v0), s, right)) = let
	val left0 = fold f start left
	val acc = f (v0, left0)
	in
    	fold f acc right
	end

fun filter f Empty =
	Empty
  | filter f (Node (left, (k0, v0), s, right)) = let
	val left0 = filter f left
	val right0 = filter f right
	val s0 = (size left0) + (size right0) + 1
	val t0 = Node (left0, (k0, v0), s0, right0)
	in
    	if f v0 then
    		t0
		else
    		delete t0
	end

end


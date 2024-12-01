signature DICTIONARY =
sig

    type key
    type 'a dict

	exception Dict

    val empty : 'a dict
	val is_empty : 'a dict -> bool
	val size : 'a dict -> int
	val insert : (key * 'a) * 'a dict -> 'a dict
	val lookup : key * 'a dict -> 'a
	val find : key * 'a dict -> 'a option
	val member : key * 'a dict -> bool
	val modify : (key * 'a) * 'a dict -> 'a dict
	val remove : key * 'a dict -> 'a dict
	val from_list : (key * 'a) list -> 'a dict
	val to_list : 'a dict -> (key * 'a) list
	val root : 'a dict -> key * 'a

	val map : ('a -> 'b) -> 'a dict -> 'b dict
	val fold : ('a * 'b -> 'b) -> 'b -> 'a dict -> 'b
	val filter : ('a -> bool) -> 'a dict -> 'a dict

end


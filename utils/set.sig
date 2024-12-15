signature SET =
sig

	type elem
    type set

	exception Set

    val empty     : set
	val size      : set -> int
	val insert    : elem * set -> set
	val lookup    : elem * set -> elem
	val find      : elem * set -> elem option
	val modify    : elem * elem * set -> set
	val remove    : elem * set -> set
	val from_list : elem list -> set
	val to_list   : set -> elem list

end


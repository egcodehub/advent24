signature HEAP =
sig

	type heap
	type elem

	val empty : heap
	val is_empty : heap -> bool
	val peek : heap -> elem option
	val pop : heap -> (elem * heap) option
	val push : elem * heap -> heap

	val from_list : elem list -> heap
	val to_list : heap -> elem list

end

signature ORD_KEY =
sig

	type t
	val cmp : t * t -> General.order
	val eq : t * t -> bool

end

fun group_n n lst = let
	fun aux xs =
		if (List.length xs) < n then
			[]
		else
			(List.take (xs, n)) :: (aux (List.tl xs))
	in
    	if n > (List.length lst) then
			raise Fail "The size of the groups is too big"
		else if n <= 0 then
			raise Fail "The size must be greater than 0"
		else
			aux lst
	end


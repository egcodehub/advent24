structure Main =
struct

fun main () = let
    val _ = List.foldl (fn (x, _) => print (x ^ "\n")) () (Read.read_lines "small.txt")
	in
    	()
	end

end

val _ = Main.main ()

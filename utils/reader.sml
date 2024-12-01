signature READ =
sig

	val read_file : string -> string
	val read_lines : string -> string list

end

structure ReadImpl =
struct

fun read_file (file_name : string) : string = let
	val f = TextIO.getInstream (TextIO.openIn file_name)
	val (file, _) = TextIO.StreamIO.inputAll f
	val _ = TextIO.StreamIO.closeIn f
	in
    	file
	end

fun read_lines (file_name : string) : string list = let
	val file = read_file file_name
	in
    	String.tokens (fn c => c = #"\n") file
	end

end

structure Read : READ = ReadImpl


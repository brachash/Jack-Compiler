note
	description: "Summary description for {ENTRY}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	ENTRY

create
	make

feature

	name: STRING_8

	type: STRING_8

	kind: STRING_8

	num: INTEGER_32
			--mispur

	make (my_name: STRING_8; my_type: STRING_8; my_kind: STRING_8; my_num: INTEGER_32)
		do
			name := ""
			type := ""
			kind := ""
			num := 0
			name.append (my_name)
			type.append (my_type)
			kind.append (my_kind)
			num := my_num
		end

end -- class ENTRY


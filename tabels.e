note
	description: "Summary description for {TABLES}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	TABLES

create
	make

feature

	table_name: STRING_8

	prev_table_name: STRING_8

	entries: ARRAY [ENTRY]

	offsets: ARRAY [INTEGER_32]
			--holds all of the offsets for the different kinds
			--order: argument, var (local), field (this), static

	make (my_table_name: STRING_8; my_prev_table_name: STRING_8)
		do
			create entries.make_empty
			create offsets.make_filled (0, 0, 3)
			create table_name.make_from_string (my_table_name)
			create prev_table_name.make_from_string (my_prev_table_name)
		end

	addEntry (entry_name: STRING_8; entry_type: STRING_8; entry_kind: STRING_8)
		local
			ind: INTEGER_32
			curline: ENTRY
		do
			if entry_kind.starts_with ("argument") then
				ind := 0
			else
				if entry_kind.starts_with ("var") then
					ind := 1
				else
					if entry_kind.starts_with ("field") then
						ind := 2
					else
						if entry_kind.starts_with ("static") then
							ind := 3
						end
					end
				end
			end
			create curline.make (entry_name,entry_type, entry_kind, offsets.at (ind))
			entries.force (curline, entries.count + 1)
			offsets.at (ind) := offsets.at (ind) + 1
		end

	get_place (my_name: STRING_8): STRING_8
			--extracts the type of my name from the table and its index
		local
			i: INTEGER_32
			curkind, res: STRING_8
		do
			res := ""
			Result := "not_found"
			from
				i := 1
			until
				i = entries.count + 1
			loop
				if entries.at (i).name.is_equal (my_name) then
					curkind := entries.at (i).kind
					if curkind.is_equal ("field") then
						res := "this"
					end
					if curkind.is_equal ("var") then
						res := "local"
					end
					if curkind.is_equal ("argument") then
						res := "argument"
					end
					res.append_character (' ')
					res.append_integer (entries.at (i).num)
					Result := res
					i := entries.count
				end
				i := i + 1
			end
		end




	getIndexOfEntry (my_name: STRING_8): INTEGER
		local
			i: INTEGER_32

		do
			from
				i := 1
			until
				i = entries.count + 1
			loop
				if entries.at (i).name.is_equal (my_name) then
					result:=i
					i := entries.count--just so we could leave faster
				end
				i := i + 1
			end
		end

end -- class TABLES


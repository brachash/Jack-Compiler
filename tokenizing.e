note
	description: "Summary description for {TOKENIZING}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class
	TOKENIZING

create
	make

feature {NONE} --main private

	make (k: INTEGER_32)
		local
			k1: INTEGER_32
		do
			k1 := k
		end

feature
	tokenize (currentfile: STRING_8; path: STRING_8)
		local
			currentfilenamewithoutjack: STRING_8
			jackfile, txmlfile: PLAIN_TEXT_FILE
			flag: BOOLEAN
			currentword: STRING_8
			currentchar, currentcharhelp: CHARACTER_8
		do
			if attached currentfile as currentfilenew then
				currentfilenamewithoutjack := currentfilenew.as_string_8
				currentfilenamewithoutjack.replace_substring_all (".jack", "")
				create jackfile.make_open_read (path + "\" + currentfilenew.as_string_32 + ".jack")
				create txmlfile.make_open_write (path + "\" + currentfilenamewithoutjack + "T.xml")
				txmlfile.put_string ("<tokens>%N")
				if (not jackfile.end_of_file) then
					jackfile.read_character
					currentchar := jackfile.last_character
				end
				from
				until
					jackfile.end_of_file
				loop
					flag := False
					currentword := ""
					if (isdigit (currentchar)) then
						flag := True
						currentword.append_character (currentchar)
						if (not jackfile.end_of_file) then
							jackfile.read_character
							currentchar := jackfile.last_character
						end
						from
						until
							(not isdigit (currentchar))
						loop
							currentword.append_character (currentchar)
							if (not jackfile.end_of_file) then
								jackfile.read_character
								currentchar := jackfile.last_character
							end
						end
						txmlfile.put_string ("<integerConstant> " + currentword + " </integerConstant>%N")
					else
						if (currentchar = '/') then
							flag := True
							currentword.append_character (currentchar)
							if (not jackfile.end_of_file) then
								jackfile.read_character
								currentchar := jackfile.last_character
							end
							if (currentchar = '/') then
								jackfile.read_line
								if (not jackfile.end_of_file) then
									jackfile.read_character
									currentchar := jackfile.last_character
								end
							else
								if (currentchar = '*') then
									jackfile.read_character
									currentchar := jackfile.last_character
									jackfile.read_character
									currentcharhelp := jackfile.last_character
									from
									until
										(currentchar = '*' and currentcharhelp = '/')
									loop
										currentchar := currentcharhelp
										jackfile.read_character
										currentcharhelp := jackfile.last_character
									end
									if (not jackfile.end_of_file) then
										jackfile.read_character
										currentchar := jackfile.last_character
									end
								else
									txmlfile.put_string ("<symbol> / </symbol>%N")
								end
							end
						else
							if (issymbol (currentchar)) then
								flag := True
								if (currentchar = '<') then
									txmlfile.put_string ("<symbol>" + " &lt; " + "</symbol>%N")
								else
									if (currentchar = '>') then
										txmlfile.put_string ("<symbol>" + " &gt; " + "</symbol>%N")
									else
										if (currentchar = '&') then
											txmlfile.put_string ("<symbol>" + " &amp; " + "</symbol>%N")
										else
											txmlfile.put_string ("<symbol> " + currentchar.out + " </symbol>%N")
										end
									end
								end
								if (not jackfile.end_of_file) then
									jackfile.read_character
									currentchar := jackfile.last_character
								end
							else
								if (currentchar = '"') then
									flag := True
									if (not jackfile.end_of_file) then
										jackfile.read_character
										currentchar := jackfile.last_character
									end
									from
									until
										(currentchar = '"')
									loop
										currentword.append_character (currentchar)
										jackfile.read_character
										currentchar := jackfile.last_character
									end
									txmlfile.put_string ("<stringConstant> " + currentword + " </stringConstant>%N")
									if (not jackfile.end_of_file) then
										jackfile.read_character
										currentchar := jackfile.last_character
									end
								else
									if (currentchar.is_alpha) then
										flag := True
										from
										until
											(not currentchar.is_alpha)
										loop
											currentword.append (currentchar.out)
											jackfile.read_character
											currentchar := jackfile.last_character
										end
										if (iskeyword (currentword)) then
											txmlfile.put_string ("<keyword> " + currentword + " </keyword>%N")
										else
											from
											until
												(not (currentchar = '_') and (not currentchar.is_alpha_numeric))
											loop
												currentword.append (currentchar.out)
												jackfile.read_character
												currentchar := jackfile.last_character
											end
											txmlfile.put_string ("<identifier> " + currentword + " </identifier>%N")
										end
									end
								end
							end
						end
					end
					if (flag = False) then
						if (not jackfile.end_of_file) then
							jackfile.read_character
							currentchar := jackfile.last_character
						end
					end
				end
				txmlfile.put_string ("</tokens>%N")
				jackfile.close
				txmlfile.close
			end
		end

feature

	isdigit (currentchar: CHARACTER_8): BOOLEAN
		local
			digit: ARRAY [CHARACTER_8]
			i: INTEGER_32
			flag: BOOLEAN
			help: CHARACTER_8
		do
			create digit.make_empty
			digit := <<'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'>>
			flag := False
			i := 1
			from
			until
				(i > digit.capacity)
			loop
				help := digit.at (i)
				if (equal (help, currentchar)) then
					flag := True
				end
				i := i + 1
			end
			Result := flag
		end

feature

	issymbol (currentchar: CHARACTER_8): BOOLEAN
		local
			symbol: ARRAY [CHARACTER_8]
			i: INTEGER_32
			flag: BOOLEAN
			help: CHARACTER_8
		do
			create symbol.make_empty
			symbol := <<'{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '&', '|', '<', '>', '=', '~'>>
			flag := False
			i := 1
			from
			until
				(i > symbol.capacity)
			loop
				help := symbol.at (i)
				if (equal (help, currentchar)) then
					flag := True
				end
				i := i + 1
			end
			Result := flag
		end

feature

	iskeyword (currentword: STRING_8): BOOLEAN
		local
			keyword: ARRAY [STRING_8]
			i: INTEGER_32
			flag: BOOLEAN
			help: STRING_8
		do
			create keyword.make_empty
			keyword := <<"constructor", "class", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return">>
			flag := False
			i := 1
			from
			until
				(i > keyword.capacity)
			loop
				help := keyword.at (i)
				if (equal (help, currentword)) then
					flag := True
				end
				i := i + 1
			end
			Result := flag
		end

end -- class TOKENIZING


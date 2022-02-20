note
	description: "project application root class"
	date: "$Date$"
	revision: "$Revision$"

class
	APPLICATION

inherit
	ARGUMENTS

create
	make

feature {NONE} --main private

	make
		local
			i, j: INTEGER_32
			directory, directory1: DIRECTORY
			iAsm, jAsm,k,s,count: INTEGER_32
		     path,file: STRING_8
		    asmfile: PLAIN_TEXT_FILE
		   	directory2,directory3: DIRECTORY
			inst: INSTRUCTIONS

		do
			print ("Please enter your path: %N")
			Io.read_line
			path := Io.last_string
			--to Token XML
			create directory.make_open_read (path)
			directory.start
			j := directory.count
			from
				i := 1
			until
				i > j
			loop
				directory.readentry
				ifjackfiletranslatetot_xml (directory, path)
				i := i + 1
			end
            directory.close
			                                           -----***-----

			--vm
			create directory1.make_open_read (path)
			directory1.start
			j := directory1.count
			i := 0
			from
				i := 1
			until
				i > j
			loop
				directory1.readentry
				ifT_xmlFileThanTranslateToVm (directory1, path)
				i := i + 1
			end
            directory1.close
		                                           -----***-----

			--to asm
			print("translating ASM file...%NThis may take a momment, Thanks for your patience!")
			create directory2.make_open_read (path)
			directory2.start
			jAsm := directory2.count
			create directory3.make_open_read (path)
			directory3.start

			--cut path name to file name
			file:=path
			from
			   	k:=1
			until
			   	k=0
			loop
				s:=k
			   	k:=file.substring_index("\", s+1)--index of first occurance of or after start index; 0 if none	
			end
			count:=file.count
			file:=file.substring(s+1,count)
			create asmfile.make_open_read_append (path + "\" + file + ".asm")
			create inst.make (count)

			--write boot strap to asm file
			from
						iAsm := 1
			until
						iAsm > jAsm+1
			loop
				directory3.readentry
				if attached directory3.last_entry_32 as currentfile then--do boot starp only if a sys file exists
					if currentfile.starts_with ("Sys.vm") then
						asmfile.put_string("%N@256%N"+"D=A%N"+"@SP%N"+"M=D")--initialize *sp=256
			            asmfile.put_string (inst.call ("Sys.init","0"))--call func sys.init to start the program

			        end
			    end
			    iAsm:=iAsm+1
			end

			--create directory.make_open_read (path)
			--directory.start
			from
				iAsm := 1
			until
				iAsm > jAsm+1
			loop
				directory2.readentry
				ifvmfiletranslatetoasm (directory2, path,asmfile,inst)
				iAsm := iAsm + 1
			end
			asmFile.close
		end--do

feature

	ifjackfiletranslatetot_xml (directory: DIRECTORY; path: STRING_8)
		local
			token: TOKENIZING
			k: INTEGER_32
		do
			if attached directory.last_entry_32 as currentfile then
				if currentfile.ends_with (".jack") then
					create token.make (k)
					token.tokenize (currentfile.as_string_32.as_string_8, path)
				end
			end
		end

feature

	ifT_xmlFileThanTranslateToVm (directory: DIRECTORY; path: STRING_8)
		local
			vm: VMING
		do
			if attached directory.last_entry_32 as currentfile then
				if currentfile.ends_with ("T.xml") then--------------------------------------------------------------------------------
					create vm.make (create {IMMUTABLE_STRING_32}.make_from_string_32 (currentfile), path)
				end
			end
		end

feature

	ifvmfiletranslatetoasm (directory: DIRECTORY; path: STRING_8; asmfile: PLAIN_TEXT_FILE;inst: INSTRUCTIONS)
		local
			currentline,funcName,lclAmount,argAmount,help: STRING_8
			vmfile: PLAIN_TEXT_FILE
			index,index1,count,lclnum:INTEGER_32
		do
			if attached directory.last_entry_32 as currentfile then
				if currentfile.ends_with (".vm") then
					if attached currentfile as currentfilecut then

						create vmfile.make_open_read (path + "\" + currentfilecut.as_string_32)

						from
						until
							vmfile.end_of_file
						loop
							vmfile.read_line
							currentline := vmfile.last_string
							if (currentline.starts_with ("add")) then
								asmfile.put_string (inst.add)
							end
							if (currentline.starts_with ("sub")) then
								asmfile.put_string (inst.sub)
							end
							if (currentline.starts_with ("neg")) then
								asmfile.put_string (inst.neg)
							end
							if (currentline.starts_with ("eq")) then
								asmfile.put_string (inst.eq)
							end
							if (currentline.starts_with ("gt")) then
								asmfile.put_string (inst.gt)
							end
							if (currentline.starts_with ("lt")) then
								asmfile.put_string (inst.lt)
							end
							if (currentline.starts_with ("and")) then
								asmfile.put_string (inst.andours)
							end
							if (currentline.starts_with ("or")) then
								asmfile.put_string (inst.orours)
							end
							if (currentline.starts_with ("not")) then
								asmfile.put_string (inst.notours)
							end
							if (currentline.starts_with ("push constant")) then
								currentline.keep_tail (currentline.count - 14)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.pushconstent (currentline))
							end
							if (currentline.starts_with ("push local")) then
								currentline.keep_tail (currentline.count - 11)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.pushlocal (currentline))
							end
							if (currentline.starts_with ("pop local")) then
								currentline.keep_tail (currentline.count - 10)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							    index1:=currentline.substring_index("%T", 1) --index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
							   if (index1 /=0) then --ckeck if there where any //
							   	currentline:=currentline.substring (1,index1-1)--this was to delete the //
							   end
								asmfile.put_string (inst.poplocal (currentline))
							end
							if (currentline.starts_with ("push argument")) then
								currentline.keep_tail (currentline.count - 14)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.pushargument (currentline))
							end
							if (currentline.starts_with ("pop argument")) then
								currentline.keep_tail (currentline.count - 13)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.popargument (currentline))
							end
							if (currentline.starts_with ("pop this")) then
								currentline.keep_tail (currentline.count - 9)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.popthis (currentline))
							end
							if (currentline.starts_with ("push this")) then
								currentline.keep_tail (currentline.count - 10)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.pushthis (currentline))
							end
							if (currentline.starts_with ("push that")) then
								currentline.keep_tail (currentline.count - 10)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.pushthat (currentline))
							end
							if (currentline.starts_with ("pop that")) then
								currentline.keep_tail (currentline.count - 9)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.popthat (currentline))
							end
							if (currentline.starts_with ("pop temp")) then
								currentline.keep_tail (currentline.count - 9)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.poptemp (currentline))
							end
							if (currentline.starts_with ("push temp")) then
								currentline.keep_tail (currentline.count - 10)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.pushtemp (currentline))
							end
							if (currentline.starts_with ("pop pointer")) then
								currentline.keep_tail (currentline.count - 12)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.poppointer (currentline))
							end
							if (currentline.starts_with ("push pointer")) then
								currentline.keep_tail (currentline.count - 13)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.pushpointer (currentline))
							end
							if (currentline.starts_with ("pop static")) then
								currentline.keep_tail (currentline.count - 11)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.popstatic (currentline,currentfile))
							end
							if (currentline.starts_with ("push static")) then
								currentline.keep_tail (currentline.count - 12)
								index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
								asmfile.put_string (inst.pushstatic (currentline,currentfile))
							end

							--targil-2
							if(currentline.starts_with ("label ")) then
							   currentline.keep_tail (currentline.count-6)
							   index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end

							   help:=""
							   help.copy(currentfile)
                               help.keep_head (help.count-3)--delete .vm at the end
							   asmfile.put_string(inst.label(currentline,help))
						    end

						    if(currentline.starts_with ("goto")) then
							   currentline.keep_tail(currentline.count-5)
							   index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
							   help:=""
							   help.copy(currentfile)
                               help.keep_head (help.count-3)
							   asmfile.put_string(inst.goto(currentline,help))
							end

						    if(currentline.starts_with ("if-goto")) then
							   currentline.keep_tail (currentline.count-8)
							   index:=currentline.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							   	currentline:=currentline.substring (1,index-1)--this was to delete the //
							   end
							   help:=""
							   help.copy(currentfile)
                               help.keep_head (help.count-3)
							   asmfile.put_string(inst.if_goto(currentline,help))
							end

						    if(currentline.starts_with ("function")) then
							   currentline.keep_tail (currentline.count-9)
						       funcName:=""
							   lclAmount:=""
							   funcName.copy (currentline)
							   --funcName.keep_head (currentline.count-2)
							   index:=funcName.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   funcName:=funcName.substring (1,index-1)--this was to delete the local num arg at the end
							   lclAmount.copy (currentline)
							   lclAmount.keep_tail (2)
							   lclnum:=lclAmount.to_integer--these two lines r just to make sure we get a num and not space+num
							   lclAmount:=lclNum.out
							   asmfile.put_string(inst.function(funcName, lclAmount))
							end

							if(currentline.starts_with ("return")) then
							   asmfile.put_string(inst.return)
							end

							if(currentline.starts_with ("call")) then
							   currentline.keep_tail (currentline.count-5)
							   funcName:=""
							   argAmount:=""
							   funcName.copy(currentline)
							   index:=funcName.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   funcName:=funcName.substring (1,index-1)--this was to delete the local num arg at the end
							   argAmount.copy(currentline)
							   index:=argAmount.substring_index(" ", 1)
							   --argAmount.keep_head(index+1)
							   count:=argAmount.count
						       argAmount.keep_tail(count-index)
						       index:=argAmount.substring_index(" ", 1)--index of first occurance of or after start index; 0 if none	
							   if (index/=0) then--ckeck if there where any //
							     argAmount:=argAmount.substring (1,index-1)--this was to delete the local num arg at the end
							   end


							   asmfile.put_string(inst.call(funcName,argAmount))
							end
						end
					end
				end
			end
		end

end -- class APPLICATION


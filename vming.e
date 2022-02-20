note
	description: "Summary description for {VMING}."
	author: ""
	date: "$Date$"
	revision: "$Revision$"

class--JackCompiler our
	VMING
	create
 make
feature {NONE} -- Initialization
 curFileN:IMMUTABLE_STRING_32
 curLine, curWord,path,valFirst, typeFirst,typeVN,typeElse, valElse, typeQ, valQ, valVN,typeTerm,valTerm:STRING
 curChar,n1:CHARACTER_8
 tokenFile,vmFile:PLAIN_TEXT_FILE
 temp,tableCounter, tableSize:INTEGER
 curRow:detachable INTEGER
 i,ind:INTEGER_32
 depth,counterField:INTEGER
 localCount:INTEGER
 tArray:ARRAY[STRING]
 unaryOp:ARRAY[CHARACTER_8]
 op:ARRAY[STRING]
 keywordConstant:ARRAY[STRING_8]
 read,flag,isConstructor,isMethod:BOOLEAN
 allTables:ARRAY[TABLES]
 namesTable:TABLES
 prevTable,curTable,dest,constructorPrintTemp,methodPrintTemp:STRING
 argCount,ifIndex,whilePIndex, argExpListCounter:INTEGER
 isVarN:BOOLEAN
 make(curFileN1:IMMUTABLE_STRING_32 path1:STRING)
 do
  curFileN:=curFileN1
  path:=path1
  curWord:=""
  --ops
  typeFirst:=""
  valFirst:=""
  typeElse:=""
  valElse:=""
  typeQ:=""
  valQ:=""
  typeVN:=""
  valVN:=""
  typeTerm:=""
  valTerm:=""
  prevTable:=""
  curTable:=""
  constructorPrintTemp:=""
  methodPrintTemp:=""
  --
  n1:='0'
  counterField:=0
  localCount:=0
  argExpListCounter:=0
  temp:=0
  i:=0
  ind:=0
  depth:=0
  curRow:=0
  tableCounter:=0
  tableSize:=0
  argCount:=0
  ifIndex:=-1
  whilePIndex:=-1

  dest:=""
  isMethod:=false
  isConstructor:=false
  flag:=false
  read:=true
  isVarN:=false
  create op.make_empty
  create unaryOp.make_empty
  create keywordConstant.make_empty
  create tArray.make_filled("", 0, 3000)--tArray includes all T.xml file
  create allTables.make_empty
  create namesTable.make("","")
  --
  op:=<<"+","-" ,"*", "/", "&amp;", "|", "&gt;", "&lt;", "=">>
  unaryOp:=<<'-','~'>>
  keywordConstant:=<<"true" , "false", "null" , "this">>

  create tokenFile.make_open_read(path+"\"+curFileN)--current file
  ind:=curFileN.substring_index (".", 1)
  create vmFile.make_open_write (path+"\"+curFileN.head (ind-2)+"Our.vm") --OUTPUT FILE .xml
  --reading <tokens>
  tokenFile.read_line
  curLine:=tokenFile.last_string
  readingFile  ---initialize the tarray with all the lines  from the file t!
  --reding class
  --curRow:=curRow+1
  curLine:=tArray.item (curRow)
  typeFirst:=elem_type(curLine)
  valFirst:=elem_val(curLine)
  if(is_class(typeFirst,valFirst)) then
   print("one jack file finished compiling%N")
   tokenFile.close
   vmFile.close
  end
 end

feature
 readingFile
 local
  k:INTEGER_32
 do
  from k:=0
  until tokenFile.end_of_file
  loop
   tokenFile.read_line
   curLine:=tokenFile.last_string
   tArray.put (curLine.out, k)
   k:=k+1
  end
 end

feature
 elem_val (line:STRING):STRING
 local
  ind1,temp1:INTEGER_32
  temp_s,str:STRING
  c,o:CHARACTER_8
 do
  str:=""
  if(not(equal(line, "</tokens>"))) then
  str.append_string (line)
  o:='<'
  c:='>'
  ind1:=-1
  temp_s:=""
  ind1:=str.index_of (o, 2)
  ind1:=ind1-1
  temp1:=ind1-1--index of the end of the value
  str.keep_head(temp1)--str has <> value--d.e: the begining of the element and its value
  ind1:=str.index_of (c, 1)--has the index of the space after the <> , right before the value
  ind1:=ind1+1
  temp1:=str.count-ind1--returns the number of the last index of the value
  str.keep_tail(temp1)--returns the value of the xml element
  end
  result:=str
 end

 feature
 elem_type(str:STRING):STRING
 local
  ind2:INTEGER_32  ----
 do
  ind2:=str.substring_index (">", 1)
  result:=str.substring (2, ind2-1)
 end


 element_builder(val:STRING type:STRING):STRING
 do
  result:="<"+type+"> "+val+" </"+type+">%N"
 end

feature
 is_op(type :STRING val:STRING):BOOLEAN
 local
  bool:BOOLEAN
 do
  result:=false
  bool:=true
  from
   i:=1
  until
   i>9
  loop
   if(equal(val,op.at(i).out)) then
    result:= true
    bool:=false
   end
   i:=i+1
  end
  if (bool) then
   result:=false
  end
 end

 is_unaryOp(type:STRING val:STRING):BOOLEAN
 local
  bool:BOOLEAN
 do
  result:=false
  bool:=true
  from
   i:=1
  until
   i>2
  loop
   if(equal(val,unaryOp.at(i).out)) then
    result:= true
    bool:=false
   end
   i:=i+1
  end
  if (bool) then
   result:=false
  end
 end
 is_keywordConstant(type:STRING val:STRING):BOOLEAN
 local
  bool:BOOLEAN
 do
  result:=false
  bool:=true
  from
   i:=1
  until
   i>4
  loop
   if(equal(val,keywordConstant.at(i))) then
    result:= true
    bool:=false
   end
   i:=i+1
  end
  if (bool) then
   result:=false
  end
 end

feature
 is_term1(type:STRING val:STRING):BOOLEAN   --getting type
 do
  result:=false
  if(is_integerConst(type, val) or is_stringConst(type, val) or is_keywordConstant(type, val) or (equal(type, "identifier")) or (equal(val,"(")) or (is_unaryOp(type, val))) then
   result:= true
  else
   result:= false
  end
 end

 printStringVM(val:STRING)
 local
  m:INTEGER
  tmp:STRING
       do
       tmp:=""
       tmp.append (val)--
        vmFile.put_string ("push constant ")
        vmFile.put_integer (tmp.count)--
        vmFile.put_string ("%N")
        vmFile.put_string("call String.new 1%N")
        from m:=1--1
        until m=tmp.count+1--+1???$$$
        loop
        vmFile.put_string ("push constant ")
        vmFile.put_integer (tmp.at (m).code)
        vmFile.put_string ("%Ncall String.appendChar 2%N")
        m:=m+1
        end
      end

    feature
      printKeywordConstant(val:STRING)
      do
       if equal(val,"true") then
        vmFile.put_string ("push constant 0%Nnot%N")
       end
       if equal(val,"null") then
        vmFile.put_string ("push constant 0%N")
       end
       if equal(val,"false") then
        vmFile.put_string ("push constant 0%N")
       end
       if equal(val,"this") then
        vmFile.put_string ("push pointer 0%N")
       end
      end

feature
 is_term(type:STRING val:STRING):BOOLEAN   --getting type
 local
 	destination:STRING
 	isThis:BOOLEAN
 do
  result:=false
  if(is_integerConst(type, val)) then
   vmFile.put_string ("push constant "+val+"%N")
   result:= true
  else if(is_stringConst(type, val)) then
   printStringVM(val);
   result:= true
    else if(is_keywordConstant(type, val)) then
     printKeywordConstant(val);
   result:= true
  else if(is_subroutineCall1(type, val)) then
   if(is_subroutineCall(type,val)) then
    result:= true
   end
  else if(equal(val, "(")) then
   if(is_openBrackets_Expression_closeBrackets(type, val)) then
    result:= true
   end
   else if( is_unaryOp(type, val)) then
    if( is_unaryOpTerm(type, val)) then--print inside
    result:= true
   end
  else
  	if(is_varNExp(type, val)) then--varName[expression]--prints expression inside
     result:=true
    else
    	if (is_varName(type, val)) then--varName
    	    isThis:=false
            destination:=find_place_of (val, curTable)--finds where this var is defined
          --look for the var in the inner scope
          if destination.is_equal ("not_found") then --if its not there, its in the father's
             destination:=find_place_of (val, prevTable)
             isThis:=true
          end
 ---bla bla

          if(destination.has_substring ("field"))
          then
          	  isThis:=false
          	  destination.replace_substring_all ("field","this")
          	  vmFile.put_string ("push " + destination + "%N") --put the value to the destination
          else
          	 if(destination.has_substring ("local")or destination.has_substring ("argument") or destination.has_substring ("this"))
          	 then
          	 	 vmFile.put_string ("push " + destination + "%N") --put the value to the destination
          	 else--destination.has_substring ("static")	
          	     isThis:=false
          	     vmFile.put_string ("push static" + destination + "%N") --put the value to the destination
          	 end
           end

          result:=true
        else
    	  result:=false
        end
   end
  end
  end
  end
  end
  end
  end
 end

feature
 is_integerConst(type:STRING val:STRING):BOOLEAN
 do
  result:=false
  if(equal(type,"integerConstant")) then
   result:=true
  else
   result:=false
  end
 end
 is_stringConst(type:STRING val:STRING):BOOLEAN
 do
  result:=false
  if(equal(type,"stringConstant")) then
   result:=true
  else
   result:=false
  end
 end
 is_className(type:STRING val:STRING):BOOLEAN
 do
  result:=false
  if(equal(type,"identifier")) then
   result:=true
  else
   result:=false
  end
 end

 is_subroutineName(type:STRING val:STRING):BOOLEAN
 do
  result:=false
  if(equal(type,"identifier")) then
   result:=true
  else
   result:=false
  end
 end

feature
 is_varName(type:STRING val:STRING):BOOLEAN
 do
  result:=false
  if(equal(type,"identifier")) then
   result:=true
  else
   result:=false
  end
 end

 is_varNExp(type:STRING val:STRING):BOOLEAN --is totally wrong ours
 local
  typeOp,valOp, typeClo, valClo, typeExp,valExp,var, varNval,destination:STRING
  isThis:BOOLEAN
 do
  typeExp:=""
  valExp:=""
  typeOp:=""
  valOp:=""
  typeClo:=""
  valClo:=""
  var:=""
  varNval:=""
  result:=false
  varNval:=val--a
  --reading [
  curRow:=curRow+1
  curLine:=tArray.at (curRow)
  typeOp:=elem_type(curLine)
  valOp:=elem_val(curLine)
  if(equal(valOp,"["))then
   --reading expression
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeExp:=elem_type(curLine)
   valExp:=elem_val(curLine)
   if(is_expression(typeExp, valExp)) then
    isVarN:=true
    ---
    var:=elem_val(curLine)

    	    isThis:=false
            destination:=find_place_of (val, curTable)--finds where this var is defined
          --look for the var in the inner scope
          if destination.is_equal ("not_found") then --if its not there, its in the father's
             destination:=find_place_of (val, prevTable)
             isThis:=true
          end

          if(destination.has_substring ("field"))
          then
          	  isThis:=false
          	  destination.replace_substring_all ("field","this")
          	  vmFile.put_string ("push " + destination + "%N") --put the value to the destination
          else
          	 if(destination.has_substring ("local")or destination.has_substring ("argument") or destination.has_substring ("this"))
          	 then
          	 	 vmFile.put_string ("push " + destination + "%N") --put the value to the destination
          	 else--destination.has_substring ("static")	
          	     isThis:=false
          	     vmFile.put_string ("push static" + destination + "%N") --put the value to the destination
          	 end
           end


    vmFile.put_string("add%N")
    vmFile.put_string("pop pointer 1%N")
    vmFile.put_string("push that 0%N")

    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeClo:=elem_type(curLine)
    valClo:=elem_val(curLine)
    if(equal(valClo,"]")) then
     result:=true
    end
   end
  else
   curRow:=curRow-1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
  end

 end
 kindOf(var:STRING scope:STRING):STRING
  local
   b:INTEGER
  do
   b:=index_of(scope)
   result:=""
  end
  offsetOf(var:STRING scope:STRING):INTEGER
  local
   b:INTEGER
  do
   b:=index_of(scope)
   result:=1
  end
 is_subroutineCall1(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp, typeClo, valClo, typeExpList,valExpList:STRING
 do
  typeOp:=""
  valOp:=""
  result:=false
  if(equal(type, "identifier")) then
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   if (equal(valOp,"(")) then
    curRow:=curRow-1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    result:=true
   else if (equal(valOp,".")) then
    curRow:=curRow-1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    result:=true
   else
    curRow:=curRow-1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
   end
   end
  end
 end
 is_subroutineCall(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp, typeClo, valClo, typeExpList,valExpList:STRING
 do
  typeExpList:=""
  valExpList:=""
  typeOp:=""
  valOp:=""
  typeClo:=""
  valClo:=""
  result:=false
  if(equal(type, "identifier")) then
   --reading . or( or extra!
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   if (equal(valOp,"(")) then
    if(is_subNameExpList(type,val,typeOp,valOp)) then
     result:=true
    end
   else if (equal(valOp,".")) then
    if(is_classVarSubExpList(type,val,typeOp,valOp)) then
     result:=true
    end
   else
   	argExpListCounter:=0
    curRow:=curRow-1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
   end
   end
  end
 end

feature
 is_subNameExpList(type:STRING val:STRING typeOp1:STRING valOp1:STRING):BOOLEAN
 local
  typeOp,valOp, typeClo, valClo, typeExpList,valExpList, subName:STRING
  curArgExpCount,prevTableIndex:INTEGER
 do
  typeExpList:=""
  valExpList:=""
  typeOp:=""
  valOp:=""
  typeClo:=""
  valClo:=""
  subName:=""
  curArgExpCount:=0
  argExpListCounter:=0
  result:=false
 -- if(is_subroutineName(type,val)) then
   subName:=val
   --reading expressionList or )
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeClo:=elem_type(curLine)
   valClo:=elem_val(curLine)
   vmFile.put_string ("push pointer 0%N")
   if (not(subName.has_substring ("new")))
   then
      argExpListCounter:=argExpListCounter+1
   end


   --output.put_string ("call "+prevTable+"."+subName+" "+argExpListCounter.out+"%N")-- i think
   if(not(equal(valClo,")"))) then

        if(is_expressionList(typeClo, valClo)) then
     --now argExpListCounter includes the number of args of this subroutine call
     --reading )
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeClo:=elem_type(curLine)
     valClo:=elem_val(curLine)

    vmFile.put_string ("call "+prevTable+"."+subName+" "+argExpListCounter.out+"%N")
        end
   else
   	   vmFile.put_string ("call "+prevTable+"."+subName+" "+argExpListCounter.out+"%N")
   end
       if(equal(valClo,")")) then
         result:=true
       end
  --end
 end

feature
 is_classVarSubExpList(type:STRING val:STRING typeP:STRING valP:STRING):BOOLEAN
 local
  typeOp,valOp, typeClo, valClo,typeSubN,valSubN,typeExpList,valExpList, classOrVarName, subName,n:STRING
  numToPrint,kindToPrint,destination:STRING
 -- kindToPrint_helpTable:TABLES
  place,inde,prevTableIndex,indexOfField:INTEGER
  classFlag,isThis:BOOLEAN

 do

 -- kindToPrint_helpTable.make ("","")
  destination:=""
  classFlag:=false
  numToPrint:=""
  typeOp:=""
  valOp:=""
  typeClo:=""
  valClo:=""
  typeSubN:=""
  valSubN:=""
  typeExpList:=""
  valExpList:=""
  classOrVarName:=""
  subName:=""
  n:=""
  indexOfField:=0
  place:=0
  inde:=0

  result:=false
  if(is_className(type,val) or is_varName(type,val))then
    classFlag:=true
    classOrVarName:=val
   if(equal(valP,".")) then
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeSubN:=elem_type(curLine)
    valSubN:=elem_val(curLine)
    argExpListCounter:=0

    if(not(equal(allTables.at (tableCounter).get_place (classOrVarName),"not_found"))) then --if we found classOrVarName
     inde:=allTables.at(tableCounter).get_place (classOrVarName).substring_index (" ", 1)
     inde:=inde--now we have the index of the counter of the value
     n:=allTables.at(tableCounter).get_place (classOrVarName)-- local 0
     n1:=n.at(inde+1)-- we cut off local and now we have '0', than next line we will do plus one, beacuse an arrys indexes start from 1 and not from 0
     kindToPrint:=allTables.at (tableCounter).entries.at(n1.out.to_integer+1).kind
     if ((kindToPrint.has_substring ("var")))then
     	kindToPrint:="local"
     else
     	kindToPrint:="argument"
     end
     numToPrint:=allTables.at (tableCounter).entries.at(n1.out.to_integer+1).num.out
     vmFile.put_string("push "+kindToPrint+" "+numToPrint+"%N")
    end


    prevTableIndex:=index_of(prevTable)
	if (equal(allTables.at (tableCounter).get_place (classOrVarName),"not_found") and  not(equal(allTables.at(prevTableIndex).get_place (classOrVarName),"not_found")))then   --varName is a field or static

         prevTableIndex:=index_of(prevTable)
         indexOfField:=allTables.at (prevTableIndex).getindexofentry (classOrVarName)  --changing varName to ClassType

           destination:=find_place_of (classOrVarName, prevTable)
           destination.replace_substring_all ("field", "this")
       	   vmFile.put_string ("push " + destination + "%N") --push this  -------8888888888888

           indexOfField:=allTables.at (prevTableIndex).getindexofentry (classOrVarName)  --changing varName to ClassType

       	   classOrVarName:=allTables.at (prevTableIndex).entries.at (indexOfField).type

       	   argExpListCounter:=argExpListCounter+1 --push arg this

   end

   if(not(equal(allTables.at (tableCounter).get_place (classOrVarName),"not_found")))then   --var name is local
           indexOfField:=allTables.at (tableCounter).getindexofentry (classOrVarName)  --changing varName to ClassType
           classOrVarName:=allTables.at (tableCounter).entries.at (indexOfField).type

           argExpListCounter:=argExpListCounter+1 --push arg this---not sure if we need
   end

    if(is_subroutineName(typeSubN,valSubN)) then
     subName:=valSubN
     --READING (
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
     if(equal(valOp,"(")) then
      --reading expressionList or )
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeClo:=elem_type(curLine)
      valClo:=elem_val(curLine)
      if(is_expressionList(typeClo, valClo)) then
       if(not(equal(valClo,")"))) then
        curRow:=curRow+1
        curLine:=tArray.at (curRow)
        typeClo:=elem_type(curLine)
        valClo:=elem_val(curLine)
       end
       --argExpListCounter:=argExpListCounter+1
       if(classFlag)
       then
          --output.put_string ("call "+allTables.at (tableCounter).entries.at(n1.out.to_integer+1).kind+ "."+subName+" "+ argExpListCounter.out+"%N")
          classFlag:=false
          if (not(subName.has_substring ("new")) and classorvarname.has_substring("Array") and classorvarname.has_substring("Keyboard") and classorvarname.has_substring("Math") and classorvarname.has_substring("Memory") and classorvarname.has_substring("Output") and classorvarname.has_substring("Screen") and classorvarname.has_substring("String") and classorvarname.has_substring("Sys") ) then
       	   	   argExpListCounter:=argExpListCounter+1
       	   end
       	   vmFile.put_string ("call " + classorvarname + "." + subname + " " + argexplistcounter.out + "%N")
       else
       	   if (not(subName.has_substring ("new")) and classorvarname.has_substring("Array") and classorvarname.has_substring("Keyboard") and classorvarname.has_substring("Math") and classorvarname.has_substring("Memory") and classorvarname.has_substring("Output") and classorvarname.has_substring("Screen") and classorvarname.has_substring("String") and classorvarname.has_substring("Sys"))then
       	   	   argExpListCounter:=argExpListCounter+1
       	   end
       	   vmFile.put_string ("call "+classOrVarName+"."+subName+" "+ argExpListCounter.out+"%N")
       end

      end
      if(equal(valClo,")")) then
       result:=true
      end
     end
    end
   end
  end
   -- argExpListCounter:=0--we addes in our targil 3
 end

feature
 is_openBrackets_Expression_closeBrackets(typeOp:STRING valOp:STRING):BOOLEAN
 local
  typeClo, valClo, typeExp, valExp:STRING
 do
  typeClo:=""
  valClo:=""
  typeExp:=""
  valExp:=""
  result:=false
  if(equal(valOp,"(")) then
   --reading expression
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeExp:=elem_type(curLine)
   valExp:=elem_val(curLine)
   if(is_expression(typeExp, valExp)) then
    --READING )
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeClo:=elem_type(curLine)
    valClo:=elem_val(curLine)
    if(equal(valClo,")")) then
     result:=true
    end
   end
  end
 end
 is_unaryOpTerm(type:STRING val:STRING):BOOLEAN
 local
 	holdValue:STRING_8
 do
  holdValue:=""
  result:=false
  if(is_unaryOp(type,val)) then
   if(equal(val,"-")) then
   	  holdValue:="neg"
   else
      if(equal(val,"~")) then
   	   	holdValue:="not"
      end
   end
   --READING TERM
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeTerm:=elem_type(curLine)
   valTerm:=elem_val(curLine)
   if(is_term(typeTerm,valTerm))
   then
    result:=true
   end
    vmFile.put_string(holdValue+"%N")
  end
 end--do

toPrintOp (val:STRING):STRING--string\substring that needs to be printed
do
 result:=""
if equal(val,"+")then
result := "add%N"
end
if equal(val,"-")then
result :="sub%N"
end
if equal(val,"*")then
result := "call Math.multiply 2%N"
end
if equal(val,"/")then
result :="call Math.divide 2%N"
end
if equal(val,"&lt;")then
result :="lt%N"
end
if equal(val,"&gt;")then
result :="gt%N"
end
if equal(val,"=")then
result :="eq%N"
end
if equal(val,"&amp;")then
result :="and%N"
end
if equal(val,"|")then
result :="or%N"
end
end

feature
 is_expression(type:STRING val:STRING):BOOLEAN
 local
  typeOp, valOp, toPrintOpS:STRING
 do
  typeOp:=""
  valOp:=""
  toPrintOpS:=""
  result:=false
  if(is_term1(type,val)) then
   if(is_term(type,val)) then--true if it's a term
    result:=true
    --reading op?
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    if(is_op(typeOp,valOp)) then
     from
     until not(is_op(typeOp,valOp))
     loop
      toPrintOpS:=toPrintOp(valOp)
      --the op after reading term
      --reading term
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeTerm:=elem_type(curLine)
      valTerm:=elem_val(curLine)
      if(is_term1(typeTerm,valTerm)) then
       if(is_term(typeTerm,valTerm)) then
        vmFile.put_string (toPrintOpS) --op printing
        result:=true
       end
      end
      --reading more op?
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeOp:=elem_type(curLine)
      valOp:=elem_val(curLine)
     end
    end
    --happans if was op or not!
    curRow:=curRow-1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
   end
  end
 end
-----
 is_expressionList(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp, typeExp, valExp:STRING
 do
  typeOp:=""
  valOp:=""
  typeExp:=""
  valExp:=""
  typeExp.append_string (type)
  valExp.append_string (val)
  result:=false
  if (equal(val,")")) then
   result:=true
  else
   if(is_expression(typeExp, valExp)) then
    result:=true
    argExpListCounter:=argExpListCounter+1
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    from
    until equal(valOp,")")
    loop
     if(equal(valOp,",")) then
      --reading expression
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeOp:=elem_type(curLine)
      valOp:=elem_val(curLine)
      if(is_expression(typeOp,valOp)) then
       --reading , or )
       argexplistcounter := argexplistcounter + 1
       curRow:=curRow+1
       curLine:=tArray.at (curRow)
       typeOp:=elem_type(curLine)
       valOp:=elem_val(curLine)
      end
     end
    end
    --we sure we read ) so curRow-1
    curRow:=curRow-1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
   end
  end
 end
------------------------------------------------
--statements
 is_statements(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp:STRING
 do
  typeOp:=type
  valOp:=val
  result:=true
  curLine:=element_builder(valOp,typeOp)  --
  from
  until (equal(valOp,"}"))
  loop
   if(is_statement(typeOp, valOp)) then
    result:=true
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
   end
  end
  curRow:=curRow-1
  curLine:=tArray.at (curRow)
  typeOp:=elem_type(curLine)
  valOp:=elem_val(curLine)
 end
 is_statement(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp:STRING
 do
  typeOp:=""
  valOp:=""
  result:=false
  if(equal(val,"let")) then
   if (is_letStatement(type,val)) then
    result:=true
   end
  else if(equal(val,"if")) then
    if (is_ifStatement(type,val)) then---MIGHT HAVE MISSING
     result:=true
    end
  else if (equal(val,"while")) then
    if (is_whileStatement(type,val)) then
     result:=true
    end
  else if(equal(val,"do")) then
    if (is_doStatement(type,val)) then
     result:=true
    end
  else if(equal(val,"return")) then
    if (is_returnStatement(type,val)) then
     result:=true
    end
  end
  end
  end
  end
  end
 end

feature
 find_place_of(val:STRING curTableName:STRING):STRING--return val type +index
  local
   b:INTEGER
  do
   b:=index_of(curTableName)--get the place of the table
   result:=allTables.at (b).get_place(val)--return the var place in the table
  end

feature
 is_letStatement(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp,valofvarN,destination:STRING
  isAnArray,isThis:BOOLEAN
  prevTableIndex,indexoffield:INTEGER
 do
  prevTableIndex:=0
  indexoffield:=0
  typeOp:=""
  valOp:=""
  valofvarN:=""
  isAnArray:=false
  result:=true
  --reading varname
  curRow:=curRow+1
  curLine:=tArray.at (curRow)
  typeVN:=elem_type(curLine)
  valVN:=elem_val(curLine)
  if (is_varName(typeVN,valVN)) then
   valofvarN:=valVN
   destination:=find_place_of (valVN, curTable)--finds where this var is defined
   --look for the var in the inner scope
   if destination.is_equal ("not_found") then --if its not there, its the father's
    destination:=find_place_of (valVN, prevTable)
    isThis:=true
   end
   --end of finding class fields or static vars
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   if(equal(valOp,"[")) then
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    if(is_expression(typeOp,valOp)) then--push i of a[i] for example
    -- output.put_string("push "+dest+"%N")--push varName
     isAnArray:=true--its an array []
     --reading ]
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
     if equal(valOp, "]") then

--      prevTableIndex:=index_of(prevTable)
--      indexOfField:=allTables.at (prevTableIndex).getindexofentry (valofvarN)  --changing varName to ClassType
--      if(not("static"=allTables.at (prevTableIndex).entries.at (indexOfField).kind))  then
--      	 output.put_string("push "+ allTables.at (tableCounter).get_place(indexOfField.out)+"%N")--push varName
--      else
      	  vmFile.put_string("push "+ allTables.at (tableCounter).get_place(valofvarN)+"%N")--push varName
--      end

      vmFile.put_string("add%N")--does add between a and i to find the right place in the array
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeOp:=elem_type(curLine)
      valOp:=elem_val(curLine)
      result:=true
     end
    end
   end
   if(equal(valOp,"=")) then
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
     if(is_expression(typeOp,valOp)) then
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeOp:=elem_type(curLine)
      valOp:=elem_val(curLine)
      if equal(valOp, ";") then
       if(not(isAnArray))then

          if(destination.has_substring ("field"))
          then
          	  isThis:=false
          	  destination.replace_substring_all ("field","this")
          	  vmFile.put_string ("pop " + destination + "%N") --put the value to the destination
          else
          	 if(destination.has_substring ("local")or destination.has_substring ("argument") or destination.has_substring ("this"))
          	 then
          	 	 vmFile.put_string ("pop " + destination + "%N") --put the value to the destination
          	 else--destination.has_substring ("static")	
          	     isThis:=false
          	     vmFile.put_string ("pop static" + destination + "%N") --put the value to the destination
          	 end
           end

        result:=true
       else--if is an array
         vmFile.put_string ("pop temp 0%N") -- keeping the value to insert in a temp place
         vmFile.put_string ("pop pointer 1%N") -- pop the adrress for the value
         vmFile.put_string ("push temp 0%N")
         vmFile.put_string ("pop that 0%N")
         isAnArray:=false
       end--else
      end
     end
   end
  end
 end
 is_ifStatement(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp:STRING
  ifDegree:INTEGER
  else_flag:BOOLEAN
 do
  typeOp:=""
  valOp:=""
  else_flag:=false
  result:=true
  ifIndex:=ifIndex+1--counter for ifs
  ifDegree:=ifIndex
  --reading (
  curRow:=curRow+1
  curLine:=tArray.at (curRow)
  typeOp:=elem_type(curLine)
  valOp:=elem_val(curLine)
  if(is_openBrackets_Expression_closeBrackets(typeOp,valOp)) then
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   vmFile.put_string ("if-goto IF_TRUE")
   vmFile.put_integer (ifDegree)
   vmFile.put_string ("%N")
   vmFile.put_string ("goto IF_FALSE")
   vmFile.put_integer (ifDegree)
   vmFile.put_string ("%N")
   vmFile.put_string ("label IF_TRUE")
   vmFile.put_integer (ifDegree)
   vmFile.put_string ("%N")
   if(equal(valOp,"{")) then
    --statments
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    if(is_statements(typeOp,valOp)) then
     --reading }
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
     if(equal(valOp,"}")) then
      result:=true
      --reading else or something next
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeElse:=elem_type(curLine)
      valElse:=elem_val(curLine)
      if(equal(valElse,"else")) then
       else_flag:=true
       vmFile.put_string ("goto IF_END"+ifDegree.out+"%N")
       vmFile.put_string ("label IF_FALSE"+ifDegree.out+"%N")
       --reading {
       curRow:=curRow+1
       curLine:=tArray.at (curRow)
       typeOp:=elem_type(curLine)
       valOp:=elem_val(curLine)
       if(equal(valOp,"{")) then
        curRow:=curRow+1
        curLine:=tArray.at (curRow)
        typeOp:=elem_type(curLine)
        valOp:=elem_val(curLine)
        if(is_statements(typeOp,valOp)) then
         --reading }
         curRow:=curRow+1
         curLine:=tArray.at (curRow)
         typeOp:=elem_type(curLine)
         valOp:=elem_val(curLine)
         if(equal(valOp,"}")) then
          result:=true
         end
        end
       end
      else
       curRow:=curRow-1
       curLine:=tArray.at (curRow)
       typeOp:=elem_type(curLine)
       valOp:=elem_val(curLine)
      end
     end
    end
   end
  end
  if (else_flag)
  then
    vmFile.put_string ("label IF_END")
  else
    vmFile.put_string ("label IF_FALSE")
  end
  --output.put_integer (tableSize)
  vmFile.put_integer (ifDegree)
  vmFile.put_string ("%N")
  --ifIndex:=ifIndex-1
  else_flag:=false
 end

feature
 is_whileStatement(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp:STRING
  whileDegree:INTEGER
 do
  typeOp:=""
  valOp:=""
  result:=true

  whilePIndex:=whilePIndex+1
  whileDegree:=whilePIndex
  vmFile.put_string("label WHILE_EXP")
  vmFile.put_integer (whilePIndex)--like kinun pnimi. first the number of while from all whilePIndex exsited and then the numbeer of inner whilePIndex
  vmFile.put_string ("%N")
  -- reeading (
  curRow:=curRow+1
  curLine:=tArray.at (curRow)
  typeOp:=elem_type(curLine)
  valOp:=elem_val(curLine)
  if(is_openBrackets_Expression_closeBrackets(typeOp,valOp)) then
   vmFile.put_string ("not%N")---------after or before statments???$$$$
   vmFile.put_string ("if-goto WHILE_END")
   --output.put_integer (tableSize)
   vmFile.put_integer (whilePIndex)
   vmFile.put_string ("%N")
   -- {
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   if(equal(valOp,"{")) then
    --staements
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    if(is_statements(typeOp,valOp)) then
     -- }
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
     if(equal(valOp,"}")) then
      result:=true
     end
    end
   end
  end
  vmFile.put_string ("goto WHILE_EXP")
  vmFile.put_integer (whileDegree)---$$$
  --output.put_integer (tableSize)
  vmFile.put_string ("%N")
  vmFile.put_string ("label WHILE_END")
  --output.put_integer (tableSize)
  vmFile.put_integer (whileDegree)
  vmFile.put_string ("%N")

 end

feature
 is_doStatement(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp, typeClo, valClo:STRING
 do
  typeOp:=""
  valOp:=""
  typeClo:=""
  valClo:=""
  result:=true
  --subroutinecall
  curRow:=curRow+1
  curLine:=tArray.at (curRow)
  typeOp:=elem_type(curLine)
  valOp:=elem_val(curLine)
  if(is_subroutineCall1(typeOp,valOp)) then
   if(is_subroutineCall(typeOp,valOp)) then
    --reading ;
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeClo:=elem_type(curLine)
    valClo:=elem_val(curLine)
    if (equal(valClo,";")) then
     vmFile.putstring ("pop temp 0%N")------------$$$$$$$$$$$$
     result:=true
    end
   end
  end
 end
 is_returnStatement(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp:STRING
 do
  typeOp:=""
  valOp:=""
  result:=true
  --expression?
  curRow:=curRow+1
  curLine:=tArray.at (curRow)
  typeOp:=elem_type(curLine)
  valOp:=elem_val(curLine)
  if(not(equal(valOp,";"))) then
   if(is_expression(typeOp,valOp)) then
    --reading ;
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
   end
  else--if it a void function and there is only ; then there is no value to return
   vmFile.put_string ("push constant 0%N")
  end
  vmFile.put_string ("return%N")
 end
-------------------------------------------------------------------------------------
--program structure
 is_class(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp:STRING
 do
  typeOp:=""
  valOp:=""
  result:=true
  if(equal(val,"class")) then
   --reading className
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   if (is_className(typeOp,valOp)) then
    --insert a new nameTable for this class
    prevTable:="global"
    curTable:=valOp--saves the name of the current scop/table
    namesTable.make(curTable,prevTable)--valOp has className
    allTables.force (namesTable, allTables.count+1) --add the new table of this class to allTaables array
    tableCounter:=tableCounter+1
    tableSize:=tableSize+1
    --read {
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    if(equal(valOp,"{")) then
     --read classVarDec or subroutineDec or }
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
     --counterField:=0
     if(equal(valOp,"}")) then   --no class var dec orsubrouting dec
      result:=true
     else if(equal(valOp,"static") or equal(valOp,"field") or equal(valOp,"constructor") or equal(valOp,"function") or equal(valOp,"method")) then
      if(equal(valOp,"static") or equal(valOp,"field")) then
       from
       until (not(equal(valOp,"static")) and not(equal(valOp,"field")))
       loop
        if(is_classVarDec(typeOp,valOp)) then
         --read classVarDec or subroutineDec or }
         curRow:=curRow+1
         curLine:=tArray.at (curRow)
         typeOp:=elem_type(curLine)
         valOp:=elem_val(curLine)
        end
       end
      end
      --does it anyway! or after classVarDec or for itself
      from
      until (not(equal(valOp,"constructor") or equal(valOp,"function") or equal(valOp,"method")))
      loop
       if (equal(valOp,"constructor"))
       then
       	   isConstructor:=true
           constructorPrintTemp.append( ("push constant "+counterField.out+"%N")) --push 2 // two 16-bit words are required (x and y)
           constructorPrintTemp.append(("call Memory.alloc 1%N"))--// one argument
           constructorPrintTemp.append(("pop pointer 0%N"))
       end
       if (equal(valOp,"method"))
       then
       	   isMethod:=true
       	   methodPrintTemp:=""
           methodPrintTemp.append("push argument 0%N")
           methodPrintTemp.append("pop pointer 0%N")
           --להוסיף this
       end

       if(is_subroutineDec(typeOp,valOp)) then
        --read subroutineDec or }
        tableCounter:=allTables.count
        curRow:=curRow+1
        curLine:=tArray.at (curRow)
        typeOp:=elem_type(curLine)
        valOp:=elem_val(curLine)
       end
      end
     end
     end
    end
   end
  end
 end

feature
 is_classVarDec(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp,elemType,static_or_field:STRING
 do
  typeOp:=""
  valOp:=""
  elemType:=""
  static_or_field:=""
  result:=true
  if(equal(val,"static") or equal(val,"field")) then
   if(equal(val,"static")) then
    static_or_field:="static"
   else
    static_or_field:="field"
   end
   --reading token type
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   elemType.append(valOp)
   if (is_type(typeOp,valOp)) then
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    --elemType.append(typeOp)
    if(is_varName(typeOp,valOp)) then
     allTables.at (tableCounter).addEntry (valOp, elemType, static_or_field)--valOp has elem val => name that needs to be saved in table
     if(static_or_field.has_substring ("field"))
     then
        counterField:=counterField+1
     end
     --readingg ; or ,
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
     from
     until (equal(valOp,";"))
     loop
      if(equal(valOp,",")) then
       --read varName
       curRow:=curRow+1
       curLine:=tArray.at (curRow)
       typeOp:=elem_type(curLine)
       valOp:=elem_val(curLine)
       if(is_varName(typeOp,valOp)) then
        allTables.at (tableCounter).addEntry (valOp, elemType, static_or_field)
        if(static_or_field.has_substring ("field"))
        then
            counterField:=counterField+1
        end
        --read ; or ,
        curRow:=curRow+1
        curLine:=tArray.at (curRow)
        typeOp:=elem_type(curLine)
        valOp:=elem_val(curLine)
       end
      end
     end
    end
   end
  end
 end
 is_type(type:STRING val:STRING):BOOLEAN
 do
  result:=false
  if(equal(val,"int") or equal(val,"char") or equal(val,"boolean") or is_className(type, val)) then
   result:=true
  end
 end
 is_parameterList(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp, paramsType:STRING
 do
  typeOp:=""
  valOp:=""
  paramsType:=""
  result:=true
  if(is_type(type,val)) then
   result:=true
   paramsType:=val
   --read varName
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   if(is_varName(typeOp,valOp)) then
    allTables.at (tableCounter).addEntry (valOp, paramsType, "argument")--puts in the currentTable the varName->the new parameter in a list
    argCount:=argCount+1
    --read , or missin line
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    from
    until (not(equal(valOp,",")))
    loop
     --read type
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
     if(is_type(typeOp,valOp)) then
      paramsType:=valOp
      --read varName
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeOp:=elem_type(curLine)
      valOp:=elem_val(curLine)
      if(is_varName(typeOp,valOp)) then
       allTables.at (tableCounter).addEntry (valOp, paramsType, "argument")
       argCount:=argCount+1
       --read , or missin line
       curRow:=curRow+1
       curLine:=tArray.at (curRow)
       typeOp:=elem_type(curLine)
       valOp:=elem_val(curLine)
      end
     end
    end
    curRow:=curRow-1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
   end
  end
  vmFile.put_string ("function " + prevTable + "." + curTable + " ")--in right place???$$$$$$$$$$4
  --reStart the counters beacuse we r entering a new function
  ifIndex:=-1
  whilePIndex:=-1
 end

feature
 is_subroutineBody(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp:STRING
 do
  typeOp:=""
  valOp:=""
  localCount:=0
  result:=false
  if(equal(val,"{")) then
   result:=true
   --reading varDec or statements or }
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   if(not(equal(valOp,"}"))) then
    from
    until (not(equal(valOp,"var")))
    loop
     if(is_varDec(typeOp,valOp)) then
      --reading
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeOp:=elem_type(curLine)
      valOp:=elem_val(curLine)
     end
    end
    vmFile.put_string (localCount.out+"%N")
    if(isConstructor)
    then
   	   vmFile.put_string(constructorPrintTemp)
  	   isConstructor:=false
    end
    if(isMethod)
    then
       vmFile.put_string(methodPrintTemp)
  	   isMethod:=false
    end
    if(is_statements(typeOp,valOp)) then
     --reading }
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
    end
   end
   --changing the scope. gets to the prevous scope
   curTable:=findPrevTable (curTable)--gets back in scopes. current table will be the previous one
   prevTable:=findPrevTable (curTable)--the prev table of the prev table
   tableCounter:=index_of(curTable)--table counter is now the place of the new ntialized curTable
  end
 end

feature
 index_of(Tname:STRING):INTEGER
  do
   from result:=1
   until allTables.at (result).table_Name.is_equal (Tname) or result = allTables.count
   loop
    result:=result+1
   end
  end

findPrevTable(curTableName:STRING):STRING
  local
   b:INTEGER
  do
   result:="null"
   from b:=1
   until b = allTables.count+1
   loop
    if allTables.at (b).table_Name.is_equal (curTableName) then
     result:= allTables.at (b).prev_table_Name
    end
    b:=b+1
   end--loop
  end

feature
 is_varDec(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp,varType:STRING
 do
  typeOp:=""
  valOp:=""
  varType:=""
  result:=false
  if(equal(val,"var")) then
   result:=true
   --reading type
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   if(is_type(typeOp, valOp)) then
    varType:=valOp----------saves type of var(s) declared
    --reading varName
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    if(is_varName(typeOp,valOp)) then
     allTables.at (tableCounter).addEntry (valOp, varType, "var")
     --reading , or ;
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
     localCount:=localCount+1
     from
     until equal(valOp,";")
     loop
      if(equal(valOp,",")) then
       localCount:=localCount+1
       --reading varName
       curRow:=curRow+1
       curLine:=tArray.at (curRow)
       typeOp:=elem_type(curLine)
       valOp:=elem_val(curLine)
       if(is_varName(typeOp,valOp)) then
        allTables.at (tableCounter).addEntry (valOp, varType, "var")
        --reading "," or ,at the end, ";"
        curRow:=curRow+1
        curLine:=tArray.at (curRow)
        typeOp:=elem_type(curLine)
        valOp:=elem_val(curLine)
       end
      end
     end
    end
   end
  end
 end
 is_subroutineDec(type:STRING val:STRING):BOOLEAN
 local
  typeOp,valOp, ctr_meth_func:STRING
 do
  result:=false
  typeOp:=""
  valOp:=""
  ctr_meth_func:=""
  if(equal(val, "constructor") or equal(val, "function") or equal(val, "method")) then
   ctr_meth_func:=val
   result:=true
   --reading void or type
   curRow:=curRow+1
   curLine:=tArray.at (curRow)
   typeOp:=elem_type(curLine)
   valOp:=elem_val(curLine)
   if(equal(valOp,"void") or is_type(typeOp,valOp)) then
    --reading subroutineName
    curRow:=curRow+1
    curLine:=tArray.at (curRow)
    typeOp:=elem_type(curLine)
    valOp:=elem_val(curLine)
    if(is_subroutineName(typeOp,valOp)) then
     --creating table for this method/func/ctr
     prevTable:=curTable
     curTable:=valOp--subroutine name is the new scope/table now
     create namesTable.make (curTable,prevTable)
     allTables.force (namesTable, allTables.count+1)
     tableCounter:=tableCounter+1
     argCount:=0
     if(equal(ctr_meth_func,"method")) then
      allTables.at (tableCounter).addEntry ("this", prevTable, "argument")
      argCount:=1
     end
     --reading (
     curRow:=curRow+1
     curLine:=tArray.at (curRow)
     typeOp:=elem_type(curLine)
     valOp:=elem_val(curLine)
     if(equal(valOp,"(")) then
      --reading parameter list or )
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeOp:=elem_type(curLine)
      valOp:=elem_val(curLine)
      if(is_parameterList(typeOp, valOp))then
       if(not(equal(valOp,")"))) then
        --reading parameterList's ")"
        curRow:=curRow+1
        curLine:=tArray.at (curRow)
        typeOp:=elem_type(curLine)
        valOp:=elem_val(curLine)
       end
      end
      --reading is_subroutineBody
      curRow:=curRow+1
      curLine:=tArray.at (curRow)
      typeOp:=elem_type(curLine)
      valOp:=elem_val(curLine)
      if(is_subroutineBody(typeOp,valOp)) then
       result:=true
      end
     end
    end
   end
  end
 end
 space()--depth:INTEGER
 local
  v:INTEGER
 do
  from
   v:=0
  until
   v>=depth
  loop
   vmFile.put_character (' ')
   v:=v+1
  end
 end


end

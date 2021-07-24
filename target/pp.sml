structure PP : 
     
     sig val compile : tiger.exp -> string 
     
     end =

    struct
	
    val red = "\u001b[31;2m"
    val yellow = "\u001b[33m"
    val green = "\u001b[32m"
    val blue = "\u001b[34m"
    val magenta = "\u001b[35m"
    val cyan = "\u001b[36m"
    val reset_ansi_code = "\u001b[0m"

  structure Ast = tiger

fun compile (tiger_ast) : string =
 let

  val return_string = ""
  
  fun concat_pprint s =  return_string ^ s

  fun concat_OP (x) = case x of
        Ast.Plus => "+"
      | Ast.Mul => "*"
      | Ast.Div => "/"
      | Ast.Minus => "-"
      | Ast.Equal => "="
      | Ast.Greaterthan => ">"
      | Ast.Gequal => ">="
      | Ast.Notequal => "<>" 
      | Ast.Lessthan => "<"
      | Ast.Lequal => "<=" 
       
   fun indent (l) = 
      if l = 0 then concat_pprint("") 
      else concat_pprint(" ") ^ indent(l-1)

  fun pprint_list_with_sep d f seperator [] = concat_pprint("")
    | pprint_list_with_sep d f seperator [a] = (f(a,d+1))
    | pprint_list_with_sep d f seperator (a :: r) = (f(a,d+1))^concat_pprint(seperator)^(pprint_list_with_sep d f seperator r)
    

  fun concat_var(Ast.SimpleVar(s),d) = (concat_pprint (yellow)^concat_pprint(s)^concat_pprint(reset_ansi_code))
    | concat_var(Ast.FieldVar(v,s),d) = (indent (d)^concat_var(v,d+1)^concat_pprint(".")^concat_pprint(s))
    | concat_var(Ast.SubsVar(v,e),d) = (concat_var(v,d+1)^concat_pprint("[")^concat_exp(e,d+1)^concat_pprint("]"))
    | concat_var(Ast.Field_Subs(v1,v2),d) = (concat_var(v1,d+1)^concat_pprint(".")^concat_var(v2,d+1))
  
  and concat_exp(Ast.VarExp v, d) = (concat_var(v,d))
    | concat_exp(Ast.RecordExp{fields, ty},d) = let fun helper_printer((name,e),d) = (indent (d))^concat_pprint(name)^("=")^(concat_exp(e,d+1))
                                                in ((indent (d))^concat_pprint(ty^"{"))^ (pprint_list_with_sep d helper_printer "," fields)^concat_pprint("}")
                                                end
    | concat_exp(Ast.Nil, d) = (indent(d)^concat_pprint("nil"))
    | concat_exp(Ast.Op{leftOP,operator,rightOP},d) = (concat_exp(leftOP,0)^concat_pprint(yellow)^concat_pprint(concat_OP operator)^concat_pprint(reset_ansi_code)^concat_exp(rightOP,0))
    | concat_exp(Ast.SqExp l, d) = (indent(d)^pprint_list_with_sep d concat_exp ";" l^concat_pprint("\n"))
    | concat_exp(Ast.IntegerExp i, d) = concat_pprint(Int.toString i)
    | concat_exp(Ast.StringExp(s),d) = concat_pprint(s)
    | concat_exp(Ast.FunctionCallExp{name,args},d) = (indent(1)^concat_pprint(red)^concat_pprint(name)^concat_pprint(reset_ansi_code)^concat_pprint("(")^pprint_list_with_sep 0 concat_exp "," args^concat_pprint(")"))
    | concat_exp(Ast.ForExp{var=v,escape=b,init,final,body},d) = (indent(d)^concat_pprint(blue)^concat_pprint("for ")^concat_pprint(reset_ansi_code)^concat_pprint(v)^concat_pprint(" :=")^
		 concat_exp(init,d+1)^concat_pprint(blue)^concat_pprint(" to")^concat_pprint(reset_ansi_code)^concat_exp(final,d+1)^concat_pprint(blue)^concat_pprint(" do\n")^concat_pprint(reset_ansi_code)^
		 concat_exp(body,d+1))
    | concat_exp(Ast.AssgnExp{var=vr,expr=expr},d) = indent(d)^concat_var(vr,d+1)^concat_pprint(":=")^concat_exp(expr,d+1)
    | concat_exp(Ast.IfExp{predicate,then',else'},d) = (indent (d+1)^concat_pprint(reset_ansi_code)^concat_pprint(magenta)^concat_pprint"if "^concat_pprint(reset_ansi_code)^concat_exp(predicate,1)^concat_pprint(magenta)^concat_pprint("  then")^concat_pprint(reset_ansi_code)^concat_pprint("\n")^indent (d+1)^concat_exp(then',d+1)^concat_pprint("")^concat_pprint("\n"))^
		(case else' of NONE => (concat_pprint(""))
			| SOME e => (indent(d+1)^concat_pprint(magenta)^concat_pprint("  else\n")^concat_pprint(reset_ansi_code)^indent(d+1)^concat_exp(e,d+1)))
    | concat_exp(Ast.WhileExp{predicate,execute},d) = (indent(d)^concat_pprint(cyan)^concat_pprint("while")^concat_exp(execute,d+1)^concat_pprint(" do")^concat_pprint("\n")^concat_exp(execute,d+1))
    | concat_exp(Ast.BreakExp, d) = (indent(d)^concat_pprint(red)^concat_pprint("break"))
    | concat_exp(Ast.LetExp{decs,body},d) = (indent(d)^concat_pprint(green)^concat_pprint("let")^concat_pprint("\n")^concat_pprint(reset_ansi_code)^pprint_list_with_sep d dec "" decs^concat_pprint(green)^ concat_pprint("in")^concat_pprint("\n")^concat_pprint(reset_ansi_code)^concat_exp(body,d+1)^concat_pprint(green)^concat_pprint("end")^concat_pprint("\n")^concat_pprint(reset_ansi_code))
    | concat_exp(Ast.ArrayExp{name,size,init},d) = (indent(d)^concat_pprint(name)^concat_pprint("[")^concat_exp(size,d)^concat_pprint("]")^concat_pprint(" of ")^concat_exp(init,d+1))


  and dec(Ast.FunDecs l, d) = 
	    let fun field({name,escape,typ},d) = (indent(0)^concat_pprint(name)^concat_pprint(":")^concat_pprint(typ))
		fun f({name,params,result,body},d) =
		   (indent(1)^concat_pprint(green)^concat_pprint("function ")^concat_pprint(reset_ansi_code)^concat_pprint(name)^concat_pprint("(")^(pprint_list_with_sep d field "," params)^concat_pprint(")"))
		    ^(case result of NONE => concat_pprint("")
			 | SOME(s) => (concat_pprint(":")^concat_pprint(s)))^
		    concat_pprint(" =")^concat_exp(body,d+1)^concat_pprint("\n")
	     in indent(0)^ pprint_list_with_sep d f "" l 
	    end
    | dec(Ast.TypeDec l, d) = 
	    let fun helper({name,ty=t},d) = (indent(d)^concat_pprint(" type ")^concat_pprint(name)^concat_pprint("=")^ty(t,d+1))
	    in indent(d)^(pprint_list_with_sep d helper "\n" l)^concat_pprint("\n")
      end
    
    | dec(Ast.VarDec{name,escape,typ,init},d) =
	   (indent (d)^concat_pprint(blue)^ concat_pprint("var ")^concat_pprint(reset_ansi_code)^concat_pprint(yellow) ^concat_pprint(name)^concat_pprint(reset_ansi_code))      
             ^(case typ of NONE => concat_pprint("") 
		      | SOME(s)=> (concat_pprint(": ")^concat_pprint(s)))^
            concat_pprint(":=")^concat_exp(init,0)^concat_pprint("\n")

   
  and ty(Ast.nameTy(s), d) = (indent (d)^concat_pprint(s))
    | ty(Ast.arrayTy(s),d) = (indent(d)^concat_pprint("array of ")^concat_pprint(s))
    | ty(Ast.recordTy(l),d) = let fun f({name,escape,typ},d) =
			(concat_pprint(name)^concat_pprint(": ")^concat_pprint(typ))
	         in indent(d)^concat_pprint("{")^(pprint_list_with_sep d f "," l)^concat_pprint("}")
		  end
 in  concat_pprint("\n--------PRETTY-PRINT------\n\n")^concat_exp(tiger_ast,0) ^ concat_pprint("\n")
 end
 end
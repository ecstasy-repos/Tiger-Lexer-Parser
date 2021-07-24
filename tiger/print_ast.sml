structure AST_TREE : 
     sig val print : tiger.exp -> string 
	 
	 end =

struct

  structure Ast = tiger

fun print (tiger_ast) : string =
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

  fun pprint_list_without_sep d f  [] = concat_pprint("")
    | pprint_list_without_sep d f  [a] = (f(a,d+1))
    | pprint_list_without_sep d f  (a :: r) = (f(a,d+1))^(pprint_list_without_sep d f r)

  fun pprint_list_withh_sep d f sep [] = concat_pprint("")
    | pprint_list_withh_sep d f sep  [a] = (f(a,d+1))
    | pprint_list_withh_sep d f sep (a :: r) = (f(a,d+1))^concat_pprint(sep)^(pprint_list_withh_sep d f sep r)

  fun var(Ast.SimpleVar(s),d) = concat_pprint("SimpleVar(")^concat_pprint(s)^concat_pprint(")")
    | var(Ast.FieldVar(v,s),d) = concat_pprint("FieldVar(")^var(v,d+1)^concat_pprint(", ")^concat_pprint(s)^concat_pprint(")")
    | var(Ast.SubsVar(v,e),d) = concat_pprint("SubVar(")^var(v,d+1)^concat_pprint(",")^exp(e,d+1)^concat_pprint(")")
    | var(Ast.Field_Subs(v1,v2),d) = concat_pprint("FieldVar(")^concat_pprint("SubVar(")^var(v2,d+1)^concat_pprint(",")^concat_pprint(")")^(var(v1,d+1)^concat_pprint(",")^concat_pprint(")"))

  and exp(Ast.VarExp v, d) = (concat_pprint("VarExpr(")^var(v,d+1)^concat_pprint(")"))
    | exp(Ast.Nil, d) = concat_pprint("Nil")
    | exp(Ast.IntegerExp i, d) = (concat_pprint(" Int(")^concat_pprint(Int.toString i))^concat_pprint(")")
    | exp(Ast.StringExp(s),d) = (indent (d)^concat_pprint(" StringExpr(\"")^concat_pprint(s)^concat_pprint("\")"))
    | exp(Ast.FunctionCallExp{name,args},d) = (concat_pprint("  Fun(")^concat_pprint(name)^concat_pprint(",[")^(pprint_list_without_sep d exp args)^concat_pprint("])"))
    | exp(Ast.Op{leftOP,operator,rightOP},d) = (concat_pprint("  Op(")^(concat_OP operator)^concat_pprint(","))^exp(leftOP,d+1)^concat_pprint(",")^exp(rightOP,d+1)^concat_pprint(")")
    | exp(Ast.RecordExp{fields,ty},d) = let fun f((name,e),d) = (concat_pprint("(")^concat_pprint(name)^concat_pprint(",")^exp(e,d+1)^concat_pprint(")"))
	                                      in concat_pprint(" RecordExpr")^concat_pprint(ty)^concat_pprint(",[")^(pprint_list_without_sep d f fields)^concat_pprint("])") 
	                                      end
    | exp(Ast.SqExp l, d) = (indent (d))^concat_pprint(" SeqExpr(")^(pprint_list_withh_sep d exp "," l)^concat_pprint(")")
    | exp(Ast.AssgnExp{var=v,expr=e},d) = (concat_pprint(" AssignExp(")^var(v,d+1)^concat_pprint(",")^exp(e,d+1)^concat_pprint(")"))
    | exp(Ast.IfExp{predicate,then',else'},d) = (indent (d)^concat_pprint(" IfExp(\n")^exp(predicate,d+1)^concat_pprint(",\n")^exp(then',d+1))
		 ^(case else' of NONE => (concat_pprint(")"))
			| SOME e => (concat_pprint(",\n")^exp(e,d+1)^concat_pprint(")")))
    | exp(Ast.WhileExp{predicate, execute},d) = (indent (d))^concat_pprint("WhileExp(\n")^exp(predicate,d+1)^concat_pprint(",\n")^exp(execute,d+1)^concat_pprint(")")
    | exp(Ast.ForExp{var=v,escape=b,init,final,body},d) = (indent (d)^concat_pprint("ForExp(\n")^concat_pprint(v)^concat_pprint(",\n")^concat_pprint(Bool.toString (!b))^concat_pprint(",")^exp(init,d+1)^concat_pprint",\n")^exp(final,d+1)^concat_pprint(",\n")^exp(body,d+1)^concat_pprint(")")
    | exp(Ast.BreakExp, d) = (indent (d))^concat_pprint("BreakExp")
    | exp(Ast.LetExp{decs,body},d) = (indent (d)^concat_pprint("LetExp([")^(pprint_list_without_sep d dec decs)^concat_pprint("],\n")^exp(body,d+1)^concat_pprint(")"))
    | exp(Ast.ArrayExp {name,size,init},d) = (indent (d))^concat_pprint("ArrayExp(")^concat_pprint(name)^concat_pprint(",")^exp(size,d+1)^concat_pprint(",")^exp(init,d+1)^concat_pprint(")")

  and dec(Ast.FunDecs l, d) = 
	    let fun field({name,escape,typ},d) = (concat_pprint("(")^concat_pprint(name)^concat_pprint(",")^concat_pprint(Bool.toString(!escape))^concat_pprint(",")^concat_pprint(typ)^concat_pprint(")"))
		    fun f({name,params,result,body},d) = concat_pprint("(")^concat_pprint(name)^concat_pprint(",[")^
		    (pprint_list_without_sep d field params)^concat_pprint("],")
		  ^(case result of NONE => concat_pprint("NONE")
			    | SOME(s) => concat_pprint("SOME(")^concat_pprint(s)^concat_pprint(")")^concat_pprint(",\n")^exp(body,d+1)^concat_pprint(")"))
	     in (indent (d))^concat_pprint("FunctionDec[")^(pprint_list_without_sep d f l )^concat_pprint("]")
	    end
    | dec(Ast.VarDec{name,escape,typ,init},d) = (indent (d))^concat_pprint("VarDec(")^concat_pprint(name)^concat_pprint(",")^concat_pprint(Bool.toString (!escape))^concat_pprint(",")
	    ^(case typ of NONE => concat_pprint("NONE),\n") 
		      | SOME(s)=> concat_pprint("SOME(")^concat_pprint(")")^concat_pprint(",")^exp(init,d+1)^concat_pprint(")\n"))

    | dec(Ast.TypeDec l, d) =  let fun tdec({name,ty=t},d) = (indent (d))^concat_pprint("(")^concat_pprint(name)^concat_pprint(",")^ty(t,d+1)^concat_pprint(")")
	                             in (indent (d))^concat_pprint("TypeDec[")^(pprint_list_withh_sep d tdec "," l)^concat_pprint("]")
                               end
   
  and ty(Ast.nameTy(s), d) = (indent (d)^concat_pprint("NameTy(")^concat_pprint(")"))
    | ty(Ast.recordTy l, d) =  let fun f({name,escape,typ},d) = (concat_pprint("(")^concat_pprint(name)^concat_pprint(",")^concat_pprint(Bool.toString(!escape))^concat_pprint(",")^concat_pprint(typ)^concat_pprint(")"))
	                             in concat_pprint("  RecordTy[")^(pprint_list_withh_sep d f "," l)^concat_pprint("]")
		                           end
    | ty(Ast.arrayTy(s),d) = (indent (d))^concat_pprint("ArrayTy(")^concat_pprint(s)^concat_pprint(")")

 in  concat_pprint("\n--------AST-TREE------\n\n")^exp(tiger_ast,0)^concat_pprint("\n\n\n")
end
end

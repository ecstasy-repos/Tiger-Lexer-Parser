
structure tiger = struct
    
    type ID = string
    type field = {name: ID, escape: bool ref, typ: ID}

    datatype ast =  exp
    and Var = SimpleVar of ID
             | FieldVar of Var*ID
             | SubsVar of Var*exp
             | Field_Subs of Var*Var

    and type_id = ID
    and typefield = tfield of type_id list

    and ty = nameTy of ID 
            | arrayTy of ID 
            | recordTy of field list


    (* and classfield= VarDec of {type_ID: type_id}
                    | Method of { method_id   : ID
                                , method_args : typefield list
                                , method_out_type : type_id option
                                , method_body : exp
                                } *)

    and exp = Nil
            | VarExp of Var
            | Op of {leftOP: exp, operator:BinOp, rightOP: exp}
            | IntegerExp of int
            | StringExp of string
            | ArrayExp of {name: ID, size: exp,init: exp}
            | IfExp of {predicate : exp, then': exp, else' : exp option}
            | WhileExp of {predicate: exp, execute: exp}
            | FunctionCallExp of {name: ID, args: exp list}
            | RecordExp of {fields: (ID*exp) list, ty: ID}
            | SqExp of exp list
            | AssgnExp of {var: Var, expr: exp}
            | ForExp of {var: ID, escape: bool ref, init: exp, final: exp, body: exp}
            | LetExp of {decs: dec list, body: exp}
            | BreakExp
    
    and dec = FunDecs of fundec list
                | VarDec of {name: ID, escape: bool ref, typ: ID option, init: exp}
                | TypeDec of {name: ID, ty: ty} list

    and BinOp = Plus
            | Minus  
            | Mul
            | Div
            | Greaterthan
            | Lessthan
            | Lequal
            | Gequal
            | Equal
            | Notequal  

   withtype fundec = {
                        name: ID,
                        params: field list,
                        result: (ID) option,
                        body: exp
                     }

end
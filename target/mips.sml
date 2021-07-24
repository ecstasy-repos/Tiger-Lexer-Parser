 structure MIPS = struct

 datatype regs = ZERO | AT | v0 | v1 | a0 | a1 | a2 | a3 | t0 | t1 | t2 | t3 | t4 | t5 | t6 | t7 | s0 | s1 | s2 | s3 | s4 | s5 | s6 | s7 | t8 | t9 | k0 | k1 | gp | sp | fp | ra 

 datatype  ('l,'t) inst = add of 't * 't * 't
                        | addi of 't * 't * 'l
                        | addu of 't * 't * 't
                        | sub of 't * 't * 't
                        | mul of 't * 't * 't
                        | div of 't * 't
                        | la of 't * 'l
                        | li of 't * 'l
                        | lw of 't * 'l
                        | sw of 't * 'l 
                        | move of 't * 't
                        | jump of 'l
                        | jumpreg of 't
                        | beq of 't * 't * 'l
                        | blt of 't * 't * 'l
                        | bgt of 't * 't * 'l
                        | bge of 't * 't * 'l
                        | bne of 't * 't * 'l


 datatype ('l , 't) inst = la of 't * 'l

 datatype Label = LUser of string
                | LTemp of int

(*

 The code generated will be of type (Label, Temp) inst

 fun toString (LUser s ) = "_" ^ s
   | toString (LTemp i)  = "_" ^ Int.toString i

*)

 (* actual code that SPIM can understand is (string, reg) inst *)
end
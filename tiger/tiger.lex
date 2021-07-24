type lineNo            = int
type pos               = lineNo  
val  lineRef : pos ref = ref 0   

fun updateLine n      = lineRef := !(lineRef) + n

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token


fun lineRange l r = "line " ^ l
				  (* (else ("line " ^ Int.toString l ^ "-" ^ Int.toString r)) *)
fun error (e,l,r) = TextIO.output(TextIO.stdErr, lineRange l r ^ ":" ^ e ^ "\n")


fun eof   ()      = Tokens.EOF (!lineRef,!lineRef)


fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt        = toSigned o String.explode

val newlineCount = List.length o List.filter (fn x => x = #"\n") o String.explode

val comm_H_ref = ref 0

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));
ws    = [\ \n \t];
digit = [0-9]+;
identifier = [a-zA-Z][a-zA-Z0-9_]* ;
str =[\"][^\"]*[\"];
COMMENT = \/\*[^*]*[a-zA-Z0-9_\n\t\n\f\s" '$!.]*[^*]*\*\/;


%%

{ws}+         => ( lex() );
\n({ws}\n)  => ( let val old = !lineRef
		   in updateLine (newlineCount yytext); Tokens.NEWLINE (old, !lineRef)
		   end
		 );


{digit}+       => (Tokens.CONST (toInt yytext, !lineRef, !lineRef) );
{COMMENT}      => (continue());
{str}+        => ( Tokens.STRING(yytext, !lineRef, !lineRef));
"var"          => (Tokens.VAR(yypos, yypos+3));
"type"         => (Tokens.TYPE(yypos, yypos+4));
"end"          => (Tokens.END(yypos, yypos+3));
"in"           => (Tokens.IN(yypos, yypos+2));
"while"        => (Tokens.WHILE(yypos, yypos+5));
"else"         => (Tokens.ELSE(yypos, yypos+4));
"then"         => (Tokens.THEN(yypos, yypos+4));
"if"           => (Tokens.IF(yypos, yypos+2));
"nil"          => (Tokens.NIL(yypos, yypos+3));
"let"          => (Tokens.LET(yypos, yypos+3));
"do"           => (Tokens.DO(yypos, yypos+2));
"to"           => (Tokens.TO(yypos, yypos+2));
"for"          => (Tokens.FOR(yypos, yypos+3));
"function"     => (Tokens.FUNCTION(yypos, yypos+8));
"break"        => (Tokens.BREAK(yypos, yypos+5));
"of"           => (Tokens.OF(yypos, yypos+2));
"array"        => (Tokens.ARRAY(yypos, yypos+5));

"."            => ( Tokens.DOT (!lineRef,!lineRef));
")"            => ( Tokens.RP (!lineRef,!lineRef));
"{"            => ( Tokens.LBRACKET (!lineRef,!lineRef));
"}"            => ( Tokens.RBRACKET (!lineRef,!lineRef));
"["            => ( Tokens.LBIGBKT (!lineRef,!lineRef));
"]"            => ( Tokens.RBIGBKT (!lineRef,!lineRef));
">"            => (Tokens.GREATER (!lineRef,!lineRef));
">="           => ( Tokens.GEQUAL (!lineRef,!lineRef));
"+"            => ( Tokens.PLUS  (!lineRef,!lineRef) );


"("            => ( Tokens.LP (!lineRef,!lineRef));
"<>"           => (Tokens.NEQUAL (!lineRef,!lineRef));
"<"            => ( Tokens.LESS(!lineRef,!lineRef));
"<="           => ( Tokens.LEQUAL (!lineRef,!lineRef));

"&"            => ( Tokens.AND (!lineRef,!lineRef));
"|"            => ( Tokens.OR (!lineRef,!lineRef));
"="            => ( Tokens.EQUAL (!lineRef,!lineRef));
"-"            => ( Tokens.MINUS  (!lineRef,!lineRef) );
"*"            => ( Tokens.MUL (!lineRef,!lineRef) );
"/"            => ( Tokens.DIV (!lineRef,!lineRef) ); 
":"            => ( Tokens.COLON (!lineRef,!lineRef));
";"            => ( Tokens.SEMICOLON (!lineRef,!lineRef));
":="           => ( Tokens.ASSIGN (!lineRef,!lineRef));
","            => ( Tokens.COMMA (!lineRef,!lineRef));
{identifier}   => ( Tokens.ID (yytext,!lineRef,!lineRef));
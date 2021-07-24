WEEK1 : I have created local and remote repository and added SSH keys as well,After that I have created tc.sml file to print "hello world", all managed with Makefile with two targets, all(for compilation), and clear(to remove the executable).

WEEK2 : I have changed the ast.sml file to change the abstract syntax which is required to do part (a.) where we need to make to it support "/" division. Also, I have changed the expr.grm, expr.lex and rp.lex files inorder to create token and calling execution of division and allowing to support parentheses.Besides, I have added test-cases in test.inp and test.expr for testing purpose.

WEEK3 : I have added two files ast.sml and mips.sml in two different folder named tiger and source respectively and in ast.sml i have added the abstract syntax datatype capture and constructors for tiger language and in mips.sml I have added names of registers as given in the reference booklet and also instructions of mips.

WEEK4: I have added two files i.e tiger.grm and tiger.lex in the tiger folder itself. In the .grm file, i have added terminals and non terminals with the grammar. Also, in the .lex file I have added the TOKEN creation part.

WEEK5: I have added few more expr kinds when i realized it's need. In grm file, i added few more non terminals, but started getting shift/reduce conflict errors, tried hard to remove still few are remaining. In lex file, i have added some more token creations as per newly added stuff.

WEEK6: I finally removed to reduce/shift conflict and completed the .lex and .grm files also tried to print the abstract syntax for function, assignment, let. Everything works well and good. I am in progress with the pretty printer. Hopefully it'll be completed by tommmorow.

WEEK7: I have finally completed the pretty-printing. I have created tc.mlb to fast build and used that in makefile. I created tc.sml to call the compile function of pp.sml and also to print into console. I have one bug(just warning)as i was not able to print recordType So,i skipped it for now and as i have defined the recordty in ast as well as grammar so, i am getting warning saying non-exhaustive function.

WEEK8: Refactored the code of parser and removed the non-exhaustivity by adding the recordtype. It is now printing it correctly for my own made testcase but still some issue in indentation. 

WEEK9: I tested all the testcases on the moodle. Implemented comments and some small minute feature i missed at the starting realised after watching the testcases.

(LAB3, 849476165fe6bdecbf727719fd408eeeca9444db)
(LAB4, 23282ca4cc0f2272746d35ebfd885baa42a99eb4)

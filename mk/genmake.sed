#!/usr/bin/sed -f
1i\
\
include head.mk\

s#[-*0-9a-zA-Z_.$()]*\.o[ 	\]#$(OBJARCH)/&#g
s#[-*0-9a-zA-Z_.$()]*\.o:#$(OBJARCH)/&#g
s#[-*0-9a-zA-Z_.$()]*\.o$#$(OBJARCH)/&#g
s#[-*0-9a-zA-Z_.$()/]*\.a[ 	\]#$(OBJARCH)/&#g
s#[-*0-9a-zA-Z_.$()/]*\.a:#$(OBJARCH)/&#g
s#[-*0-9a-zA-Z_.$()/]*\.a$#$(OBJARCH)/&#g
s#\$(OBJARCH)/\$(LIBDIR)#$(LIBDIR)#g
s#lex\.yy\.c#$(OBJARCH)/&#g
s#y\.tab\.c#$(OBJARCH)/&#g
s#y\.tab\.h#$(OBJARCH)/&#g
s#\$(YACC)#& -b $(OBJARCH)/y#g
$a\
\
include tail.mk\


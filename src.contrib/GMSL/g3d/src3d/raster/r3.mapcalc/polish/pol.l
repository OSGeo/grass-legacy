T  [^ ^#@,"'\t\n()\[\]+\-*/%><!=&|]+
%%

[ \t]  ;         /* ignore blanks and tabs */

\&\&              { return AND; }
\|\|              { return OR; }

\>                { return GT; }
\>\=              { return GE; }
\<                { return LT; }
\<\=              { return LE; }
\=\=              { return EQ; }
\!\=              { return NE; }

[0-9]+\.[0-9]* |
[0-9]*\.[0-9]+ {
		yylval = store (yytext);
		return FLOAT ;
	       }
[0-9]*	    {
		yylval = atoi (yytext);
		return INTEGER;
	    }

[rR]#       { return COLOR_RED;}
[gG]#       { return COLOR_GREEN;}
[bB]#       { return COLOR_BLUE;}
#           { return COLOR_GRAY;}

{T}/[ \t]*\(  {
	        yylval = store (yytext);
		begin_function();
		return FUNCTION; 
	    }

{T}	             {
		      yylval = store (yytext);
		      return NAME; 
	             }

\"[^"]+\"   |
\'[^']+\'   {
		yytext[yyleng-1] = 0;
		yylval = store (yytext+1);
		return STRING; 
	    }

.      |
\n          { return (int) *yytext; }
%%

%{
#define YYSTYPE long
#include <stdio.h>
#include <ctype.h>
%}

%token NUM
%left '-' '+'
%left '*' '/'
%left NEG     /* negation--unary minus */
%right '^'    /* exponentiation        */

%%
input:    /* empty string */
        | input line
;
line:     '\n'
        | exp '\n'  { printf ("%ld\n", $1); }
;
exp:      NUM                { $$ = $1;         }
        | exp '+' exp        { $$ = $1 + $3;    }
        | exp '-' exp        { $$ = $1 - $3;    }
        | exp '*' exp        { $$ = $1 * $3;    }
        | exp '/' exp        { $$ = $1 / $3;    }
        | '-' exp  %prec NEG { $$ = -$2;        }
        | '(' exp ')'        { $$ = $2;         }
;
%%

yylex ()
{
	int c;

	/* skip white space  */
	while ((c = getchar ()) == ' ' || c == '\t')
		;
	/* process numbers   */
	if (c == '.' || isdigit (c)) {
		ungetc (c, stdin);
		scanf ("%ld", &yylval);
		return NUM;
	}
	if (c == EOF) return 0;
	return c;
}

yyerror (s)
	char *s;
{
	printf ("%s\n", s);
}

main()
{
	yyparse();
	exit(0);
}

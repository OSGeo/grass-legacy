
%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mapcalc.h"

#define YYDEBUG 1

%}

%union {
	int ival;
	double fval;
	char *sval;
	expression *exp;
	expr_list *list;
}

%token <sval> VARNAME
%token <sval> NAME
%token <sval> STRING
%token <ival> INTEGER
%token <fval> FLOAT
%token <fval> DOUBLE

%token GT GE LT LE EQ NE AND OR

%type <exp> exp
%type <exp> exp_atom
%type <exp> exp_neg
%type <exp> exp_pow
%type <exp> exp_mul
%type <exp> exp_add
%type <exp> exp_cmp
%type <exp> exp_log
%type <exp> exp_cond
%type <exp> exp_let

%type <exp> atom_var
%type <exp> atom_map
%type <exp> atom_func

%type <ival> mapmod
%type <ival> index

%type <sval> map

%type <list> expr_list

%type <list> stmts
%type <list> program

%{

static expr_list *result;

extern int yylex(void);

int yyparse(void);
void yyerror(char *s);

%}

%%

program		: stmts			{ $$ = result = $1;		}

stmts		: exp_let		{ $$ = list($1,NULL);		}
		| exp_let ';'		{ $$ = list($1,NULL);		}
		| exp_let ';' stmts	{ $$ = list($1,$3);		}
		;

map		: STRING
		| NAME
		| NAME '@' NAME		{ $$ = composite($1,$3);	}
		;

mapmod		: '@'			{ $$ = '@';			}
		| 'r'			{ $$ = 'r';			}
		| 'g'			{ $$ = 'g';			}
		| 'b'			{ $$ = 'b';			}
		| '#'			{ $$ = '#';			}
		| 'y'			{ $$ = 'y';			}
		| 'i'			{ $$ = 'i';			}
		;

index		: INTEGER
		| '-' INTEGER		{ $$ = -$2;			}
		;

expr_list	: exp			{ $$ = singleton($1);		}
		| exp ',' expr_list	{ $$ = list($1, $3);		}
		;

atom_var	: VARNAME		{ $$ = variable($1);		}
		;

atom_map	: map '[' index ']'	{ $$ = mapname($1,'M',$3,0);	}
		| map '[' index ',' index ']'
					{ $$ = mapname($1,'M',$3,$5);	}
		| map			{ $$ = mapname($1,'M',0,0);	}
		| mapmod map '[' index ']'
					{ $$ = mapname($2,$1,$4,0);	}
		| mapmod map '[' index ',' index ']'
					{ $$ = mapname($2,$1,$4,$6);	}
		| mapmod map		{ $$ = mapname($2,$1,0,0);	}
		;

atom_func	: NAME '(' ')'		{ $$ = function($1, NULL);	}
		| NAME '(' expr_list ')'
					{ $$ = function($1, $3);	}
		;

exp_atom	: '(' exp ')'		{ $$ = $2;			}
		| atom_var
		| atom_map
		| atom_func
		| INTEGER		{ $$ = constant_int($1);	}
		| FLOAT			{ $$ = constant_float($1);	}
		| DOUBLE		{ $$ = constant_double($1);	}
		;

exp_neg		: exp_atom
		| '-' exp_atom		{ $$ = function("neg",singleton($2));	}
		;

exp_pow		: exp_neg
		| exp_neg '^' exp_pow	{ $$ = function("pow",pair($1,$3));	}
		;

exp_mul		: exp_pow
		| exp_mul '*' exp_pow	{ $$ = function("mul",pair($1,$3));	}
		| exp_mul '/' exp_pow	{ $$ = function("div",pair($1,$3));	}
		| exp_mul '%' exp_pow	{ $$ = function("mod",pair($1,$3));	}
		;

exp_add		: exp_mul
		| exp_add '+' exp_mul	{ $$ = function("add",pair($1,$3));	}
		| exp_add '-' exp_mul	{ $$ = function("sub",pair($1,$3));	}
		;

exp_cmp		: exp_add
		| exp_cmp GT exp_add	{ $$ = function("gt", pair($1,$3));	}
		| exp_cmp GE exp_add	{ $$ = function("ge", pair($1,$3));	}
		| exp_cmp LT exp_add	{ $$ = function("lt", pair($1,$3));	}
		| exp_cmp LE exp_add	{ $$ = function("le", pair($1,$3));	}
		| exp_cmp EQ exp_add	{ $$ = function("eq", pair($1,$3));	}
		| exp_cmp NE exp_add	{ $$ = function("ne", pair($1,$3));	}
		;

exp_log		: exp_cmp
		| exp_log OR exp_cmp	{ $$ = function("or", pair($1,$3));	}
		| exp_log AND exp_cmp	{ $$ = function("and",pair($1,$3));	}
		;

exp_cond	: exp_log
		| exp_log '?' exp_cond ':' exp_cond
					{ $$ = function("if",triple($1,$3,$5));	}
		;

exp_let		: exp_cond
		| NAME '=' exp_let	{ $$ = binding($1,$3);
						define_variable($$);	}
		;

exp		: exp_let
		;

%%

void yyerror(char *s)
{
	fprintf(stderr, "%s\n", s);
}

static expr_list *parse(void)
{
#if 0
	yydebug = 1;
#endif
	if (yyparse() != 0)
	{
		fprintf(stderr, "Parse error\n");
		return NULL;
	}
	return result;
}

expr_list *parse_string(const char *s)
{
	initialize_scanner_string(s);
	return parse();
}

expr_list *parse_stream(FILE *fp)
{
	initialize_scanner_stream(fp);
	return parse();
}


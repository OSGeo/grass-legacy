
%{
#include <stdlib.h>
#include <string.h>

#include "mapcalc.h"

#include "y.tab.h"

static const char *input_string;
static int input_length;
static int input_offset;

#define YY_INPUT(buf,result,max_size)				\
{								\
	const char *next = input_string + input_offset;		\
	int left = input_length - input_offset;			\
	if (left <= 0)						\
		result = YY_NULL;				\
	else {							\
		result = (left > max_size) ? max_size : left;	\
		memcpy(buf, next, result);			\
		input_offset += result;				\
	}							\
}

%}

W		[ \t\r]+
N		[^ ^#@,"'\000-\037()\[\]+\-*/%><!=&|?:;]+
I		[0-9]+
E		[eE][-+]?[0-9]+

%%

{W}		;	/* ignore white space */

{I}"."{I}?{E}?	|
"."{I}{E}?	{
			yylval.fval = atof(yytext);
			return FLOAT ;
		}

{I}"."{I}?[eE]	|
{I}"."{I}?[eE][-+]	|
"."{I}?[eE]	|
"."{I}?[eE][-+]	{
			fprintf(stderr, "unterminated FP constant\n");
			yyterminate();
		}

{I}		{
			yylval.ival = atoi(yytext);
			return INTEGER;
		}

{N}		{
			yylval.sval = strdup(yytext);
			return is_var(yytext) ? VARNAME : NAME; 
		}

\"[^"]+\"	|
\'[^']+\'	{
			yylval.sval = strdup(yytext + 1);
			yylval.sval[yyleng - 1] = '\0';
			return STRING; 
		}

\"[^"]+		|
\'[^']+		{
			fprintf(stderr, "unterminated string\n");
			yyterminate();
		}

"+"		{	return '+';	}
"-"		{	return '-';	}
"*"		{	return '*';	}
"/"		{	return '/';	}
"^"		{	return '^';	}
"%"		{	return '%';	}

"&&"		{	return AND;	}
"||"		{	return OR;	}

">"		{	return GT;	}
">="		{	return GE;	}
"<"		{	return LT;	}
"<="		{	return LE;	}
"=="		{	return EQ;	}
"!="		{	return NE;	}

"?"		{	return '?';	}
":"		{	return ':';	}

[rR]"#"		{	return 'r';	}
[gG]"#"		{	return 'g';	}
[bB]"#"		{	return 'b';	}
"#"		{	return '#';	}

"@"		{	return '@';	}

"("		{	return '(';	}
")"		{	return ')';	}

"["		{	return '[';	}
"]"		{	return ']';	}

"="		{	return '=';	}

","		{	return ',';	}

%%

int yywrap(void)
{
    return 1;
}

void initialize_scanner(const char *s)
{
	input_string = s;
	input_length = strlen(s);
	input_offset = 0;
}


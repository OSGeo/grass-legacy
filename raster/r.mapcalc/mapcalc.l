
%{
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mapcalc.h"

#include "y.tab.h"

static const char *input_string;
static int input_length;
static int input_offset;
static FILE *input_stream;

static int get_input_string(char *buf, int max_size)
{
	const char *next = input_string + input_offset;
	int left = input_length - input_offset;
	int result;

	if (left <= 0)
		return YY_NULL;

	result = (left > max_size) ? max_size : left;
	memcpy(buf, next, result);
	input_offset += result;

	return result;
}

static int get_input_stream(char *buf, int max_size)
{
	if (feof(input_stream))
		return YY_NULL;

	if (isatty(fileno(input_stream)))
		fputs("mapcalc> ", stderr);

	if (!fgets(buf, max_size, input_stream))
		return YY_NULL;

	return strlen(buf);
}

#define YY_INPUT(buf,result,max_size)				\
{								\
	result = input_string					\
		? get_input_string(buf, max_size)		\
		: get_input_stream(buf, max_size);		\
}

%}

W		[ \t\r]+
N		[^ ^#@,"'\000-\037()\[\]+\-*/%><!=&|?:;]+
I		[0-9]+
E		[eE][-+]?[0-9]+
F		[fF]

%%

{W}		;	/* ignore white space */

"\\"{W}?"\n"	;	/* ignore backslash-newline */

{I}"."{I}?{E}?{F}	|
"."{I}{E}?{F}	{
			yylval.fval = atof(yytext);
			return FLOAT;
		}

{I}"."{I}?{E}?	|
"."{I}{E}?	{
			yylval.fval = atof(yytext);
			return DOUBLE;
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
			yylval.sval[yyleng - 2] = '\0';
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
[yY]"#"		{	return 'y';	}
[iI]"#"		{	return 'i';	}

"@"		{	return '@';	}

"("		{	return '(';	}
")"		{	return ')';	}

"["		{	return '[';	}
"]"		{	return ']';	}

"="		{	return '=';	}

","		{	return ',';	}

"\n\n"		{	yyterminate();	}

";"		|
"\n"		{	return ';';	}

%%

int yywrap(void)
{
	return 1;
}

void initialize_scanner_string(const char *s)
{
	input_string = s;
	input_length = strlen(s);
	input_offset = 0;
}

void initialize_scanner_stream(FILE *fp)
{
	input_stream = fp;
	setbuf(fp, NULL);
}


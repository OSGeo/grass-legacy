%{
extern char *falloc();
#include "gis_pars.h"
#ifdef getc
#undef getc
#endif
#define getc mygetc
long atol();
%}

D       [-0-9]
N        [A-Za-z_][A-Za-z_.0-9]*
%%

{D}+    {
		yylval.long_val = atol(yytext) ;
		return (LONG_NUM) ;
	}

[?]     |
help	{
		return(HELP_TKN) ;
	}

colors	|
color	{
		return(COLR_TKN) ;
	}

type	|
show	|
list	{
		return(LST_TKN) ;
	}

"q"     |
stop    |
exit    |
quit    |
bye     |
BYE     {
		return(QUIT_TKN) ;
	}

print   {
		return(PRT_TKN) ;
	}

erase   {
		return(ERAS_TKN) ;
	}

set		|
assign  {
		return(ASG_TKN) ;
	}

table   	|
analyses	|
analysis  {
		return(ANAL_TKN) ;
	}

remove		|
unchoose    {
		return(UNCH_TKN) ;
	}

pick	|
choose  {
		return(CHOS_TKN) ;
	}

get		|
recover {
		return(REC_TKN) ;
	}

execute    {
		return(EXEC_TKN) ;
	}

saved	|
save    {
		return(SAV_TKN) ;
	}

maps   {
		return(MAP_TKN) ;
	}

categories    {
		return(CATS_TKN) ;
	}

mult    |
multiply	{
		return(MULT_TKN) ;
	}

add    {
		return(ADD_TKN) ;
	}

"-"     |
for		|
the		|
please	|
to		{
	}

{N}     {
		yylval.str_val = (char *)falloc(yyleng + 1, sizeof(char),
			"falloc: lexical analyser call failed") ;
		strcpy (yylval.str_val, yytext) ;
		return (NAM_STR) ;
	}

\"[ \t]*{N}[ \t]*\" |
\'[ \t]*{N}[ \t]*\' |
\([ \t]*{N}[ \t]*\) {
		yytext[yyleng-1] = 0;
		yylval.str_val = (char *)falloc(yyleng, sizeof(char),
			"falloc: lexical analyser call failed") ;
		strcpy (yylval.str_val, yytext+1) ;
		G_strip (yylval.str_val);
		return (NAM_STR) ;
	}

[:"'()]	{
		/*
		return (PAREN_TKN) ;
		*/
	}


[ ]     {
	}

[\t]    {
	}

[\n]    {
		return (LINE_TKN) ;
	}
%%
yywrap()
{
	return(1) ;
}

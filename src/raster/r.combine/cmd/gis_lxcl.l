%{
#include "gis_pars.h"
#ifdef getc
#undef getc
#endif
#define getc mygetc

extern FILE *e_sav_fil ;
FILE *newinput ;
%}

D       [0-9]
%%

{D}+    {
		yylval.int_val = atoi(yytext) ;
		return (INUMBER) ;
	}

red  	|
RED  	{
		yylval.int_val = 1 ;
		return (INUMBER) ;
	}

yellow	|
yel   	|
YELLOW	{
		yylval.int_val = 2 ;
		return (INUMBER) ;
	}

blue  	|
blu  	|
BLUE  	{
		yylval.int_val = 3 ;
		return (INUMBER) ;
	}

grey  	|
gray  	|
GREY  	|
GRAY  	{
		yylval.int_val = 4 ;
		return (INUMBER) ;
	}

hist	|
history	|
HISTORY	{
		return(HST_TKN) ;
	}

erase	|
ERASE	{
		return(ERA_TKN) ;
	}

help	|
HELP	|
"?"		{
		return(HLP_TKN) ;
	}

window  |
WINDOW  {
		return(WIN_TKN) ;
	}

"q"     |
stop    |
STOP    |
exit    |
EXIT    |
quit    |
QUIT    |
bye     |
BYE     {
		return(BYE_TKN) ;
	}

CATEGORIES      |
categories      |
cats    |
CATS    {
		return(CATS_TKN) ;
	}

"&"     |
"&&"    |
and     |
AND     {
		return(AND_TKN) ;
	}

"|"     |
"||"    |
or      |
OR      {
		return(OR_TKN) ;
	}

"~"     |
not     |
NOT     {
		return(NOT_TKN) ;
	}

grp     |
group   |
GRP     |
GROUP   {
		return(GRP_TKN) ;
	}

exp     |
expr    |
EXP     |
EXPR    {
		return(EXPR_TKN) ;
	}

"-"     |
rng     |
range   |
RNG     |
RANGE   {

		return(RANGE_TKN) ;
	}

nam     |
name    |
NAME    {
		return(NAM_TKN) ;
	}

overlay |
over    |
OVER    {
		return(OVR_TKN) ;
	}

COVER   |
cover   |
paint   {
		return(COV_TKN) ;
	}

[A-Za-z_][-A-Za-z_.0-9]* {
		yylval.str_val = (char *)falloc(yyleng + 1, sizeof(char),
			"falloc: lexical analyser call failed") ;
		strncpy (yylval.str_val, yytext, yyleng + 1) ;
		return (NAM_STR) ;
	}

[ ]     {
	}

[\t]    {
	}

[\n]    {
	}

"("     {
		return (LP) ;
	}

")"     {
		return (RP) ;
	}

"#"     { char achar ;
		printf("# ") ;
		while ((achar=yyinput()) != 012)
			putchar(achar) ;
		printf("\n") ;
	}


"<"" "?[A-Za-z_0-9][A-Za-z_.0-9]* {
		switch_input(yytext) ;
	}

"!".+	{ 
		shell_escape(yytext) ;
	}
%%
switch_input(string)
	char *string;
{
	char filename[128] ;

	sscanf(string,"< %s",filename) ;
	if ((newinput = fopen(filename,"r") ) != NULL)
	{
		yyin = newinput ;
		fprintf(stderr,"\nTaking commands from: %s\n", filename) ;
	}
	else
	{
		fprintf(stderr,"\nCan't take commands from: %s\n", filename) ;
		fprintf(stderr,"\n[Continue]:") ;
	}
}

shell_escape(command) 
	char *command ;
{

	fprintf(stdout,"Running command: %s\n", command+1) ;
	system(command+1) ;

	fprintf(stderr,"\n[Continue]:") ;
}

yywrap()
{
	if (yyin != stdin)
	{
		yyin = stdin ;
		return(0) ;
	}
	else
	{
		return(1) ;
	}
}

clear_input()
{
    if (yyin != stdin)
	yyin = stdin ;
     else
	 while ('\n' != getc(yyin))
	    ;
}

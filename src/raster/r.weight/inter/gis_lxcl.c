#include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
#ifndef __cplusplus
# define output(c) (void)putc(c,yyout)
#else
# define lex_output(c) (void)putc(c,yyout)
#endif

#if defined(__cplusplus) || defined(__STDC__)

#if defined(__cplusplus) && defined(__EXTERN_C__)
extern "C" {
#endif
	int yyback(int *, int);
	int yyinput(void);
	int yylook(void);
	void yyoutput(int);
	int yyracc(int);
	int yyreject(void);
	void yyunput(int);
	int yylex(void);
#ifdef YYLEX_E
	void yywoutput(wchar_t);
	wchar_t yywinput(void);
#endif
#ifndef yyless
	void yyless(int);
#endif
#ifndef yywrap
	int yywrap(void);
#endif
#ifdef LEXDEBUG
	void allprint(char);
	void sprint(char *);
#endif
#if defined(__cplusplus) && defined(__EXTERN_C__)
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
	void exit(int);
#ifdef __cplusplus
}
#endif

#endif
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
#ifndef __cplusplus
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#else
# define lex_input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
#endif
#define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
extern char *falloc();
#include "gis_pars.h"
#ifdef getc
#undef getc
#endif
#define getc mygetc
long atol();
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
#ifdef __cplusplus
/* to avoid CC and lint complaining yyfussy not being used ...*/
static int __lex_hack = 0;
if (__lex_hack) goto yyfussy;
#endif
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:

# line 15 "gis_lxcl.l"
   {
		yylval.long_val = atol(yytext) ;
		return (LONG_NUM) ;
	}
break;
case 2:

# line 20 "gis_lxcl.l"
    case 3:

# line 21 "gis_lxcl.l"
{
		return(HELP_TKN) ;
	}
break;
case 4:

# line 25 "gis_lxcl.l"
case 5:

# line 26 "gis_lxcl.l"
{
		return(COLR_TKN) ;
	}
break;
case 6:

# line 30 "gis_lxcl.l"
case 7:

# line 31 "gis_lxcl.l"
case 8:

# line 32 "gis_lxcl.l"
{
		return(LST_TKN) ;
	}
break;
case 9:

# line 36 "gis_lxcl.l"
    case 10:

# line 37 "gis_lxcl.l"
   case 11:

# line 38 "gis_lxcl.l"
   case 12:

# line 39 "gis_lxcl.l"
   case 13:

# line 40 "gis_lxcl.l"
    case 14:

# line 41 "gis_lxcl.l"
    {
		return(QUIT_TKN) ;
	}
break;
case 15:

# line 45 "gis_lxcl.l"
  {
		return(PRT_TKN) ;
	}
break;
case 16:

# line 49 "gis_lxcl.l"
  {
		return(ERAS_TKN) ;
	}
break;
case 17:

# line 53 "gis_lxcl.l"
	case 18:

# line 54 "gis_lxcl.l"
 {
		return(ASG_TKN) ;
	}
break;
case 19:

# line 58 "gis_lxcl.l"
  	case 20:

# line 59 "gis_lxcl.l"
case 21:

# line 60 "gis_lxcl.l"
 {
		return(ANAL_TKN) ;
	}
break;
case 22:

# line 64 "gis_lxcl.l"
	case 23:

# line 65 "gis_lxcl.l"
   {
		return(UNCH_TKN) ;
	}
break;
case 24:

# line 69 "gis_lxcl.l"
case 25:

# line 70 "gis_lxcl.l"
 {
		return(CHOS_TKN) ;
	}
break;
case 26:

# line 74 "gis_lxcl.l"
	case 27:

# line 75 "gis_lxcl.l"
{
		return(REC_TKN) ;
	}
break;
case 28:

# line 79 "gis_lxcl.l"
   {
		return(EXEC_TKN) ;
	}
break;
case 29:

# line 83 "gis_lxcl.l"
case 30:

# line 84 "gis_lxcl.l"
   {
		return(SAV_TKN) ;
	}
break;
case 31:

# line 88 "gis_lxcl.l"
  {
		return(MAP_TKN) ;
	}
break;
case 32:

# line 92 "gis_lxcl.l"
   {
		return(CATS_TKN) ;
	}
break;
case 33:

# line 96 "gis_lxcl.l"
   case 34:

# line 97 "gis_lxcl.l"
{
		return(MULT_TKN) ;
	}
break;
case 35:

# line 101 "gis_lxcl.l"
   {
		return(ADD_TKN) ;
	}
break;
case 36:

# line 105 "gis_lxcl.l"
    case 37:

# line 106 "gis_lxcl.l"
	case 38:

# line 107 "gis_lxcl.l"
	case 39:

# line 108 "gis_lxcl.l"
case 40:

# line 109 "gis_lxcl.l"
	{
	}
break;
case 41:

# line 112 "gis_lxcl.l"
    {
		yylval.str_val = (char *)falloc(yyleng + 1, sizeof(char),
			"falloc: lexical analyser call failed") ;
		strcpy (yylval.str_val, yytext) ;
		return (NAM_STR) ;
	}
break;
case 42:

# line 119 "gis_lxcl.l"
case 43:

# line 120 "gis_lxcl.l"
case 44:

# line 121 "gis_lxcl.l"
{
		yytext[yyleng-1] = 0;
		yylval.str_val = (char *)falloc(yyleng, sizeof(char),
			"falloc: lexical analyser call failed") ;
		strcpy (yylval.str_val, yytext+1) ;
		G_strip (yylval.str_val);
		return (NAM_STR) ;
	}
break;
case 45:

# line 130 "gis_lxcl.l"
{
		/*
		return (PAREN_TKN) ;
		*/
	}
break;
case 46:

# line 137 "gis_lxcl.l"
    {
	}
break;
case 47:

# line 140 "gis_lxcl.l"
   {
	}
break;
case 48:

# line 143 "gis_lxcl.l"
   {
		return (LINE_TKN) ;
	}
break;
case -1:
break;
default:
(void)fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
yywrap()
{
	return(1) ;
}
int yyvstop[] = {
0,

47,
0,

48,
0,

46,
0,

45,
0,

45,
0,

45,
0,

45,
0,

1,
36,
0,

1,
0,

2,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

9,
41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

40,
41,
0,

41,
0,

41,
0,

42,
0,

43,
0,

44,
0,

14,
41,
0,

35,
41,
0,

41,
0,

41,
0,

13,
41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

37,
41,
0,

26,
41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

17,
41,
0,

41,
0,

41,
0,

41,
0,

38,
41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

11,
41,
0,

3,
41,
0,

8,
41,
0,

31,
41,
0,

33,
41,
0,

24,
41,
0,

41,
0,

41,
0,

12,
41,
0,

41,
0,

41,
0,

30,
41,
0,

7,
41,
0,

10,
41,
0,

41,
0,

6,
41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

5,
41,
0,

16,
41,
0,

41,
0,

41,
0,

41,
0,

15,
41,
0,

41,
0,

41,
0,

29,
41,
0,

19,
41,
0,

41,
0,

41,
0,

18,
41,
0,

41,
0,

25,
41,
0,

4,
41,
0,

41,
0,

41,
0,

39,
41,
0,

41,
0,

22,
41,
0,

41,
0,

41,
0,

41,
0,

41,
0,

28,
41,
0,

41,
0,

27,
41,
0,

41,
0,

20,
41,
0,

21,
41,
0,

41,
0,

34,
41,
0,

23,
41,
0,

41,
0,

32,
41,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,3,	1,4,	
0,0,	66,66,	68,68,	0,0,	
70,70,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,5,	0,0,	1,6,	
66,66,	68,68,	66,67,	70,70,	
1,7,	1,8,	1,9,	0,0,	
68,69,	0,0,	1,10,	0,0,	
70,71,	1,11,	1,11,	1,11,	
1,11,	1,11,	1,11,	1,11,	
1,11,	1,11,	1,11,	1,9,	
0,0,	0,0,	0,0,	0,0,	
1,12,	0,0,	1,13,	1,14,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
1,13,	1,13,	1,13,	1,13,	
14,36,	36,72,	0,0,	0,0,	
1,13,	0,0,	1,15,	1,16,	
1,17,	1,13,	1,18,	1,19,	
1,20,	1,21,	1,13,	1,13,	
1,13,	1,22,	1,23,	1,13,	
1,13,	1,24,	1,25,	1,26,	
1,27,	1,28,	1,29,	1,13,	
1,13,	1,13,	1,13,	1,13,	
6,30,	10,11,	16,40,	19,46,	
10,11,	10,11,	10,11,	10,11,	
10,11,	10,11,	10,11,	10,11,	
10,11,	10,11,	15,37,	18,44,	
20,47,	21,48,	17,41,	22,49,	
23,50,	18,45,	25,55,	6,30,	
15,38,	17,42,	26,56,	24,52,	
29,65,	15,39,	24,53,	27,57,	
17,43,	37,73,	38,74,	27,58,	
24,54,	39,75,	27,59,	40,76,	
23,51,	41,77,	42,78,	43,79,	
44,80,	45,81,	46,83,	47,84,	
48,85,	45,82,	27,60,	49,86,	
50,87,	51,88,	52,89,	53,90,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	54,91,	55,92,	
57,95,	58,96,	6,31,	59,97,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	7,32,	28,61,	
56,93,	31,66,	60,98,	61,99,	
62,100,	64,101,	28,62,	65,102,	
74,103,	75,104,	56,94,	77,105,	
78,106,	28,63,	79,107,	80,108,	
81,109,	82,110,	85,111,	86,112,	
87,113,	7,32,	88,114,	28,64,	
31,66,	89,115,	31,67,	90,116,	
91,117,	92,118,	93,119,	94,120,	
95,121,	97,122,	98,123,	99,124,	
101,125,	102,126,	31,31,	103,127,	
31,31,	31,31,	31,31,	31,31,	
31,31,	31,31,	31,31,	31,31,	
31,31,	31,31,	104,128,	105,129,	
106,130,	107,131,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
108,132,	109,133,	114,134,	116,135,	
7,33,	117,136,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
8,34,	33,68,	119,137,	120,138,	
121,139,	124,140,	126,141,	127,142,	
128,143,	129,144,	130,145,	131,146,	
133,147,	134,148,	135,149,	137,150,	
138,151,	141,152,	142,153,	144,155,	
147,156,	148,157,	142,154,	8,34,	
33,68,	150,158,	152,159,	153,160,	
154,161,	155,162,	157,163,	33,69,	
159,164,	162,165,	165,166,	0,0,	
0,0,	0,0,	33,33,	0,0,	
33,33,	33,33,	33,33,	33,33,	
33,33,	33,33,	33,33,	33,33,	
33,33,	33,33,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	0,0,	0,0,	
0,0,	0,0,	8,35,	0,0,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	8,35,	8,35,	
8,35,	8,35,	13,13,	0,0,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	0,0,	
0,0,	0,0,	0,0,	13,13,	
0,0,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	13,13,	
13,13,	13,13,	13,13,	35,70,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	35,70,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	35,71,	
0,0,	0,0,	0,0,	0,0,	
35,35,	0,0,	35,35,	35,35,	
35,35,	35,35,	35,35,	35,35,	
35,35,	35,35,	35,35,	35,35,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+1,	0,		0,	
yycrank+0,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+5,
yycrank+115,	0,		yyvstop+7,
yycrank+229,	0,		yyvstop+9,
yycrank+343,	0,		yyvstop+11,
yycrank+0,	0,		yyvstop+13,
yycrank+80,	0,		yyvstop+15,
yycrank+0,	yysvec+10,	yyvstop+18,
yycrank+0,	0,		yyvstop+20,
yycrank+420,	0,		yyvstop+22,
yycrank+3,	yysvec+13,	yyvstop+24,
yycrank+38,	yysvec+13,	yyvstop+26,
yycrank+5,	yysvec+13,	yyvstop+28,
yycrank+45,	yysvec+13,	yyvstop+30,
yycrank+25,	yysvec+13,	yyvstop+32,
yycrank+16,	yysvec+13,	yyvstop+34,
yycrank+39,	yysvec+13,	yyvstop+36,
yycrank+40,	yysvec+13,	yyvstop+38,
yycrank+38,	yysvec+13,	yyvstop+40,
yycrank+47,	yysvec+13,	yyvstop+42,
yycrank+46,	yysvec+13,	yyvstop+44,
yycrank+29,	yysvec+13,	yyvstop+46,
yycrank+49,	yysvec+13,	yyvstop+49,
yycrank+58,	yysvec+13,	yyvstop+51,
yycrank+142,	yysvec+13,	yyvstop+53,
yycrank+42,	yysvec+13,	yyvstop+55,
yycrank+0,	yysvec+6,	0,	
yycrank+232,	yysvec+6,	0,	
yycrank+0,	yysvec+7,	0,	
yycrank+344,	yysvec+7,	0,	
yycrank+0,	yysvec+8,	0,	
yycrank+534,	yysvec+8,	0,	
yycrank+24,	yysvec+13,	yyvstop+57,
yycrank+57,	yysvec+13,	yyvstop+59,
yycrank+61,	yysvec+13,	yyvstop+61,
yycrank+46,	yysvec+13,	yyvstop+63,
yycrank+62,	yysvec+13,	yyvstop+65,
yycrank+49,	yysvec+13,	yyvstop+67,
yycrank+55,	yysvec+13,	yyvstop+69,
yycrank+59,	yysvec+13,	yyvstop+71,
yycrank+71,	yysvec+13,	yyvstop+73,
yycrank+68,	yysvec+13,	yyvstop+75,
yycrank+56,	yysvec+13,	yyvstop+77,
yycrank+55,	yysvec+13,	yyvstop+79,
yycrank+64,	yysvec+13,	yyvstop+81,
yycrank+60,	yysvec+13,	yyvstop+83,
yycrank+64,	yysvec+13,	yyvstop+85,
yycrank+69,	yysvec+13,	yyvstop+87,
yycrank+79,	yysvec+13,	yyvstop+89,
yycrank+78,	yysvec+13,	yyvstop+91,
yycrank+101,	yysvec+13,	yyvstop+93,
yycrank+102,	yysvec+13,	yyvstop+95,
yycrank+141,	yysvec+13,	yyvstop+97,
yycrank+90,	yysvec+13,	yyvstop+99,
yycrank+93,	yysvec+13,	yyvstop+101,
yycrank+100,	yysvec+13,	yyvstop+103,
yycrank+131,	yysvec+13,	yyvstop+105,
yycrank+145,	yysvec+13,	yyvstop+107,
yycrank+143,	yysvec+13,	yyvstop+109,
yycrank+0,	yysvec+13,	yyvstop+111,
yycrank+133,	yysvec+13,	yyvstop+114,
yycrank+148,	yysvec+13,	yyvstop+116,
yycrank+4,	0,		0,	
yycrank+0,	0,		yyvstop+118,
yycrank+5,	0,		0,	
yycrank+0,	0,		yyvstop+120,
yycrank+7,	0,		0,	
yycrank+0,	0,		yyvstop+122,
yycrank+0,	yysvec+13,	yyvstop+124,
yycrank+0,	yysvec+13,	yyvstop+127,
yycrank+140,	yysvec+13,	yyvstop+130,
yycrank+144,	yysvec+13,	yyvstop+132,
yycrank+0,	yysvec+13,	yyvstop+134,
yycrank+150,	yysvec+13,	yyvstop+137,
yycrank+141,	yysvec+13,	yyvstop+139,
yycrank+143,	yysvec+13,	yyvstop+141,
yycrank+140,	yysvec+13,	yyvstop+143,
yycrank+157,	yysvec+13,	yyvstop+145,
yycrank+141,	yysvec+13,	yyvstop+147,
yycrank+0,	yysvec+13,	yyvstop+149,
yycrank+0,	yysvec+13,	yyvstop+152,
yycrank+146,	yysvec+13,	yyvstop+155,
yycrank+143,	yysvec+13,	yyvstop+157,
yycrank+145,	yysvec+13,	yyvstop+159,
yycrank+146,	yysvec+13,	yyvstop+161,
yycrank+158,	yysvec+13,	yyvstop+163,
yycrank+170,	yysvec+13,	yyvstop+165,
yycrank+158,	yysvec+13,	yyvstop+167,
yycrank+153,	yysvec+13,	yyvstop+169,
yycrank+159,	yysvec+13,	yyvstop+171,
yycrank+160,	yysvec+13,	yyvstop+173,
yycrank+171,	yysvec+13,	yyvstop+175,
yycrank+0,	yysvec+13,	yyvstop+177,
yycrank+154,	yysvec+13,	yyvstop+180,
yycrank+162,	yysvec+13,	yyvstop+182,
yycrank+167,	yysvec+13,	yyvstop+184,
yycrank+0,	yysvec+13,	yyvstop+186,
yycrank+175,	yysvec+13,	yyvstop+189,
yycrank+173,	yysvec+13,	yyvstop+191,
yycrank+158,	yysvec+13,	yyvstop+193,
yycrank+187,	yysvec+13,	yyvstop+195,
yycrank+188,	yysvec+13,	yyvstop+197,
yycrank+177,	yysvec+13,	yyvstop+199,
yycrank+179,	yysvec+13,	yyvstop+201,
yycrank+219,	yysvec+13,	yyvstop+203,
yycrank+204,	yysvec+13,	yyvstop+205,
yycrank+0,	yysvec+13,	yyvstop+207,
yycrank+0,	yysvec+13,	yyvstop+210,
yycrank+0,	yysvec+13,	yyvstop+213,
yycrank+0,	yysvec+13,	yyvstop+216,
yycrank+217,	yysvec+13,	yyvstop+219,
yycrank+0,	yysvec+13,	yyvstop+222,
yycrank+208,	yysvec+13,	yyvstop+225,
yycrank+209,	yysvec+13,	yyvstop+227,
yycrank+0,	yysvec+13,	yyvstop+229,
yycrank+236,	yysvec+13,	yyvstop+232,
yycrank+237,	yysvec+13,	yyvstop+234,
yycrank+256,	yysvec+13,	yyvstop+236,
yycrank+0,	yysvec+13,	yyvstop+239,
yycrank+0,	yysvec+13,	yyvstop+242,
yycrank+256,	yysvec+13,	yyvstop+245,
yycrank+0,	yysvec+13,	yyvstop+247,
yycrank+247,	yysvec+13,	yyvstop+250,
yycrank+244,	yysvec+13,	yyvstop+252,
yycrank+250,	yysvec+13,	yyvstop+254,
yycrank+250,	yysvec+13,	yyvstop+256,
yycrank+261,	yysvec+13,	yyvstop+258,
yycrank+248,	yysvec+13,	yyvstop+260,
yycrank+0,	yysvec+13,	yyvstop+263,
yycrank+248,	yysvec+13,	yyvstop+266,
yycrank+253,	yysvec+13,	yyvstop+268,
yycrank+265,	yysvec+13,	yyvstop+270,
yycrank+0,	yysvec+13,	yyvstop+272,
yycrank+266,	yysvec+13,	yyvstop+275,
yycrank+267,	yysvec+13,	yyvstop+277,
yycrank+0,	yysvec+13,	yyvstop+279,
yycrank+0,	yysvec+13,	yyvstop+282,
yycrank+258,	yysvec+13,	yyvstop+285,
yycrank+269,	yysvec+13,	yyvstop+287,
yycrank+0,	yysvec+13,	yyvstop+289,
yycrank+257,	yysvec+13,	yyvstop+292,
yycrank+0,	yysvec+13,	yyvstop+294,
yycrank+0,	yysvec+13,	yyvstop+297,
yycrank+271,	yysvec+13,	yyvstop+300,
yycrank+265,	yysvec+13,	yyvstop+302,
yycrank+0,	yysvec+13,	yyvstop+304,
yycrank+263,	yysvec+13,	yyvstop+307,
yycrank+0,	yysvec+13,	yyvstop+309,
yycrank+263,	yysvec+13,	yyvstop+312,
yycrank+264,	yysvec+13,	yyvstop+314,
yycrank+265,	yysvec+13,	yyvstop+316,
yycrank+276,	yysvec+13,	yyvstop+318,
yycrank+0,	yysvec+13,	yyvstop+320,
yycrank+261,	yysvec+13,	yyvstop+323,
yycrank+0,	yysvec+13,	yyvstop+325,
yycrank+283,	yysvec+13,	yyvstop+328,
yycrank+0,	yysvec+13,	yyvstop+330,
yycrank+0,	yysvec+13,	yyvstop+333,
yycrank+284,	yysvec+13,	yyvstop+336,
yycrank+0,	yysvec+13,	yyvstop+338,
yycrank+0,	yysvec+13,	yyvstop+341,
yycrank+271,	yysvec+13,	yyvstop+344,
yycrank+0,	yysvec+13,	yyvstop+346,
0,	0,	0};
struct yywork *yytop = yycrank+591;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
  0,   1,   1,   1,   1,   1,   1,   1, 
  1,   9,  10,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
 32,   1,  34,   1,   1,   1,   1,  34, 
 34,  34,   1,   1,   1,  45,  46,   1, 
 48,  48,  48,  48,  48,  48,  48,  48, 
 48,  48,  34,   1,   1,   1,   1,  63, 
  1,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,   1,   1,   1,   1,  65, 
  1,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,  65,  65,  65,  65,  65, 
 65,  65,  65,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
  1,   1,   1,   1,   1,   1,   1,   1, 
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	Copyright (c) 1989 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#pragma ident	"@(#)ncform	6.7	93/06/07 SMI"

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
#if defined(__cplusplus) || defined(__STDC__)
int yylook(void)
#else
yylook()
#endif
{
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
#ifndef __cplusplus
			*yylastch++ = yych = input();
#else
			*yylastch++ = yych = lex_input();
#endif
			if(yylastch > &yytext[YYLMAX]) {
				fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
				exit(1);
			}
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					if(lsp > &yylstate[YYLMAX]) {
						fprintf(yyout,"Input string too long, limit %d\n",YYLMAX);
						exit(1);
					}
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
#ifndef __cplusplus
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
#else
		yyprevious = yytext[0] = lex_input();
		if (yyprevious>0)
			lex_output(yyprevious);
#endif
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
#if defined(__cplusplus) || defined(__STDC__)
int yyback(int *p, int m)
#else
yyback(p, m)
	int *p;
#endif
{
	if (p==0) return(0);
	while (*p) {
		if (*p++ == m)
			return(1);
	}
	return(0);
}
	/* the following are only used in the lex library */
#if defined(__cplusplus) || defined(__STDC__)
int yyinput(void)
#else
yyinput()
#endif
{
#ifndef __cplusplus
	return(input());
#else
	return(lex_input());
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyoutput(int c)
#else
yyoutput(c)
  int c; 
#endif
{
#ifndef __cplusplus
	output(c);
#else
	lex_output(c);
#endif
	}
#if defined(__cplusplus) || defined(__STDC__)
void yyunput(int c)
#else
yyunput(c)
   int c; 
#endif
{
	unput(c);
	}

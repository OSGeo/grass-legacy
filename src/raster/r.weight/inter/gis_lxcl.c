# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
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
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
   {
		yylval.long_val = atol(yytext) ;
		return (LONG_NUM) ;
	}
break;
case 2:
    case 3:
{
		return(HELP_TKN) ;
	}
break;
case 4:
case 5:
{
		return(COLR_TKN) ;
	}
break;
case 6:
case 7:
case 8:
{
		return(LST_TKN) ;
	}
break;
case 9:
    case 10:
   case 11:
   case 12:
   case 13:
    case 14:
    {
		return(QUIT_TKN) ;
	}
break;
case 15:
  {
		return(PRT_TKN) ;
	}
break;
case 16:
  {
		return(ERAS_TKN) ;
	}
break;
case 17:
	case 18:
 {
		return(ASG_TKN) ;
	}
break;
case 19:
  	case 20:
case 21:
 {
		return(ANAL_TKN) ;
	}
break;
case 22:
	case 23:
   {
		return(UNCH_TKN) ;
	}
break;
case 24:
case 25:
 {
		return(CHOS_TKN) ;
	}
break;
case 26:
	case 27:
{
		return(REC_TKN) ;
	}
break;
case 28:
   {
		return(EXEC_TKN) ;
	}
break;
case 29:
case 30:
   {
		return(SAV_TKN) ;
	}
break;
case 31:
  {
		return(MAP_TKN) ;
	}
break;
case 32:
   {
		return(CATS_TKN) ;
	}
break;
case 33:
   case 34:
{
		return(MULT_TKN) ;
	}
break;
case 35:
   {
		return(ADD_TKN) ;
	}
break;
case 36:
    case 37:
	case 38:
	case 39:
case 40:
	{
	}
break;
case 41:
    {
		yylval.str_val = (char *)falloc(yyleng + 1, sizeof(char),
			"falloc: lexical analyser call failed") ;
		strcpy (yylval.str_val, yytext) ;
		return (NAM_STR) ;
	}
break;
case 42:
case 43:
case 44:
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
{
		/*
		return (PAREN_TKN) ;
		*/
	}
break;
case 46:
    {
	}
break;
case 47:
   {
	}
break;
case 48:
   {
		return (LINE_TKN) ;
	}
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
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
# define YYTYPE int
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,3,	1,4,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,5,	0,0,	1,6,	
0,0,	0,0,	0,0,	0,0,	
1,7,	1,8,	1,9,	0,0,	
0,0,	0,0,	1,10,	0,0,	
0,0,	1,11,	1,11,	1,11,	
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
0,0,	0,0,	0,0,	0,0,	
1,13,	0,0,	1,15,	1,16,	
1,17,	1,13,	1,18,	1,19,	
1,20,	1,21,	1,13,	1,13,	
1,13,	1,22,	1,23,	1,13,	
1,13,	1,24,	1,25,	1,26,	
1,27,	1,28,	1,29,	1,13,	
1,13,	1,13,	1,13,	1,13,	
6,30,	10,11,	14,36,	19,46,	
10,11,	10,11,	10,11,	10,11,	
10,11,	10,11,	10,11,	10,11,	
10,11,	10,11,	15,37,	16,40,	
18,44,	20,47,	17,41,	21,48,	
22,49,	23,50,	18,45,	6,30,	
15,38,	17,42,	25,55,	24,52,	
26,56,	15,39,	24,53,	27,57,	
17,43,	29,65,	36,72,	27,58,	
24,54,	37,73,	27,59,	38,74,	
39,75,	23,51,	40,76,	41,77,	
42,78,	43,79,	44,80,	45,81,	
46,83,	47,84,	27,60,	45,82,	
48,85,	49,86,	50,87,	51,88,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	52,89,	53,90,	
54,91,	55,92,	6,31,	57,95,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	6,31,	6,31,	
6,31,	6,31,	7,32,	28,61,	
56,93,	58,96,	59,97,	60,98,	
61,99,	62,100,	28,62,	64,101,	
65,102,	66,66,	56,94,	74,103,	
68,68,	28,63,	70,70,	75,104,	
77,105,	78,106,	79,107,	80,108,	
81,109,	7,32,	82,110,	28,64,	
85,111,	86,112,	87,113,	88,114,	
89,115,	90,116,	91,117,	92,118,	
66,66,	93,119,	66,67,	68,68,	
94,120,	70,70,	95,121,	97,122,	
98,123,	99,124,	68,69,	101,125,	
102,126,	103,127,	70,71,	104,128,	
105,129,	106,130,	107,131,	108,132,	
109,133,	114,134,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
116,135,	117,136,	119,137,	120,138,	
7,33,	121,139,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
7,33,	7,33,	7,33,	7,33,	
8,34,	124,140,	126,141,	127,142,	
128,143,	129,144,	130,145,	131,146,	
133,147,	134,148,	135,149,	137,150,	
138,151,	141,152,	142,153,	144,155,	
147,156,	148,157,	142,154,	150,158,	
152,159,	153,160,	154,161,	8,34,	
155,162,	157,163,	159,164,	162,165,	
165,166,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
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
13,13,	13,13,	13,13,	31,0,	
31,0,	31,0,	31,0,	31,0,	
31,0,	31,0,	31,0,	31,66,	
31,0,	31,0,	31,0,	31,0,	
31,0,	31,0,	31,0,	31,0,	
31,0,	31,0,	31,0,	31,0,	
31,0,	31,0,	31,0,	31,0,	
31,0,	31,0,	31,0,	31,0,	
31,0,	31,0,	31,66,	31,0,	
31,67,	31,0,	31,0,	31,0,	
31,0,	31,0,	31,0,	31,0,	
31,0,	31,0,	31,0,	31,0,	
31,31,	31,0,	31,31,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
31,0,	31,0,	31,0,	31,0,	
31,0,	31,0,	31,0,	31,31,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	31,0,	31,0,	31,0,	
31,0,	0,0,	31,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	31,0,	31,0,	31,0,	
31,0,	31,0,	33,0,	33,0,	
33,0,	33,0,	33,0,	33,0,	
33,0,	33,0,	33,68,	33,0,	
33,0,	33,0,	33,0,	33,0,	
33,0,	33,0,	33,0,	33,0,	
33,0,	33,0,	33,0,	33,0,	
33,0,	33,0,	33,0,	33,0,	
33,0,	33,0,	33,0,	33,0,	
33,0,	33,68,	33,0,	33,0,	
33,0,	33,0,	33,0,	33,0,	
33,69,	33,0,	33,0,	33,0,	
33,0,	33,0,	33,0,	33,33,	
33,0,	33,33,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	33,0,	
33,0,	33,0,	33,0,	33,0,	
33,0,	33,0,	33,33,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
33,0,	33,0,	33,0,	33,0,	
0,0,	33,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
33,0,	33,0,	33,0,	33,0,	
33,0,	35,0,	35,0,	35,0,	
35,0,	35,0,	35,0,	35,0,	
35,0,	35,70,	35,0,	35,0,	
35,0,	35,0,	35,0,	35,0,	
35,0,	35,0,	35,0,	35,0,	
35,0,	35,0,	35,0,	35,0,	
35,0,	35,0,	35,0,	35,0,	
35,0,	35,0,	35,0,	35,0,	
35,70,	35,0,	35,0,	35,0,	
35,0,	35,0,	35,0,	35,0,	
35,0,	35,71,	35,0,	35,0,	
35,0,	35,0,	35,35,	35,0,	
35,35,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	35,0,	35,0,	
35,0,	35,0,	35,0,	35,0,	
35,0,	35,35,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	35,0,	
35,0,	35,0,	35,0,	0,0,	
35,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	35,0,	
35,0,	35,0,	35,0,	35,0,	
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
yycrank+37,	yysvec+13,	yyvstop+24,
yycrank+38,	yysvec+13,	yyvstop+26,
yycrank+18,	yysvec+13,	yyvstop+28,
yycrank+45,	yysvec+13,	yyvstop+30,
yycrank+26,	yysvec+13,	yyvstop+32,
yycrank+16,	yysvec+13,	yyvstop+34,
yycrank+40,	yysvec+13,	yyvstop+36,
yycrank+42,	yysvec+13,	yyvstop+38,
yycrank+39,	yysvec+13,	yyvstop+40,
yycrank+48,	yysvec+13,	yyvstop+42,
yycrank+46,	yysvec+13,	yyvstop+44,
yycrank+33,	yysvec+13,	yyvstop+46,
yycrank+51,	yysvec+13,	yyvstop+49,
yycrank+58,	yysvec+13,	yyvstop+51,
yycrank+142,	yysvec+13,	yyvstop+53,
yycrank+47,	yysvec+13,	yyvstop+55,
yycrank+0,	yysvec+6,	0,	
yycrank+-542,	0,		0,	
yycrank+0,	yysvec+7,	0,	
yycrank+-669,	0,		0,	
yycrank+0,	yysvec+8,	0,	
yycrank+-796,	0,		0,	
yycrank+89,	yysvec+13,	yyvstop+57,
yycrank+61,	yysvec+13,	yyvstop+59,
yycrank+66,	yysvec+13,	yyvstop+61,
yycrank+49,	yysvec+13,	yyvstop+63,
yycrank+65,	yysvec+13,	yyvstop+65,
yycrank+51,	yysvec+13,	yyvstop+67,
yycrank+57,	yysvec+13,	yyvstop+69,
yycrank+61,	yysvec+13,	yyvstop+71,
yycrank+73,	yysvec+13,	yyvstop+73,
yycrank+70,	yysvec+13,	yyvstop+75,
yycrank+58,	yysvec+13,	yyvstop+77,
yycrank+57,	yysvec+13,	yyvstop+79,
yycrank+68,	yysvec+13,	yyvstop+81,
yycrank+62,	yysvec+13,	yyvstop+83,
yycrank+66,	yysvec+13,	yyvstop+85,
yycrank+71,	yysvec+13,	yyvstop+87,
yycrank+107,	yysvec+13,	yyvstop+89,
yycrank+106,	yysvec+13,	yyvstop+91,
yycrank+103,	yysvec+13,	yyvstop+93,
yycrank+104,	yysvec+13,	yyvstop+95,
yycrank+141,	yysvec+13,	yyvstop+97,
yycrank+93,	yysvec+13,	yyvstop+99,
yycrank+125,	yysvec+13,	yyvstop+101,
yycrank+131,	yysvec+13,	yyvstop+103,
yycrank+132,	yysvec+13,	yyvstop+105,
yycrank+146,	yysvec+13,	yyvstop+107,
yycrank+144,	yysvec+13,	yyvstop+109,
yycrank+0,	yysvec+13,	yyvstop+111,
yycrank+135,	yysvec+13,	yyvstop+114,
yycrank+149,	yysvec+13,	yyvstop+116,
yycrank+240,	0,		0,	
yycrank+0,	0,		yyvstop+118,
yycrank+243,	0,		0,	
yycrank+0,	0,		yyvstop+120,
yycrank+245,	0,		0,	
yycrank+0,	0,		yyvstop+122,
yycrank+0,	yysvec+13,	yyvstop+124,
yycrank+0,	yysvec+13,	yyvstop+127,
yycrank+143,	yysvec+13,	yyvstop+130,
yycrank+150,	yysvec+13,	yyvstop+132,
yycrank+0,	yysvec+13,	yyvstop+134,
yycrank+155,	yysvec+13,	yyvstop+137,
yycrank+146,	yysvec+13,	yyvstop+139,
yycrank+147,	yysvec+13,	yyvstop+141,
yycrank+144,	yysvec+13,	yyvstop+143,
yycrank+161,	yysvec+13,	yyvstop+145,
yycrank+146,	yysvec+13,	yyvstop+147,
yycrank+0,	yysvec+13,	yyvstop+149,
yycrank+0,	yysvec+13,	yyvstop+152,
yycrank+152,	yysvec+13,	yyvstop+155,
yycrank+149,	yysvec+13,	yyvstop+157,
yycrank+151,	yysvec+13,	yyvstop+159,
yycrank+151,	yysvec+13,	yyvstop+161,
yycrank+161,	yysvec+13,	yyvstop+163,
yycrank+172,	yysvec+13,	yyvstop+165,
yycrank+160,	yysvec+13,	yyvstop+167,
yycrank+155,	yysvec+13,	yyvstop+169,
yycrank+162,	yysvec+13,	yyvstop+171,
yycrank+165,	yysvec+13,	yyvstop+173,
yycrank+177,	yysvec+13,	yyvstop+175,
yycrank+0,	yysvec+13,	yyvstop+177,
yycrank+160,	yysvec+13,	yyvstop+180,
yycrank+168,	yysvec+13,	yyvstop+182,
yycrank+173,	yysvec+13,	yyvstop+184,
yycrank+0,	yysvec+13,	yyvstop+186,
yycrank+182,	yysvec+13,	yyvstop+189,
yycrank+180,	yysvec+13,	yyvstop+191,
yycrank+164,	yysvec+13,	yyvstop+193,
yycrank+184,	yysvec+13,	yyvstop+195,
yycrank+185,	yysvec+13,	yyvstop+197,
yycrank+174,	yysvec+13,	yyvstop+199,
yycrank+176,	yysvec+13,	yyvstop+201,
yycrank+190,	yysvec+13,	yyvstop+203,
yycrank+175,	yysvec+13,	yyvstop+205,
yycrank+0,	yysvec+13,	yyvstop+207,
yycrank+0,	yysvec+13,	yyvstop+210,
yycrank+0,	yysvec+13,	yyvstop+213,
yycrank+0,	yysvec+13,	yyvstop+216,
yycrank+188,	yysvec+13,	yyvstop+219,
yycrank+0,	yysvec+13,	yyvstop+222,
yycrank+205,	yysvec+13,	yyvstop+225,
yycrank+205,	yysvec+13,	yyvstop+227,
yycrank+0,	yysvec+13,	yyvstop+229,
yycrank+204,	yysvec+13,	yyvstop+232,
yycrank+205,	yysvec+13,	yyvstop+234,
yycrank+225,	yysvec+13,	yyvstop+236,
yycrank+0,	yysvec+13,	yyvstop+239,
yycrank+0,	yysvec+13,	yyvstop+242,
yycrank+252,	yysvec+13,	yyvstop+245,
yycrank+0,	yysvec+13,	yyvstop+247,
yycrank+243,	yysvec+13,	yyvstop+250,
yycrank+240,	yysvec+13,	yyvstop+252,
yycrank+246,	yysvec+13,	yyvstop+254,
yycrank+246,	yysvec+13,	yyvstop+256,
yycrank+257,	yysvec+13,	yyvstop+258,
yycrank+244,	yysvec+13,	yyvstop+260,
yycrank+0,	yysvec+13,	yyvstop+263,
yycrank+244,	yysvec+13,	yyvstop+266,
yycrank+249,	yysvec+13,	yyvstop+268,
yycrank+261,	yysvec+13,	yyvstop+270,
yycrank+0,	yysvec+13,	yyvstop+272,
yycrank+262,	yysvec+13,	yyvstop+275,
yycrank+263,	yysvec+13,	yyvstop+277,
yycrank+0,	yysvec+13,	yyvstop+279,
yycrank+0,	yysvec+13,	yyvstop+282,
yycrank+254,	yysvec+13,	yyvstop+285,
yycrank+265,	yysvec+13,	yyvstop+287,
yycrank+0,	yysvec+13,	yyvstop+289,
yycrank+253,	yysvec+13,	yyvstop+292,
yycrank+0,	yysvec+13,	yyvstop+294,
yycrank+0,	yysvec+13,	yyvstop+297,
yycrank+267,	yysvec+13,	yyvstop+300,
yycrank+261,	yysvec+13,	yyvstop+302,
yycrank+0,	yysvec+13,	yyvstop+304,
yycrank+257,	yysvec+13,	yyvstop+307,
yycrank+0,	yysvec+13,	yyvstop+309,
yycrank+257,	yysvec+13,	yyvstop+312,
yycrank+258,	yysvec+13,	yyvstop+314,
yycrank+259,	yysvec+13,	yyvstop+316,
yycrank+271,	yysvec+13,	yyvstop+318,
yycrank+0,	yysvec+13,	yyvstop+320,
yycrank+256,	yysvec+13,	yyvstop+323,
yycrank+0,	yysvec+13,	yyvstop+325,
yycrank+277,	yysvec+13,	yyvstop+328,
yycrank+0,	yysvec+13,	yyvstop+330,
yycrank+0,	yysvec+13,	yyvstop+333,
yycrank+278,	yysvec+13,	yyvstop+336,
yycrank+0,	yysvec+13,	yyvstop+338,
yycrank+0,	yysvec+13,	yyvstop+341,
yycrank+265,	yysvec+13,	yyvstop+344,
yycrank+0,	yysvec+13,	yyvstop+346,
0,	0,	0};
struct yywork *yytop = yycrank+923;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
040 ,01  ,'"' ,01  ,01  ,01  ,01  ,'"' ,
'"' ,'"' ,01  ,01  ,01  ,'-' ,'.' ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,'"' ,01  ,01  ,01  ,01  ,'?' ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,01  ,'A' ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,01  ,01  ,
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
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

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
yylook(){
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
			*yylastch++ = yych = input();
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
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}

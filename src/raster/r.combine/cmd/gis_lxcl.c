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
#include "gis_pars.h"
#ifdef getc
#undef getc
#endif
#define getc mygetc

extern FILE *e_sav_fil ;
FILE *newinput ;
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
   {
		yylval.int_val = atoi(yytext) ;
		return (INUMBER) ;
	}
break;
case 2:
 	case 3:
 	{
		yylval.int_val = 1 ;
		return (INUMBER) ;
	}
break;
case 4:
case 5:
  	case 6:
{
		yylval.int_val = 2 ;
		return (INUMBER) ;
	}
break;
case 7:
 	case 8:
 	case 9:
 	{
		yylval.int_val = 3 ;
		return (INUMBER) ;
	}
break;
case 10:
 	case 11:
 	case 12:
 	case 13:
 	{
		yylval.int_val = 4 ;
		return (INUMBER) ;
	}
break;
case 14:
case 15:
case 16:
{
		return(HST_TKN) ;
	}
break;
case 17:
case 18:
{
		return(ERA_TKN) ;
	}
break;
case 19:
case 20:
case 21:
	{
		return(HLP_TKN) ;
	}
break;
case 22:
 case 23:
 {
		return(WIN_TKN) ;
	}
break;
case 24:
    case 25:
   case 26:
   case 27:
   case 28:
   case 29:
   case 30:
   case 31:
    case 32:
    {
		return(BYE_TKN) ;
	}
break;
case 33:
     case 34:
     case 35:
   case 36:
   {
		return(CATS_TKN) ;
	}
break;
case 37:
    case 38:
   case 39:
    case 40:
    {
		return(AND_TKN) ;
	}
break;
case 41:
    case 42:
   case 43:
     case 44:
     {
		return(OR_TKN) ;
	}
break;
case 45:
    case 46:
    case 47:
    {
		return(NOT_TKN) ;
	}
break;
case 48:
    case 49:
  case 50:
    case 51:
  {
		return(GRP_TKN) ;
	}
break;
case 52:
    case 53:
   case 54:
    case 55:
   {
		return(EXPR_TKN) ;
	}
break;
case 56:
    case 57:
    case 58:
  case 59:
    case 60:
  {

		return(RANGE_TKN) ;
	}
break;
case 61:
    case 62:
   case 63:
   {
		return(NAM_TKN) ;
	}
break;
case 64:
case 65:
   case 66:
   {
		return(OVR_TKN) ;
	}
break;
case 67:
  case 68:
  case 69:
  {
		return(COV_TKN) ;
	}
break;
case 70:
{
		yylval.str_val = (char *)falloc(yyleng + 1, sizeof(char),
			"falloc: lexical analyser call failed") ;
		strncpy (yylval.str_val, yytext, yyleng + 1) ;
		return (NAM_STR) ;
	}
break;
case 71:
    {
	}
break;
case 72:
   {
	}
break;
case 73:
   {
	}
break;
case 74:
    {
		return (LP) ;
	}
break;
case 75:
    {
		return (RP) ;
	}
break;
case 76:
    { char achar ;
		printf("# ") ;
		while ((achar=yyinput()) != 012)
			putchar(achar) ;
		printf("\n") ;
	}
break;
case 77:
{
		switch_input(yytext) ;
	}
break;
case 78:
{ 
		shell_escape(yytext) ;
	}
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
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
int yyvstop[] = {
0,

72,
0,

73,
0,

71,
0,

76,
0,

37,
0,

74,
0,

75,
0,

56,
0,

1,
0,

21,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

24,
70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

41,
0,

45,
0,

78,
0,

38,
0,

77,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

44,
70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

43,
70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

42,
0,

40,
70,
0,

70,
0,

32,
70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

54,
70,
0,

70,
0,

70,
0,

70,
0,

50,
70,
0,

70,
0,

70,
0,

70,
0,

47,
70,
0,

70,
0,

70,
0,

70,
0,

3,
70,
0,

59,
70,
0,

70,
0,

70,
0,

70,
0,

39,
70,
0,

8,
70,
0,

31,
70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

52,
70,
0,

70,
0,

70,
0,

70,
0,

48,
70,
0,

70,
0,

70,
0,

61,
70,
0,

46,
70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

2,
70,
0,

57,
70,
0,

70,
0,

70,
0,

5,
70,
0,

9,
70,
0,

70,
0,

36,
70,
0,

70,
0,

70,
0,

28,
70,
0,

55,
70,
0,

13,
70,
0,

12,
70,
0,

70,
0,

20,
70,
0,

70,
0,

63,
70,
0,

66,
70,
0,

30,
70,
0,

70,
0,

26,
70,
0,

70,
0,

70,
0,

7,
70,
0,

70,
0,

35,
70,
0,

70,
0,

70,
0,

27,
70,
0,

53,
70,
0,

11,
70,
0,

10,
70,
0,

70,
0,

19,
70,
0,

14,
70,
0,

62,
70,
0,

65,
70,
0,

70,
0,

29,
70,
0,

70,
0,

25,
70,
0,

70,
0,

70,
0,

70,
0,

67,
70,
0,

18,
70,
0,

51,
70,
0,

70,
0,

60,
70,
0,

70,
0,

70,
0,

70,
0,

68,
70,
0,

17,
70,
0,

49,
70,
0,

70,
0,

70,
0,

69,
70,
0,

58,
70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

23,
70,
0,

6,
70,
0,

70,
0,

70,
0,

70,
0,

22,
70,
0,

4,
70,
0,

70,
0,

16,
70,
0,

70,
0,

15,
70,
0,

64,
70,
0,

70,
0,

70,
0,

70,
0,

70,
0,

33,
70,
0,

34,
70,
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
0,0,	1,5,	1,6,	0,0,	
1,7,	0,0,	0,0,	1,8,	
8,46,	1,9,	1,10,	0,0,	
0,0,	0,0,	1,11,	0,0,	
0,0,	1,12,	1,12,	1,12,	
1,12,	1,12,	1,12,	1,12,	
1,12,	1,12,	1,12,	45,0,	
0,0,	1,13,	0,0,	0,0,	
1,14,	0,0,	1,15,	1,16,	
1,17,	1,18,	1,19,	1,18,	
1,20,	1,21,	1,18,	1,18,	
1,18,	1,18,	1,18,	1,22,	
1,23,	1,18,	1,24,	1,25,	
1,26,	1,18,	1,18,	1,18,	
1,27,	1,18,	1,28,	1,18,	
18,18,	20,18,	33,18,	48,48,	
1,18,	20,56,	1,29,	1,30,	
1,31,	1,18,	1,32,	1,18,	
1,33,	1,34,	1,18,	1,18,	
1,18,	1,18,	1,18,	1,35,	
1,36,	1,37,	1,38,	1,39,	
1,40,	1,18,	1,18,	1,18,	
1,41,	1,18,	1,42,	1,18,	
6,45,	1,43,	27,68,	1,44,	
38,18,	37,18,	33,77,	27,18,	
6,45,	6,0,	12,12,	12,12,	
12,12,	12,12,	12,12,	12,12,	
12,12,	12,12,	12,12,	12,12,	
16,50,	17,52,	16,18,	26,18,	
37,84,	22,59,	19,18,	29,18,	
43,92,	26,67,	19,54,	6,45,	
13,47,	16,51,	17,18,	17,53,	
19,55,	21,57,	22,18,	22,60,	
42,18,	21,58,	61,18,	38,85,	
6,45,	6,45,	21,18,	6,45,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	40,18,	29,70,	
57,105,	64,111,	57,18,	42,91,	
6,45,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	59,107,	
59,18,	67,18,	67,114,	13,48,	
40,89,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	13,48,	
13,48,	13,48,	13,48,	15,18,	
15,18,	54,98,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
58,18,	68,115,	54,18,	82,18,	
80,18,	58,106,	41,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,49,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	41,90,	69,116,	80,131,	
69,18,	15,18,	93,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	15,18,	15,18,	15,18,	
15,18,	23,18,	25,64,	24,18,	
28,69,	23,61,	25,65,	30,18,	
31,18,	23,62,	24,63,	32,18,	
34,18,	28,18,	49,93,	25,66,	
50,18,	39,18,	36,18,	35,18,	
51,95,	65,112,	52,18,	50,94,	
49,18,	53,18,	60,18,	31,73,	
52,96,	51,18,	56,101,	65,18,	
60,108,	53,97,	56,102,	34,78,	
39,86,	30,71,	35,80,	34,79,	
39,87,	31,74,	62,109,	56,18,	
56,103,	56,104,	84,18,	32,75,	
66,113,	39,88,	30,72,	62,18,	
35,81,	32,76,	36,82,	66,18,	
75,18,	95,18,	36,83,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	63,110,	79,18,	104,18,	
89,18,	84,134,	63,18,	75,122,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	73,18,	71,18,	
74,18,	89,139,	47,48,	79,130,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	47,48,	47,48,	
47,48,	47,48,	55,99,	70,18,	
72,18,	76,18,	78,18,	55,18,	
73,120,	55,100,	71,118,	77,18,	
74,121,	81,18,	83,18,	85,18,	
86,18,	90,18,	87,18,	94,142,	
91,18,	88,18,	97,145,	99,18,	
96,143,	70,117,	108,18,	72,119,	
94,18,	99,147,	77,125,	97,18,	
76,123,	96,18,	77,126,	106,18,	
78,129,	83,133,	96,144,	76,124,	
87,137,	106,153,	85,135,	101,18,	
77,127,	77,128,	88,138,	98,18,	
86,136,	90,140,	91,141,	81,132,	
98,146,	100,18,	101,149,	102,18,	
103,18,	100,148,	105,18,	107,154,	
105,152,	109,18,	111,157,	103,151,	
110,18,	109,155,	102,150,	112,18,	
107,18,	111,18,	110,156,	113,18,	
114,18,	115,159,	114,158,	116,160,	
117,18,	116,18,	118,18,	119,18,	
120,18,	121,18,	122,18,	115,18,	
123,18,	124,18,	125,18,	126,18,	
127,18,	128,18,	129,18,	131,18,	
132,18,	130,18,	134,18,	135,18,	
133,18,	137,18,	136,18,	138,18,	
139,18,	118,161,	142,18,	120,162,	
121,164,	141,18,	140,18,	144,18,	
143,181,	145,18,	147,18,	146,183,	
148,18,	145,182,	131,173,	143,18,	
149,18,	120,163,	150,18,	122,165,	
146,18,	124,167,	123,166,	136,177,	
129,171,	152,18,	134,175,	127,170,	
140,179,	125,168,	126,169,	130,172,	
133,174,	135,176,	139,178,	141,180,	
151,18,	154,18,	151,184,	153,18,	
153,185,	155,18,	156,18,	157,186,	
158,18,	159,18,	159,187,	160,18,	
160,188,	161,18,	162,18,	163,18,	
157,18,	164,18,	165,18,	166,18,	
167,18,	168,18,	169,18,	170,18,	
171,18,	172,18,	173,18,	175,18,	
176,18,	174,18,	178,18,	177,18,	
182,18,	179,18,	180,18,	181,18,	
181,199,	183,18,	184,18,	162,189,	
185,18,	165,191,	186,18,	187,18,	
185,200,	189,18,	188,18,	190,18,	
191,18,	192,18,	193,18,	194,18,	
187,201,	164,190,	177,196,	188,202,	
195,18,	170,192,	172,193,	174,194,	
196,18,	197,18,	198,18,	200,18,	
199,18,	175,195,	179,197,	180,198,	
199,208,	201,18,	194,205,	202,18,	
203,18,	204,18,	200,209,	205,18,	
206,18,	207,18,	189,203,	208,213,	
209,18,	211,18,	210,18,	212,18,	
208,18,	213,215,	193,204,	214,18,	
215,18,	216,18,	217,18,	218,18,	
0,0,	215,217,	213,18,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	197,206,	198,207,	
0,0,	0,0,	0,0,	0,0,	
203,210,	210,214,	214,216,	0,0,	
0,0,	0,0,	0,0,	0,0,	
204,211,	0,0,	205,212,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	216,218,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+1,	0,		0,	
yycrank+0,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+5,
yycrank+-123,	0,		0,	
yycrank+0,	0,		yyvstop+7,
yycrank+2,	0,		yyvstop+9,
yycrank+0,	0,		yyvstop+11,
yycrank+0,	0,		yyvstop+13,
yycrank+0,	0,		yyvstop+15,
yycrank+86,	0,		yyvstop+17,
yycrank+124,	0,		0,	
yycrank+0,	0,		yyvstop+19,
yycrank+202,	0,		yyvstop+21,
yycrank+68,	yysvec+15,	yyvstop+23,
yycrank+80,	yysvec+15,	yyvstop+25,
yycrank+14,	yysvec+15,	yyvstop+27,
yycrank+72,	yysvec+15,	yyvstop+29,
yycrank+15,	yysvec+15,	yyvstop+31,
yycrank+92,	yysvec+15,	yyvstop+33,
yycrank+84,	yysvec+15,	yyvstop+35,
yycrank+247,	yysvec+15,	yyvstop+37,
yycrank+249,	yysvec+15,	yyvstop+39,
yycrank+261,	yysvec+15,	yyvstop+41,
yycrank+69,	yysvec+15,	yyvstop+43,
yycrank+53,	yysvec+15,	yyvstop+45,
yycrank+259,	yysvec+15,	yyvstop+47,
yycrank+73,	yysvec+15,	yyvstop+49,
yycrank+253,	yysvec+15,	yyvstop+51,
yycrank+254,	yysvec+15,	yyvstop+53,
yycrank+257,	yysvec+15,	yyvstop+55,
yycrank+16,	yysvec+15,	yyvstop+57,
yycrank+258,	yysvec+15,	yyvstop+59,
yycrank+265,	yysvec+15,	yyvstop+61,
yycrank+264,	yysvec+15,	yyvstop+63,
yycrank+51,	yysvec+15,	yyvstop+65,
yycrank+50,	yysvec+15,	yyvstop+67,
yycrank+263,	yysvec+15,	yyvstop+70,
yycrank+104,	yysvec+15,	yyvstop+72,
yycrank+188,	yysvec+15,	yyvstop+74,
yycrank+86,	yysvec+15,	yyvstop+76,
yycrank+28,	0,		yyvstop+78,
yycrank+0,	0,		yyvstop+80,
yycrank+-49,	yysvec+6,	yyvstop+82,
yycrank+0,	0,		yyvstop+84,
yycrank+335,	0,		0,	
yycrank+49,	yysvec+47,	yyvstop+86,
yycrank+270,	yysvec+15,	yyvstop+88,
yycrank+262,	yysvec+15,	yyvstop+90,
yycrank+275,	yysvec+15,	yyvstop+92,
yycrank+268,	yysvec+15,	yyvstop+94,
yycrank+271,	yysvec+15,	yyvstop+96,
yycrank+184,	yysvec+15,	yyvstop+98,
yycrank+385,	yysvec+15,	yyvstop+100,
yycrank+289,	yysvec+15,	yyvstop+102,
yycrank+108,	yysvec+15,	yyvstop+104,
yycrank+182,	yysvec+15,	yyvstop+106,
yycrank+138,	yysvec+15,	yyvstop+108,
yycrank+272,	yysvec+15,	yyvstop+110,
yycrank+88,	yysvec+15,	yyvstop+112,
yycrank+297,	yysvec+15,	yyvstop+115,
yycrank+320,	yysvec+15,	yyvstop+117,
yycrank+107,	yysvec+15,	yyvstop+119,
yycrank+277,	yysvec+15,	yyvstop+121,
yycrank+301,	yysvec+15,	yyvstop+123,
yycrank+139,	yysvec+15,	yyvstop+125,
yycrank+183,	yysvec+15,	yyvstop+127,
yycrank+218,	yysvec+15,	yyvstop+129,
yycrank+381,	yysvec+15,	yyvstop+131,
yycrank+349,	yysvec+15,	yyvstop+133,
yycrank+382,	yysvec+15,	yyvstop+135,
yycrank+348,	yysvec+15,	yyvstop+137,
yycrank+350,	yysvec+15,	yyvstop+139,
yycrank+302,	yysvec+15,	yyvstop+141,
yycrank+383,	yysvec+15,	yyvstop+143,
yycrank+389,	yysvec+15,	yyvstop+145,
yycrank+384,	yysvec+15,	yyvstop+147,
yycrank+316,	yysvec+15,	yyvstop+149,
yycrank+186,	yysvec+15,	yyvstop+151,
yycrank+391,	yysvec+15,	yyvstop+153,
yycrank+185,	yysvec+15,	yyvstop+155,
yycrank+392,	yysvec+15,	yyvstop+158,
yycrank+292,	yysvec+15,	yyvstop+160,
yycrank+393,	yysvec+15,	yyvstop+162,
yycrank+394,	yysvec+15,	yyvstop+164,
yycrank+396,	yysvec+15,	yyvstop+166,
yycrank+399,	yysvec+15,	yyvstop+168,
yycrank+318,	yysvec+15,	yyvstop+170,
yycrank+395,	yysvec+15,	yyvstop+172,
yycrank+398,	yysvec+15,	yyvstop+174,
yycrank+0,	0,		yyvstop+176,
yycrank+220,	yysvec+15,	yyvstop+178,
yycrank+406,	yysvec+15,	yyvstop+181,
yycrank+303,	yysvec+15,	yyvstop+183,
yycrank+411,	yysvec+15,	yyvstop+186,
yycrank+409,	yysvec+15,	yyvstop+188,
yycrank+425,	yysvec+15,	yyvstop+190,
yycrank+401,	yysvec+15,	yyvstop+192,
yycrank+431,	yysvec+15,	yyvstop+194,
yycrank+421,	yysvec+15,	yyvstop+197,
yycrank+433,	yysvec+15,	yyvstop+199,
yycrank+434,	yysvec+15,	yyvstop+201,
yycrank+317,	yysvec+15,	yyvstop+203,
yycrank+436,	yysvec+15,	yyvstop+206,
yycrank+413,	yysvec+15,	yyvstop+208,
yycrank+446,	yysvec+15,	yyvstop+210,
yycrank+404,	yysvec+15,	yyvstop+212,
yycrank+439,	yysvec+15,	yyvstop+215,
yycrank+442,	yysvec+15,	yyvstop+217,
yycrank+447,	yysvec+15,	yyvstop+219,
yycrank+445,	yysvec+15,	yyvstop+221,
yycrank+449,	yysvec+15,	yyvstop+224,
yycrank+450,	yysvec+15,	yyvstop+227,
yycrank+461,	yysvec+15,	yyvstop+229,
yycrank+455,	yysvec+15,	yyvstop+231,
yycrank+454,	yysvec+15,	yyvstop+233,
yycrank+456,	yysvec+15,	yyvstop+236,
yycrank+457,	yysvec+15,	yyvstop+239,
yycrank+458,	yysvec+15,	yyvstop+242,
yycrank+459,	yysvec+15,	yyvstop+244,
yycrank+460,	yysvec+15,	yyvstop+246,
yycrank+462,	yysvec+15,	yyvstop+248,
yycrank+463,	yysvec+15,	yyvstop+250,
yycrank+464,	yysvec+15,	yyvstop+253,
yycrank+465,	yysvec+15,	yyvstop+255,
yycrank+466,	yysvec+15,	yyvstop+257,
yycrank+467,	yysvec+15,	yyvstop+259,
yycrank+468,	yysvec+15,	yyvstop+262,
yycrank+471,	yysvec+15,	yyvstop+264,
yycrank+469,	yysvec+15,	yyvstop+266,
yycrank+470,	yysvec+15,	yyvstop+269,
yycrank+474,	yysvec+15,	yyvstop+272,
yycrank+472,	yysvec+15,	yyvstop+274,
yycrank+473,	yysvec+15,	yyvstop+276,
yycrank+476,	yysvec+15,	yyvstop+278,
yycrank+475,	yysvec+15,	yyvstop+280,
yycrank+477,	yysvec+15,	yyvstop+283,
yycrank+478,	yysvec+15,	yyvstop+286,
yycrank+484,	yysvec+15,	yyvstop+288,
yycrank+483,	yysvec+15,	yyvstop+290,
yycrank+480,	yysvec+15,	yyvstop+293,
yycrank+493,	yysvec+15,	yyvstop+296,
yycrank+485,	yysvec+15,	yyvstop+298,
yycrank+487,	yysvec+15,	yyvstop+301,
yycrank+498,	yysvec+15,	yyvstop+303,
yycrank+488,	yysvec+15,	yyvstop+305,
yycrank+490,	yysvec+15,	yyvstop+308,
yycrank+494,	yysvec+15,	yyvstop+311,
yycrank+496,	yysvec+15,	yyvstop+314,
yycrank+514,	yysvec+15,	yyvstop+317,
yycrank+503,	yysvec+15,	yyvstop+319,
yycrank+517,	yysvec+15,	yyvstop+322,
yycrank+515,	yysvec+15,	yyvstop+324,
yycrank+519,	yysvec+15,	yyvstop+327,
yycrank+520,	yysvec+15,	yyvstop+330,
yycrank+530,	yysvec+15,	yyvstop+333,
yycrank+522,	yysvec+15,	yyvstop+335,
yycrank+523,	yysvec+15,	yyvstop+338,
yycrank+525,	yysvec+15,	yyvstop+340,
yycrank+527,	yysvec+15,	yyvstop+342,
yycrank+528,	yysvec+15,	yyvstop+345,
yycrank+529,	yysvec+15,	yyvstop+347,
yycrank+531,	yysvec+15,	yyvstop+350,
yycrank+532,	yysvec+15,	yyvstop+352,
yycrank+533,	yysvec+15,	yyvstop+354,
yycrank+534,	yysvec+15,	yyvstop+357,
yycrank+535,	yysvec+15,	yyvstop+360,
yycrank+536,	yysvec+15,	yyvstop+363,
yycrank+537,	yysvec+15,	yyvstop+366,
yycrank+538,	yysvec+15,	yyvstop+368,
yycrank+539,	yysvec+15,	yyvstop+371,
yycrank+540,	yysvec+15,	yyvstop+374,
yycrank+543,	yysvec+15,	yyvstop+377,
yycrank+541,	yysvec+15,	yyvstop+380,
yycrank+542,	yysvec+15,	yyvstop+382,
yycrank+545,	yysvec+15,	yyvstop+385,
yycrank+544,	yysvec+15,	yyvstop+387,
yycrank+547,	yysvec+15,	yyvstop+390,
yycrank+548,	yysvec+15,	yyvstop+392,
yycrank+549,	yysvec+15,	yyvstop+394,
yycrank+546,	yysvec+15,	yyvstop+396,
yycrank+551,	yysvec+15,	yyvstop+399,
yycrank+552,	yysvec+15,	yyvstop+402,
yycrank+554,	yysvec+15,	yyvstop+405,
yycrank+556,	yysvec+15,	yyvstop+407,
yycrank+557,	yysvec+15,	yyvstop+410,
yycrank+560,	yysvec+15,	yyvstop+412,
yycrank+559,	yysvec+15,	yyvstop+414,
yycrank+561,	yysvec+15,	yyvstop+416,
yycrank+562,	yysvec+15,	yyvstop+419,
yycrank+563,	yysvec+15,	yyvstop+422,
yycrank+564,	yysvec+15,	yyvstop+425,
yycrank+565,	yysvec+15,	yyvstop+427,
yycrank+570,	yysvec+15,	yyvstop+429,
yycrank+574,	yysvec+15,	yyvstop+432,
yycrank+575,	yysvec+15,	yyvstop+435,
yycrank+576,	yysvec+15,	yyvstop+437,
yycrank+578,	yysvec+15,	yyvstop+439,
yycrank+577,	yysvec+15,	yyvstop+441,
yycrank+583,	yysvec+15,	yyvstop+443,
yycrank+585,	yysvec+15,	yyvstop+446,
yycrank+586,	yysvec+15,	yyvstop+449,
yycrank+587,	yysvec+15,	yyvstop+451,
yycrank+589,	yysvec+15,	yyvstop+453,
yycrank+590,	yysvec+15,	yyvstop+455,
yycrank+591,	yysvec+15,	yyvstop+458,
yycrank+598,	yysvec+15,	yyvstop+461,
yycrank+594,	yysvec+15,	yyvstop+463,
yycrank+596,	yysvec+15,	yyvstop+466,
yycrank+595,	yysvec+15,	yyvstop+468,
yycrank+597,	yysvec+15,	yyvstop+471,
yycrank+608,	yysvec+15,	yyvstop+474,
yycrank+601,	yysvec+15,	yyvstop+476,
yycrank+602,	yysvec+15,	yyvstop+478,
yycrank+603,	yysvec+15,	yyvstop+480,
yycrank+604,	yysvec+15,	yyvstop+482,
yycrank+605,	yysvec+15,	yyvstop+485,
0,	0,	0};
struct yywork *yytop = yycrank+718;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
040 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,'-' ,'.' ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
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

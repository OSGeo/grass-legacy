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
#ifdef DEBUG
#   include <assert.h>
main()
{
char *p;

assert(sizeof(int) >= sizeof(char *));

while (p = (char *) yylex())
printf("%-10.10s is \"%s\"\n", p, yytext);
}

s_lookup() {}
int yynerrs = 0;

#   define token(x)      (int) "x"

#else   ! DEBUG

#   include   "symtab.h"
#   include   "gram.h"
#   define     token(x)      x

#endif   DEBUG

#define   END(v)   (v-1 + sizeof v / sizeof v[0])
static int screen();
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
                    ;
break;
case 2:
{
return token(NO_PROBABILITY_MAPS); }
break;
case 3:
    {
return token(NO_COMBINED_MAP); }
break;
case 4:
 {
return token(COMBINED_MAP); }
break;
case 5:
 {
return token(ASPECT); }
break;
case 6:
 {
return token(GREY); }
break;
case 7:
 {
return token(GREY); }
break;
case 8:
 {
return token(HISTO); }
break;
case 9:
 {
return token(RAINBOW); }
break;
case 10:
 {
return token(RAMP); }
break;
case 11:
 {
return token(RANDOM); }
break;
case 12:
 {
return token(REDYELLOWGREEN); }
break;
case 13:
 {
return token(WAVE); }
break;
case 14:
return screen();
break;
case 15:
     {
s_lookup(token(String));
return token(String);
}
break;
case 16:
  case 17:
       {
s_lookup(token(Constant));
return token(Constant);
}
break;
case 18:
            ;
break;
case 19:
                   return token(yytext[0]);
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */

static struct rwtable {
char   *rw_name;
int    rw_yylex;
} rwtable[] = {
"layer",         token(LAYER),
"context",       token(CONTEXT),
"subjective",    token(SUBJECTIVE),
"inferred",      token(INFERRED),
"question",      token(QUESTION),
"thru",          token(THRU),
};

static int screen()
{
struct rwtable *low = rwtable,
*high = END(rwtable);

int c;

while ( low <= high ) {
if ((c = strcmp(low->rw_name,yytext)) == 0)
return low->rw_yylex;
low++;
}
s_lookup(token(Identifier));
return token(Identifier);
}

int yyvstop[] = {
0,

19,
0,

18,
19,
0,

18,
0,

19,
0,

19,
-1,
0,

19,
0,

16,
19,
0,

14,
19,
0,

14,
19,
0,

14,
19,
0,

14,
19,
0,

14,
19,
0,

14,
19,
0,

14,
19,
0,

14,
19,
0,

15,
0,

-1,
0,

1,
0,

17,
0,

16,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

15,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

12,
14,
0,

14,
0,

14,
0,

14,
0,

7,
14,
0,

6,
14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

10,
14,
0,

14,
0,

13,
14,
0,

14,
0,

14,
0,

8,
14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

11,
14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

9,
14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

4,
14,
0,

14,
0,

14,
0,

5,
14,
0,

14,
0,

14,
0,

3,
14,
0,

14,
0,

14,
0,

14,
0,

14,
0,

2,
14,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
0,0,	0,0,	0,0,	4,5,	
4,5,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	18,0,	
20,18,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,6,	
1,7,	2,7,	4,5,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	35,0,	0,0,	1,8,	
20,35,	1,9,	0,0,	0,0,	
0,0,	6,18,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	6,18,	6,0,	0,0,	
0,0,	0,0,	1,10,	1,11,	
1,12,	1,11,	1,11,	1,11,	
1,13,	1,14,	1,11,	0,0,	
0,0,	1,11,	1,11,	1,15,	
1,11,	1,11,	0,0,	1,16,	
1,11,	1,11,	6,19,	1,11,	
1,17,	0,0,	1,11,	0,0,	
0,0,	0,0,	11,26,	0,0,	
1,3,	2,3,	6,18,	0,0,	
6,18,	14,30,	0,0,	0,0,	
7,21,	18,20,	20,20,	0,0,	
26,26,	0,0,	12,28,	14,26,	
7,21,	7,22,	12,26,	13,29,	
13,26,	6,18,	6,18,	6,18,	
6,18,	6,18,	6,18,	6,18,	
6,18,	6,18,	11,26,	35,20,	
6,18,	6,18,	6,18,	6,18,	
6,18,	14,30,	6,18,	6,18,	
6,18,	7,21,	6,18,	6,18,	
26,26,	6,18,	12,28,	14,26,	
6,20,	30,40,	12,26,	13,29,	
13,26,	7,21,	0,0,	7,21,	
8,23,	8,23,	8,23,	8,23,	
8,23,	8,23,	8,23,	8,23,	
8,23,	8,23,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
7,21,	7,21,	7,21,	7,21,	
7,21,	7,21,	7,21,	7,21,	
7,21,	30,40,	0,0,	7,21,	
7,21,	7,21,	7,21,	7,21,	
15,31,	7,21,	7,21,	7,21,	
15,26,	7,21,	7,21,	9,24,	
7,21,	9,25,	9,25,	9,25,	
9,25,	9,25,	9,25,	9,25,	
9,25,	9,25,	9,25,	10,26,	
0,0,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	0,0,	
15,31,	46,26,	27,36,	50,26,	
15,26,	27,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
10,27,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
0,0,	46,26,	27,36,	50,26,	
10,26,	27,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
10,27,	10,26,	10,26,	10,26,	
10,26,	10,26,	10,26,	10,26,	
16,32,	17,34,	28,37,	29,38,	
31,41,	32,43,	51,26,	29,39,	
28,26,	32,44,	32,45,	33,46,	
36,48,	34,26,	37,49,	32,26,	
34,47,	31,42,	16,26,	17,26,	
31,26,	29,26,	38,26,	33,26,	
16,33,	0,0,	36,26,	0,0,	
38,50,	40,26,	40,52,	37,26,	
16,32,	17,34,	28,37,	29,38,	
31,41,	32,43,	51,26,	29,39,	
28,26,	32,44,	32,45,	33,46,	
36,48,	34,26,	37,49,	32,26,	
34,47,	31,42,	16,26,	17,26,	
31,26,	29,26,	38,26,	33,26,	
16,33,	45,57,	36,26,	39,26,	
38,50,	40,26,	40,52,	37,26,	
41,53,	39,51,	42,54,	42,26,	
41,26,	43,55,	44,56,	47,58,	
45,26,	44,26,	43,26,	52,61,	
48,59,	49,60,	53,62,	52,26,	
0,0,	56,26,	54,63,	58,26,	
53,26,	47,26,	54,26,	49,26,	
0,0,	45,57,	57,65,	39,26,	
48,26,	61,26,	57,26,	55,64,	
41,53,	39,51,	42,54,	42,26,	
41,26,	43,55,	44,56,	47,58,	
45,26,	44,26,	43,26,	52,61,	
48,59,	49,60,	53,62,	52,26,	
55,26,	56,26,	54,63,	58,26,	
53,26,	47,26,	54,26,	49,26,	
59,26,	59,66,	57,65,	60,67,	
48,26,	61,26,	57,26,	55,64,	
60,26,	62,68,	63,69,	64,70,	
65,71,	66,72,	67,73,	64,26,	
69,75,	70,26,	65,26,	71,26,	
68,74,	70,76,	73,78,	76,26,	
55,26,	0,0,	62,26,	63,26,	
67,26,	66,26,	68,26,	0,0,	
59,26,	59,66,	69,26,	60,67,	
0,0,	73,26,	0,0,	75,80,	
60,26,	62,68,	63,69,	64,70,	
65,71,	66,72,	67,73,	64,26,	
69,75,	70,26,	65,26,	71,26,	
68,74,	70,76,	73,78,	76,26,	
75,26,	77,81,	62,26,	63,26,	
67,26,	66,26,	68,26,	72,77,	
77,26,	74,79,	69,26,	72,26,	
78,82,	73,26,	74,26,	75,80,	
79,83,	80,84,	78,26,	81,85,	
82,86,	83,87,	84,88,	81,26,	
85,89,	85,26,	0,0,	80,26,	
86,90,	84,26,	79,26,	86,26,	
75,26,	77,81,	89,93,	88,92,	
83,26,	90,26,	82,26,	72,77,	
77,26,	74,79,	93,26,	72,26,	
78,82,	88,26,	74,26,	91,94,	
79,83,	80,84,	78,26,	81,85,	
82,86,	83,87,	84,88,	81,26,	
85,89,	85,26,	87,91,	80,26,	
86,90,	84,26,	79,26,	86,26,	
87,26,	91,26,	89,93,	88,92,	
83,26,	90,26,	82,26,	92,26,	
92,95,	94,96,	93,26,	95,26,	
94,26,	88,26,	96,26,	91,94,	
97,98,	95,97,	98,99,	100,101,	
99,100,	101,26,	97,26,	99,26,	
0,0,	0,0,	87,91,	0,0,	
0,0,	0,0,	0,0,	0,0,	
87,26,	91,26,	0,0,	0,0,	
98,26,	0,0,	0,0,	92,26,	
92,95,	94,96,	0,0,	95,26,	
94,26,	0,0,	96,26,	0,0,	
97,98,	95,97,	98,99,	100,101,	
99,100,	101,26,	97,26,	99,26,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
98,26,	0,0,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-2,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+6,	0,		yyvstop+3,
yycrank+0,	yysvec+4,	yyvstop+6,
yycrank+-52,	0,		yyvstop+8,
yycrank+-103,	0,		yyvstop+10,
yycrank+104,	0,		yyvstop+13,
yycrank+145,	0,		yyvstop+15,
yycrank+157,	0,		yyvstop+18,
yycrank+11,	yysvec+10,	yyvstop+21,
yycrank+31,	yysvec+10,	yyvstop+24,
yycrank+33,	yysvec+10,	yyvstop+27,
yycrank+28,	yysvec+10,	yyvstop+30,
yycrank+105,	yysvec+10,	yyvstop+33,
yycrank+215,	yysvec+10,	yyvstop+36,
yycrank+216,	yysvec+10,	yyvstop+39,
yycrank+-13,	yysvec+6,	0,	
yycrank+0,	0,		yyvstop+42,
yycrank+-14,	yysvec+6,	0,	
yycrank+0,	yysvec+7,	yyvstop+44,
yycrank+0,	0,		yyvstop+46,
yycrank+0,	yysvec+8,	yyvstop+48,
yycrank+0,	yysvec+8,	0,	
yycrank+0,	yysvec+9,	yyvstop+50,
yycrank+25,	yysvec+10,	yyvstop+52,
yycrank+138,	yysvec+10,	yyvstop+54,
yycrank+205,	yysvec+10,	yyvstop+56,
yycrank+218,	yysvec+10,	yyvstop+58,
yycrank+62,	yysvec+10,	yyvstop+60,
yycrank+217,	yysvec+10,	yyvstop+62,
yycrank+212,	yysvec+10,	yyvstop+64,
yycrank+220,	yysvec+10,	yyvstop+66,
yycrank+210,	yysvec+10,	yyvstop+68,
yycrank+-35,	yysvec+6,	yyvstop+70,
yycrank+223,	yysvec+10,	yyvstop+72,
yycrank+228,	yysvec+10,	yyvstop+74,
yycrank+219,	yysvec+10,	yyvstop+76,
yycrank+256,	yysvec+10,	yyvstop+78,
yycrank+226,	yysvec+10,	yyvstop+80,
yycrank+265,	yysvec+10,	yyvstop+82,
yycrank+264,	yysvec+10,	yyvstop+84,
yycrank+271,	yysvec+10,	yyvstop+86,
yycrank+270,	yysvec+10,	yyvstop+88,
yycrank+269,	yysvec+10,	yyvstop+90,
yycrank+134,	yysvec+10,	yyvstop+92,
yycrank+282,	yysvec+10,	yyvstop+95,
yycrank+289,	yysvec+10,	yyvstop+97,
yycrank+284,	yysvec+10,	yyvstop+99,
yycrank+136,	yysvec+10,	yyvstop+101,
yycrank+203,	yysvec+10,	yyvstop+104,
yycrank+276,	yysvec+10,	yyvstop+107,
yycrank+281,	yysvec+10,	yyvstop+109,
yycrank+283,	yysvec+10,	yyvstop+111,
yycrank+309,	yysvec+10,	yyvstop+113,
yycrank+278,	yysvec+10,	yyvstop+115,
yycrank+291,	yysvec+10,	yyvstop+118,
yycrank+280,	yysvec+10,	yyvstop+120,
yycrank+317,	yysvec+10,	yyvstop+123,
yycrank+325,	yysvec+10,	yyvstop+125,
yycrank+290,	yysvec+10,	yyvstop+127,
yycrank+343,	yysvec+10,	yyvstop+130,
yycrank+344,	yysvec+10,	yyvstop+132,
yycrank+332,	yysvec+10,	yyvstop+134,
yycrank+335,	yysvec+10,	yyvstop+136,
yycrank+346,	yysvec+10,	yyvstop+138,
yycrank+345,	yysvec+10,	yyvstop+140,
yycrank+347,	yysvec+10,	yyvstop+142,
yycrank+351,	yysvec+10,	yyvstop+144,
yycrank+334,	yysvec+10,	yyvstop+146,
yycrank+336,	yysvec+10,	yyvstop+148,
yycrank+384,	yysvec+10,	yyvstop+151,
yycrank+354,	yysvec+10,	yyvstop+153,
yycrank+387,	yysvec+10,	yyvstop+155,
yycrank+373,	yysvec+10,	yyvstop+157,
yycrank+340,	yysvec+10,	yyvstop+159,
yycrank+381,	yysvec+10,	yyvstop+162,
yycrank+391,	yysvec+10,	yyvstop+164,
yycrank+403,	yysvec+10,	yyvstop+166,
yycrank+400,	yysvec+10,	yyvstop+168,
yycrank+396,	yysvec+10,	yyvstop+170,
yycrank+411,	yysvec+10,	yyvstop+172,
yycrank+409,	yysvec+10,	yyvstop+174,
yycrank+402,	yysvec+10,	yyvstop+176,
yycrank+398,	yysvec+10,	yyvstop+178,
yycrank+404,	yysvec+10,	yyvstop+180,
yycrank+437,	yysvec+10,	yyvstop+182,
yycrank+418,	yysvec+10,	yyvstop+184,
yycrank+407,	yysvec+10,	yyvstop+186,
yycrank+410,	yysvec+10,	yyvstop+188,
yycrank+438,	yysvec+10,	yyvstop+191,
yycrank+444,	yysvec+10,	yyvstop+193,
yycrank+415,	yysvec+10,	yyvstop+195,
yycrank+449,	yysvec+10,	yyvstop+198,
yycrank+448,	yysvec+10,	yyvstop+200,
yycrank+451,	yysvec+10,	yyvstop+202,
yycrank+459,	yysvec+10,	yyvstop+205,
yycrank+473,	yysvec+10,	yyvstop+207,
yycrank+460,	yysvec+10,	yyvstop+209,
yycrank+456,	yysvec+10,	yyvstop+211,
yycrank+458,	yysvec+10,	yyvstop+213,
0,	0,	0};
struct yywork *yytop = yycrank+588;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,'"' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,'.' ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'B' ,'C' ,'D' ,'E' ,'F' ,'G' ,
'H' ,'I' ,'F' ,'F' ,'L' ,'M' ,'N' ,'O' ,
'P' ,'F' ,'R' ,'S' ,'T' ,'F' ,'V' ,'W' ,
'F' ,'Y' ,'F' ,01  ,01  ,01  ,01  ,'.' ,
01  ,'A' ,'B' ,'C' ,'D' ,'E' ,'F' ,'G' ,
'H' ,'I' ,'F' ,'F' ,'L' ,'M' ,'N' ,'O' ,
'P' ,'F' ,'R' ,'S' ,'T' ,'F' ,'V' ,'W' ,
'F' ,'Y' ,'F' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,1,0,0,0,0,0,0,
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

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
             { return AND; }
break;
case 3:
             { return OR; }
break;
case 4:
               { return GT; }
break;
case 5:
             { return GE; }
break;
case 6:
               { return LT; }
break;
case 7:
             { return LE; }
break;
case 8:
             { return EQ; }
break;
case 9:
             { return NE; }
break;
case 10:
case 11:
{
		yylval = store (yytext);
		return FLOAT ;
	       }
break;
case 12:
    {
		yylval = atoi (yytext);
		return INTEGER;
	    }
break;
case 13:
      { return COLOR_RED;}
break;
case 14:
      { return COLOR_GREEN;}
break;
case 15:
      { return COLOR_BLUE;}
break;
case 16:
          { return COLOR_GRAY;}
break;
case 17:
 {
	        yylval = store (yytext);
		begin_function();
		return FUNCTION; 
	    }
break;
case 18:
             {
		      yylval = store (yytext);
		      return NAME; 
	             }
break;
case 19:
  case 20:
  {
		yytext[yyleng-1] = 0;
		yylval = store (yytext+1);
		return STRING; 
	    }
break;
case 21:
     case 22:
         { return (int) *yytext; }
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

12,
0,

12,
0,

18,
21,
-17,
0,

1,
21,
0,

22,
0,

21,
0,

21,
0,

16,
21,
0,

21,
0,

21,
0,

21,
0,

18,
21,
-17,
0,

12,
18,
21,
-17,
0,

6,
21,
0,

21,
0,

4,
21,
0,

18,
21,
-17,
0,

18,
21,
-17,
0,

18,
21,
-17,
0,

21,
0,

18,
-17,
0,

17,
0,

9,
0,

2,
0,

11,
18,
-17,
0,

10,
18,
-17,
0,

12,
18,
-17,
0,

7,
0,

8,
0,

5,
0,

15,
0,

14,
0,

13,
0,

3,
0,

19,
0,

20,
0,

10,
11,
18,
-17,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
22,22,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,6,	1,7,	
1,8,	25,38,	1,9,	1,10,	
1,11,	1,9,	1,9,	1,9,	
1,9,	1,9,	1,9,	1,12,	
1,9,	1,13,	10,26,	22,22,	
27,39,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	22,23,	
0,0,	1,14,	1,15,	1,16,	
6,24,	1,9,	14,31,	1,17,	
15,32,	16,33,	0,0,	0,0,	
1,18,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	2,8,	
0,0,	2,9,	2,10,	1,19,	
2,9,	2,9,	2,9,	2,9,	
2,9,	2,9,	2,12,	2,9,	
1,9,	0,0,	1,9,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
2,14,	2,15,	2,16,	0,0,	
2,9,	0,0,	3,21,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	3,22,	3,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,20,	20,37,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	2,9,	
0,0,	2,9,	0,0,	0,0,	
0,0,	0,0,	3,0,	3,0,	
3,0,	0,0,	3,0,	3,0,	
3,0,	3,23,	3,0,	3,0,	
3,0,	3,0,	3,0,	0,0,	
3,0,	3,21,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
2,20,	3,0,	3,0,	3,0,	
0,0,	3,0,	7,25,	3,21,	
11,27,	0,0,	0,0,	0,0,	
3,21,	0,0,	7,25,	7,25,	
11,27,	11,27,	0,0,	0,0,	
0,0,	0,0,	0,0,	3,21,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
3,0,	0,0,	3,0,	0,0,	
0,0,	0,0,	7,25,	7,0,	
11,27,	11,27,	0,0,	0,0,	
7,25,	0,0,	11,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	7,25,	0,0,	11,27,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	3,0,	0,0,	12,0,	
0,0,	0,0,	0,0,	7,25,	
0,0,	11,27,	0,0,	0,0,	
7,25,	0,0,	11,27,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	7,25,	
0,0,	11,27,	12,0,	12,0,	
12,0,	0,0,	12,0,	12,0,	
12,0,	12,23,	12,0,	12,0,	
12,0,	12,0,	12,0,	13,0,	
12,0,	12,28,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	12,0,	12,0,	12,0,	
0,0,	12,0,	0,0,	0,0,	
0,0,	0,0,	13,0,	13,0,	
13,0,	0,0,	13,0,	13,0,	
13,0,	13,23,	13,0,	13,0,	
13,0,	13,0,	13,0,	13,29,	
13,0,	13,30,	17,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
12,0,	0,0,	12,0,	0,0,	
0,0,	13,0,	13,0,	13,0,	
0,0,	13,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	17,0,	17,0,	17,34,	
0,0,	17,0,	17,0,	17,0,	
17,23,	17,0,	17,0,	17,0,	
17,0,	17,0,	18,0,	17,0,	
0,0,	12,0,	0,0,	0,0,	
13,0,	0,0,	13,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
17,0,	17,0,	17,0,	0,0,	
17,0,	0,0,	0,0,	0,0,	
0,0,	18,0,	18,0,	18,35,	
0,0,	18,0,	18,0,	18,0,	
18,23,	18,0,	18,0,	18,0,	
18,0,	18,0,	19,0,	18,0,	
0,0,	13,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	17,0,	
0,0,	17,0,	0,0,	0,0,	
18,0,	18,0,	18,0,	0,0,	
18,0,	0,0,	0,0,	0,0,	
0,0,	19,0,	19,0,	19,36,	
0,0,	19,0,	19,0,	19,0,	
19,23,	19,0,	19,0,	19,0,	
19,0,	19,0,	21,0,	19,0,	
0,0,	0,0,	0,0,	0,0,	
17,0,	0,0,	0,0,	18,0,	
0,0,	18,0,	0,0,	0,0,	
19,0,	19,0,	19,0,	0,0,	
19,0,	0,0,	0,0,	0,0,	
0,0,	21,0,	21,0,	21,0,	
0,0,	21,0,	21,0,	21,0,	
21,23,	21,0,	21,0,	21,0,	
21,0,	21,0,	28,0,	21,0,	
0,0,	0,0,	0,0,	0,0,	
18,0,	0,0,	0,0,	19,0,	
0,0,	19,0,	0,0,	0,0,	
21,0,	21,0,	21,0,	0,0,	
21,0,	0,0,	0,0,	0,0,	
0,0,	28,0,	28,0,	28,0,	
0,0,	28,0,	28,0,	28,0,	
28,23,	28,0,	28,0,	28,0,	
28,0,	28,0,	29,0,	28,0,	
28,28,	0,0,	0,0,	0,0,	
19,0,	0,0,	0,0,	21,0,	
0,0,	21,0,	0,0,	0,0,	
28,0,	28,0,	28,0,	0,0,	
28,0,	0,0,	0,0,	0,0,	
0,0,	29,0,	29,0,	29,0,	
0,0,	29,0,	29,0,	29,0,	
29,23,	29,0,	29,0,	29,0,	
29,0,	29,0,	30,0,	29,0,	
29,40,	0,0,	0,0,	0,0,	
21,0,	0,0,	0,0,	28,0,	
0,0,	28,0,	0,0,	0,0,	
29,0,	29,0,	29,0,	0,0,	
29,0,	0,0,	0,0,	0,0,	
0,0,	30,0,	30,0,	30,0,	
0,0,	30,0,	30,0,	30,0,	
30,23,	30,0,	30,0,	30,0,	
30,0,	30,0,	30,29,	30,0,	
30,30,	40,0,	0,0,	0,0,	
28,0,	0,0,	0,0,	29,0,	
0,0,	29,0,	0,0,	0,0,	
30,0,	30,0,	30,0,	0,0,	
30,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
40,0,	40,0,	40,0,	0,0,	
40,0,	40,0,	40,0,	40,23,	
40,0,	40,0,	40,0,	40,0,	
40,0,	0,0,	40,0,	40,40,	
29,0,	0,0,	0,0,	30,0,	
0,0,	30,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	40,0,	
40,0,	40,0,	0,0,	40,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
30,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	40,0,	0,0,	
40,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	40,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		yyvstop+1,
yycrank+-44,	yysvec+1,	yyvstop+3,
yycrank+-109,	0,		yyvstop+5,
yycrank+0,	0,		yyvstop+9,
yycrank+0,	0,		yyvstop+12,
yycrank+3,	0,		yyvstop+14,
yycrank+-173,	0,		yyvstop+16,
yycrank+0,	0,		yyvstop+18,
yycrank+0,	0,		yyvstop+21,
yycrank+12,	0,		yyvstop+23,
yycrank+-175,	0,		yyvstop+25,
yycrank+-225,	yysvec+3,	yyvstop+27,
yycrank+-261,	yysvec+3,	yyvstop+31,
yycrank+5,	0,		yyvstop+36,
yycrank+7,	0,		yyvstop+39,
yycrank+8,	0,		yyvstop+41,
yycrank+-300,	yysvec+3,	yyvstop+44,
yycrank+-336,	yysvec+3,	yyvstop+48,
yycrank+-372,	yysvec+3,	yyvstop+52,
yycrank+2,	0,		yyvstop+56,
yycrank+-408,	yysvec+3,	yyvstop+58,
yycrank+19,	0,		0,	
yycrank+0,	0,		yyvstop+61,
yycrank+0,	0,		yyvstop+63,
yycrank+-3,	yysvec+7,	0,	
yycrank+0,	0,		yyvstop+65,
yycrank+-13,	yysvec+11,	0,	
yycrank+-444,	yysvec+3,	yyvstop+67,
yycrank+-480,	yysvec+3,	yyvstop+71,
yycrank+-516,	yysvec+3,	yyvstop+75,
yycrank+0,	0,		yyvstop+79,
yycrank+0,	0,		yyvstop+81,
yycrank+0,	0,		yyvstop+83,
yycrank+0,	0,		yyvstop+85,
yycrank+0,	0,		yyvstop+87,
yycrank+0,	0,		yyvstop+89,
yycrank+0,	0,		yyvstop+91,
yycrank+0,	0,		yyvstop+93,
yycrank+0,	0,		yyvstop+95,
yycrank+-555,	yysvec+3,	yyvstop+97,
0,	0,	0};
struct yywork *yytop = yycrank+679;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,'!' ,'"' ,'!' ,01  ,'!' ,'!' ,047 ,
'!' ,'!' ,'!' ,'!' ,'!' ,'!' ,01  ,'!' ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,'!' ,'!' ,'!' ,01  ,
'!' ,01  ,'B' ,01  ,01  ,01  ,01  ,'G' ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,'R' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,'!' ,01  ,'!' ,01  ,01  ,
01  ,01  ,'B' ,01  ,01  ,01  ,01  ,'G' ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,'R' ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,'!' ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,1,0,0,0,0,0,0,
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

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
#    ifndef Boolean
typedef char Boolean;
#    endif
#   include "y.tab.h"
YYSTYPE yylval;
char *lexbuf;
char *SaveString();
int lexlen;
main()
{
char *p;

assert(sizeof(int) >= sizeof(char *));

while (p = (char *) yylex())
printf("%-16.16s is \"%s\"\n", p, yytext);
}

char *
SaveString(s)
    char *s;
{
    char *cp = (char *)XtMalloc(strlen(s) + 1);
    
    if ( cp ) {
        strcpy(cp,s);
        return(cp);
    }
    XgenFatalError("save string","out of memory");    
}


s_lookup() {}
int yynerrs = 0;

#   define token(x)      (int) "x"

#else   ! DEBUG

#    ifndef Boolean
typedef char Boolean;
#    endif
#   include "y.tab.h"
#   define     token(x)      x
char *SaveString();
double atof();
char *lexbuf;
int lexlen;

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
     { /* find quoted stuff */
char *strncpy();
void XtFree();

lexbuf = SaveString(yytext);
lexlen = strlen(lexbuf) - 2;
(void)strncpy(yytext,&lexbuf[1],lexlen);
yytext[lexlen] = '\0';
XtFree(lexbuf);
yylval.cval = yytext;
return token(String);
}
break;
case 2:
case 3:
case 4:
       { /* find real "%age" constants */
yylval.dval = atof(yytext);
return token(Real);
}
break;
case 5:
case 6:
case 7:
                { /* find "pixel" constants */
yylval.ival = atoi(yytext);
return token(Integer);
}
break;
case 8:
            ;
break;
case 9:
   { /* find setXXXXX and parse it here (Yuuchh) */
yylval.cval = SaveString(&yytext[3]);
return token(Set);
}
break;
case 10:
     { /* find colors (hex) */
yylval.cval = SaveString(yytext);
return token(String);
}
break;
case 11:
{ return screen(); }
break;
case 12:
                   return token(yytext[0]);
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */

static struct rwtable {
    char *rw_name;
    int rw_yylex;
} rwtable[] = {
    "environment",            token(Environment),
    "initialshells",          token(InitialShells),
    "menu",                   token(Menu),
    "commandboard",           token(CommandBoard),
    "label",                  token(Label),
    "message",                token(Message),
    "list",                   token(List),
    "button",                 token(PushButton),
    "textentry",              token(TextEntry),
    "table",                  token(Table),
    "separator",              token(Separator),
    "slider",                 token(Slider),
    "toggle",                 token(Toggle),
    "pulldown",               token(PullDown),
    "x",                      token(X),
    "dx",                     token(DX),
    "y",                      token(Y),
    "dy",                     token(DY),
    "width",                  token(Width),
    "height",                 token(Height),
    "maxwidth",               token(MaxWidth),
    "maxheight",              token(MaxHeight),
    "columns",                token(Columns),
    "font",                   token(Font),
    "fixedfont",              token(FixedFont),
    "entryfont",              token(EntryFont),
    "background",             token(Background),
    "foreground",             token(Foreground),
    "backgroundpixmap",       token(BackgroundPixmap),
    "topshadowcolor",         token(TopShadowColor),
    "topshadowpixmap",        token(TopShadowPixmap),
    "bottomshadowcolor",      token(BottomShadowColor),
    "bottomshadowpixmap",     token(BottomShadowPixmap),
    "override",               token(Override),
    "popup",                  token(Popup),
    "popdown",                token(Popdown),
    "destroy",                token(Destroy),
    "exit",                   token(Exit),
    "help",                   token(Help),
    "eval",                   token(Eval),
    "runforeground",          token(RunForeground),
    "runbackground",          token(RunBackground),
    "inputfrom",              token(InputFrom),
    "captureoutput",          token(CaptureOutput),
    "updatefrom",             token(UpdateFrom),
    "pane",                   token(Pane),
    "store",                  token(Store),
    "getenv",                 token(GetEnv),
    "clear",                  token(Clear),
    "commandarg",             token(CommandArg),
    "set",                    token(Set),
    "alignment",              token(Alignment),
    "listelement",            token(ListElement),
    "listseparator",          token(ListSeparator),
    "listtype",               token(ListType),
    "visibleitems",           token(VisibleItems),
    "scrollbar",              token(ScrollBar),
    "valuestring",            token(ValueString),
    "labelpixmap",            token(LabelPixmap),
    "maxlength",              token(MaxLength),
    "minimum",                token(Minimum),
    "maximum",                token(Maximum),
    "startvalue",             token(StartValue),
    "sliderwidth",            token(SliderWidth),
    "sliderheight",           token(SliderHeight),
    "orientation",            token(Orientation),
    "decimalpoints",          token(DecimalPoints),
    "rows",                   token(Rows),
    "rowsdisplayed",          token(RowsDisplayed),
    "columnsdisplayed",       token(ColumnsDisplayed),
    "columnheadings",         token(ColumnHeadings),
    "rowheadings",            token(RowHeadings),
    "rowvalue",               token(RowValue),
    "tablevalue",             token(TableValue),
    "rowheight",              token(RowHeight),
    "columnwidth",            token(ColumnWidth),
    "newline",                token(Newline),
    "titlestring",            token(TitleString),
    "toggletype",             token(ToggleType),
    "separatortype",          token(SeparatorType),
    "sensitive",              token(Sensitive),
    "insensitive",            token(Insensitive),
};

static int screen()
{
extern char yytext[];
struct rwtable *low = rwtable,
*high = END(rwtable);

while ( low <= high ) {
if (strcmp(low->rw_name,yytext) == 0)
return low->rw_yylex;
low++;
}
yylval.cval = SaveString(yytext);
return token(String);
}

int yyvstop[] = {
0,

12,
0,

8,
12,
0,

8,
0,

12,
0,

12,
0,

11,
12,
0,

12,
0,

12,
0,

12,
0,

7,
12,
0,

11,
12,
0,

1,
0,

10,
0,

11,
0,

6,
0,

5,
0,

4,
0,

7,
0,

11,
0,

1,
0,

3,
0,

2,
0,

11,
0,

9,
11,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
0,0,	0,0,	0,0,	4,5,	
4,5,	0,0,	6,14,	14,0,	
27,0,	16,14,	0,0,	0,0,	
0,0,	0,0,	6,14,	6,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,6,	
1,7,	1,8,	4,5,	0,0,	
0,0,	0,0,	2,7,	0,0,	
1,9,	16,27,	1,10,	1,11,	
1,8,	1,12,	2,9,	6,15,	
2,10,	6,14,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	6,14,	
6,14,	6,14,	9,19,	0,0,	
9,20,	9,20,	9,20,	9,20,	
9,20,	9,20,	9,20,	9,20,	
9,20,	9,20,	10,21,	0,0,	
10,22,	10,22,	10,22,	10,22,	
10,22,	10,22,	10,22,	10,22,	
10,22,	10,22,	11,23,	11,23,	
11,23,	11,23,	11,23,	11,23,	
11,23,	11,23,	11,23,	11,23,	
0,0,	14,16,	27,16,	16,16,	
0,0,	0,0,	0,0,	0,0,	
0,0,	6,16,	13,26,	0,0,	
0,0,	0,0,	0,0,	0,0,	
1,13,	0,0,	0,0,	0,0,	
0,0,	0,0,	2,13,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	26,30,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
0,0,	0,0,	0,0,	0,0,	
7,17,	0,0,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
7,17,	7,17,	7,17,	7,17,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	0,0,	0,0,	0,0,	
0,0,	8,18,	0,0,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	8,18,	8,18,	8,18,	
8,18,	12,24,	0,0,	12,25,	
12,25,	12,25,	12,25,	12,25,	
12,25,	12,25,	12,25,	12,25,	
12,25,	19,28,	19,28,	19,28,	
19,28,	19,28,	19,28,	19,28,	
19,28,	19,28,	19,28,	21,29,	
21,29,	21,29,	21,29,	21,29,	
21,29,	21,29,	21,29,	21,29,	
21,29,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	0,0,	0,0,	
0,0,	0,0,	30,31,	0,0,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	30,31,	30,31,	
30,31,	30,31,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-7,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+6,	0,		yyvstop+3,
yycrank+0,	yysvec+4,	yyvstop+6,
yycrank+-17,	0,		yyvstop+8,
yycrank+77,	0,		yyvstop+10,
yycrank+154,	0,		yyvstop+12,
yycrank+20,	0,		yyvstop+15,
yycrank+32,	0,		yyvstop+17,
yycrank+42,	0,		yyvstop+19,
yycrank+231,	0,		yyvstop+21,
yycrank+9,	yysvec+8,	yyvstop+24,
yycrank+-9,	yysvec+6,	0,	
yycrank+0,	0,		yyvstop+27,
yycrank+-11,	yysvec+6,	0,	
yycrank+0,	yysvec+7,	yyvstop+29,
yycrank+0,	yysvec+8,	yyvstop+31,
yycrank+241,	0,		0,	
yycrank+0,	yysvec+9,	yyvstop+33,
yycrank+251,	0,		0,	
yycrank+0,	yysvec+10,	yyvstop+35,
yycrank+0,	yysvec+11,	yyvstop+37,
yycrank+0,	yysvec+11,	0,	
yycrank+0,	yysvec+12,	yyvstop+39,
yycrank+19,	yysvec+8,	yyvstop+41,
yycrank+-10,	yysvec+6,	yyvstop+43,
yycrank+0,	yysvec+19,	yyvstop+45,
yycrank+0,	yysvec+21,	yyvstop+47,
yycrank+263,	0,		yyvstop+49,
yycrank+0,	yysvec+30,	yyvstop+51,
0,	0,	0};
struct yywork *yytop = yycrank+385;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,'"' ,01  ,'$' ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,'.' ,'/' ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,
'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,
'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,
'/' ,'/' ,'/' ,01  ,01  ,01  ,01  ,'/' ,
01  ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,
'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,
'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,'/' ,
'/' ,'/' ,'/' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
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

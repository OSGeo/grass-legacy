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
extern char *G_strdup();
char *lexbuf;
int lexlen;
static char _xg_stringbuf[4096];
main()
{
char *p;

assert(sizeof(int) >= sizeof(char *));

while (p = (char *) yylex())
printf("%-16.16s is \"%s\"\n",    p, yytext);
}

s_lookup() {}
int yynerrs = 0;

#   define token(x)      (int) "x"

#else   ! DEBUG

#    ifndef Boolean
typedef char Boolean;
#    endif
#   include "y.tab.h"
#   include <ctype.h>
#   define     token(x)      x
double atof();
extern char *G_strdup();
char *lexbuf;
int lexlen;
static char _xg_stringbuf[4096];

#endif   DEBUG

#define   END(v)   (v-1 + sizeof v / sizeof v[0])
static int screen();
# define STRING 2
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
{
_xg_digestString();
yylval.cval = _xg_stringbuf;
return token(String);
}
break;
case 2:
            ;
break;
case 3:
{ return screen(); }
break;
case 4:
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
    "DOCUMENTSIZE", token(DOCUMENTSIZE),
    "PAGEWIDTH", token(PAGEWIDTH),
    "PAGEHEIGHT", token(PAGEHEIGHT),
    "UNITS", token(UNITS),
    "NIL", token(NIL),
    "OBJECT", token(OBJECT),
    "SPLINE", token(SPLINE),
    "GEOFRAME", token(GEOFRAME),
    "REGION", token(REGION),
    "RASTER", token(RASTER_MAP),
    "VECTOR", token(VECTOR_MAP),
    "SITE_MAP", token(SITE_MAP),
    "STANDARDSITE", token(STANDARDSITE),
    "PIXMAPSITE", token(PIXMAPSITE),
    "FREEHANDSITE", token(FREEHANDSITE),
    "GRID", token(GRID),
    "BARSCALE", token(BARSCALE),
    "LEGEND", token(LEGEND),
    "SQUARE", token(SQUARE),
    "RECTANGLE", token(RECTANGLE),
    "CIRCLE", token(CIRCLE),
    "ELLIPSE", token(ELLIPSE),
    "POLYLINE", token(POLYLINE),
    "POLYGON", token(POLYGON),
    "OPEN_INTERP_SPLINE", token(OPEN_INTERP_SPLINE),
    "CLOSED_INTERP_SPLINE", token(CLOSED_INTERP_SPLINE),
    "OPEN_APPROX_SPLINE", token(OPEN_APPROX_SPLINE),
    "CLOSED_APPROX_SPLINE", token(CLOSED_APPROX_SPLINE),
    "LABEL", token(LABEL),
};

static int screen()
{
    extern char yytext[];
    struct rwtable *low = rwtable,
    *high = END(rwtable);

    while ( low <= high ) {
    char *tptr, *ptr = G_strdup(yytext);

    tptr = ptr;
    while ( *ptr != NULL ) {
	if (islower(*ptr)) *ptr = toupper(*ptr);
	ptr++;
    }
    if (strcmp(low->rw_name,tptr) == 0)
    return low->rw_yylex;
    low++;
    }
    yylval.cval = G_strdup(yytext);
    return token(String);
}

_xg_digestString()
{
    int done = 0;
    char lastchar = 0;
    int i = 0;

    _xg_stringbuf[i] = 0;

    while ( !done ) {
        char c = input();
        char savechar;

        switch ( c ) {
        case '\\':
            savechar = c;
            c = input();
            if ( c == '\"' ) {
		_xg_stringbuf[i++] = c;
            } else {
		_xg_stringbuf[i++] = savechar;
		_xg_stringbuf[i++] = c;
            }
            break;
        case '\"':
	    done = 1;
            break;
        default:
            _xg_stringbuf[i++] = c;
            break;
        }
        lastchar = c;
    }
    _xg_stringbuf[i] = 0;
}


int yyvstop[] = {
0,

3,
0,

3,
0,

3,
0,

3,
0,

4,
0,

2,
4,
0,

2,
0,

1,
4,
0,

3,
4,
0,

3,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,5,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,6,	1,7,	
0,0,	0,0,	0,0,	0,0,	
6,7,	6,7,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,8,	
2,8,	3,8,	4,8,	6,7,	
0,0,	0,0,	0,0,	0,0,	
1,9,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,10,	
9,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	9,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
9,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	9,0,	
9,0,	9,0,	9,0,	0,0,	
9,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	9,0,	
9,0,	9,0,	9,0,	9,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	0,0,	10,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	10,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	10,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	10,0,	10,0,	
10,0,	10,0,	0,0,	10,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	10,0,	10,0,	
10,0,	10,0,	10,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		yyvstop+1,
yycrank+-2,	yysvec+1,	yyvstop+3,
yycrank+-3,	yysvec+1,	yyvstop+5,
yycrank+-4,	yysvec+1,	yyvstop+7,
yycrank+0,	0,		yyvstop+9,
yycrank+7,	0,		yyvstop+11,
yycrank+0,	yysvec+6,	yyvstop+14,
yycrank+0,	0,		yyvstop+16,
yycrank+-44,	0,		yyvstop+19,
yycrank+-171,	yysvec+9,	yyvstop+22,
0,	0,	0};
struct yywork *yytop = yycrank+298;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,'+' ,01  ,'+' ,'+' ,'+' ,
'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,
'+' ,'+' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,
'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,
'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,
'+' ,'+' ,'+' ,01  ,01  ,01  ,01  ,'+' ,
01  ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,
'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,
'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,'+' ,
'+' ,'+' ,'+' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
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

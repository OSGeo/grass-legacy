%{
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
%}

letter           [a-zA-Z$/]
digit            [0-9]
letter_or_digit  [a-zA-Z0-9/._+-]
white_space      [ \t\n]
blank            [ \t]
other            .

%s STRING

%%



\" {
_xg_digestString();
yylval.cval = _xg_stringbuf;
return token(String);
}

{white_space}+             ; /* squeeze out white space */

{letter_or_digit}* { return screen(); }

{other}                    return token(yytext[0]);
%%

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



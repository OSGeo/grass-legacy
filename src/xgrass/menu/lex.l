%{
#ifdef DEBUG
#   include <assert.h>
#    ifndef Boolean
typedef char Boolean;
#    endif
#   include "y.tab.h"
YYSTYPE yylval;
char *StrDup();
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
char *StrDup();
char *lexbuf;
int lexlen;
static char _xg_stringbuf[4096];

#endif   DEBUG

#define   END(v)   (v-1 + sizeof v / sizeof v[0])
static int screen();
%}

letter           [a-zA-Z$/]
digit            [0-9]
letter_or_digit  [a-zA-Z0-9_./<>-]
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

{letter}{letter_or_digit}* { return screen(); }

{other}                    return token(yytext[0]);
%%

static struct rwtable {
    char *rw_name;
    int rw_yylex;
} rwtable[] = {
    "menu",             token(MenuKey),
    "defaultmenu",      token(DefaultMenuKey),
    "menu",             token(MenuKey),
    "f.quit",           token(F_Quit),
    "f.label",          token(F_Label),
    "f.separator",      token(F_Separator),
    "f.pulldown",       token(F_Pulldown),
    "f.pullright",      token(F_Pullright),
    "f.menu",           token(F_HMenu),
    "f.hmenu",          token(F_HMenu),
    "f.vmenu",          token(F_VMenu),
    "f.dbset",          token(F_DBSet),
    "f.exec",           token(F_Exec),
    "f.exec_c",         token(F_ExecCapture),
    "f.exec_h",         token(F_ExecHist),
    "f.exec_ch",        token(F_ExecCaptureHist),
    "f.xclip",          token(F_Xclip),
    "f.history_toggle", token(F_HistoryToggle),
    "f.history_clear",  token(F_HistoryClear),
    "f.history_edit",   token(F_HistoryEdit),
    "f.history_replay", token(F_HistoryReplay),
};

static int screen()
{
    extern char yytext[];
    struct rwtable *low = rwtable,
    *high = END(rwtable);

    while ( low <= high ) {
    char *tptr, *ptr = StrDup(yytext);

    tptr = ptr;
    while ( *ptr != NULL ) {
	if (isupper(*ptr)) *ptr = tolower(*ptr);
	ptr++;
    }
    if (strcmp(low->rw_name,tptr) == 0)
    return low->rw_yylex;
    low++;
    }
    yylval.cval = StrDup(yytext);
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



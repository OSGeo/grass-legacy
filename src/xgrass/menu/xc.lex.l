%{
#    ifndef Boolean
typedef char Boolean;
#    endif
#   include "xc.y.tab.h"
#   define     token(x)      x
char *_XgStrDup();
double atof();
char *lexbuf;
int lexlen;
static char _xc_stringbuf[4096];

#define   END(v)   (v-1 + sizeof v / sizeof v[0])
static int screen();
%}

letter           [a-zA-Z_$/]
digit            [0-9]
letter_or_digit  [a-zA-Z0-9_./]
white_space      [ \t\n]
blank            [ \t]
other            .

%s STRING

%%

\" {
_xc_digestString();
yylval.cval = _xc_stringbuf;
return token(String);
}

"-"{digit}*"."{digit}+ |
"+"{digit}*"."{digit}+ |
{digit}*"."{digit}+        { /* find real constants */
    yylval.dval = atof(yytext);
    return token(Real);
}

"-"{digit}+ |
"+"{digit}+ |
{digit}+                 { /* find integer constants */
    yylval.ival = atoi(yytext);
    return token(Integer);
}

{white_space}+             ; /* squeeze out white space */

"#"{letter_or_digit}+      { /* find colors (hex) */
    yylval.cval = _XgStrDup(yytext);
    return token(String);
}

{letter}{letter_or_digit}* { return screen(); }

{other}                    return token(yytext[0]);
%%

static struct rwtable {
    char *rw_name;
    int rw_yylex;
} rwtable[] = {
    "program",            token(Program),
    "title",              token(Title),
    "description",        token(Description),
    "help",               token(Help),
    "helpfile",           token(HelpFile),
    "helpwidgetref",      token(HelpWidgetRef),
    "capture",            token(Capture),
    "errorcodes",         token(ErrorCodes),
    "commandstring",      token(CommandString),
    "dialog",             token(Dialog),
    "flag",               token(Flag),
    "parameter",          token(Parameter),
    "key",                token(Key),
    "precludes",          token(Precludes),
    "requires",           token(Requires),
    "optional",           token(Optional),
    "input",              token(Input),
    "multiple",           token(Multiple),
    "selecttype",         token(SelectType),
    "type",               token(Type),
    "database_element",   token(TypeDatabaseElement),
    "character",          token(TypeCharacter),
    "integer",            token(TypeInteger),
    "enum",               token(TypeEnumerate),
    "float",              token(TypeFloat),
    "double",             token(TypeDouble),
    "logical",            token(TypeLogical),
    "filename",           token(TypeFileName),
    "raster",             token(DatabaseElementRaster),
    "dlg_ascii",          token(DatabaseElementAsciiDlg),
    "dlg",                token(DatabaseElementDlg),
    "vector_ascii",       token(DatabaseElementAsciiVector),
    "vector",             token(DatabaseElementVector),
    "sites",              token(DatabaseElementSites),
    "region",             token(DatabaseElementRegion),
    "icon",               token(DatabaseElementIcon),
    "label",              token(DatabaseElementLabel),
    "group",              token(DatabaseElementGroup),
    "subgroup",           token(DatabaseElementSubGroup),
    "old",                token(StatusOld),
    "new",                token(StatusNew),
    "default",            token(Default),
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
    yylval.cval = _XgStrDup(yytext);
    return token(String);
}


_xc_digestString()
{
    int done = 0;
    int backslash = 0;
    int i = 0;

    _xc_stringbuf[i] = 0;

    while ( !done ) {
        char c = input();

        switch ( c ) {
        case '\\':
	    if ( backslash ) {
                _xc_stringbuf[i++] = c;
		backslash = 0;
	    } else {
		backslash = 1;
	    }
            break;
        case '\"':
            if ( backslash ) {
                _xc_stringbuf[i++] = c;
		backslash = 0;
            } else
                done = 1;
            break;
        default:
            _xc_stringbuf[i++] = c;
            break;
        }
    }
    _xc_stringbuf[i] = 0;
}


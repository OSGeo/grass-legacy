%{
#ifdef DEBUG
#   include <assert.h>
#    ifndef Boolean
typedef char Boolean;
#    endif
#   include "grammar.h"
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
#   include "grammar.h"
#   define     token(x)      x
char *SaveString();
double atof();
char *lexbuf;
int lexlen;

#endif   DEBUG

#define   END(v)   (v-1 + sizeof v / sizeof v[0])
static int screen();
%}

letter           [a-zA-Z_$/]
digit            [0-9]
letter_or_digit  [a-zA-Z0-9_./]
white_space      [ \t\n]
blank            [ \t]
other            .
%%



\"([^"\n]|\\["\n])*\"      { /* find quoted stuff */
lexbuf = SaveString(yytext);
lexlen = strlen(lexbuf) - 2;
(void)strncpy(yytext,&lexbuf[1],lexlen);
yytext[lexlen] = '\0';
XtFree(lexbuf);
yylval.cval = yytext;
return token(String);
}

"-"{digit}*"."{digit}+ |
"+"{digit}*"."{digit}+ |
{digit}*"."{digit}+        { /* find real "%age" constants */
yylval.dval = atof(yytext);
return token(Real);
}

"-"{digit}+ |
"+"{digit}+ |
{digit}+                 { /* find "pixel" constants */
yylval.ival = atoi(yytext);
return token(Integer);
}

{white_space}+             ; /* squeeze out white space */

"set"{letter_or_digit}+    { /* find setXXXXX and parse it here (Yuuchh) */
yylval.cval = SaveString(&yytext[3]);
return token(Set);
}

"#"{letter_or_digit}+      { /* find colors (hex) */
yylval.cval = SaveString(yytext);
return token(String);
}

{letter}{letter_or_digit}* { return screen(); }

{other}                    return token(yytext[0]);
%%

static struct rwtable {
    char *rw_name;
    int rw_yylex;
} rwtable[] = {
    "environment",            token(Environment),
    "initialshells",          token(InitialShells),
    "makeshells",             token(MakeShells),
    "staticmenu",             token(StaticMenu),
    "dynamicmenu",            token(DynamicMenu),
    "messageboard",           token(MessageBoard),
    "commandboard",           token(CommandBoard),
    "label",                  token(Label),
    "message",                token(Message),
    "list",                   token(List),
    "button",                 token(PushButton),
    "textentry",              token(TextEntry),
    "table",                  token(Table),
    "fileselect",             token(FileSelect),
    "separator",              token(Separator),
    "slider",                 token(Slider),
    "toggle",                 token(Toggle),
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
    "captureoutput",          token(CaptureOutput),
    "updatefrom",             token(UpdateFrom),
    "store",                  token(Store),
    "getenv",                 token(GetEnv),
    "clear",                  token(Clear),
    "commandarg",             token(CommandArg),
    "set",                    token(Set),
    "alignment",              token(Alignment),
    "listelement",            token(ListElement),
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
    "rowheight",              token(RowHeight),
    "columnwidth",            token(ColumnWidth),
    "titlestring",            token(TitleString),
    "toggletype",             token(ToggleType),
    "separatortype",          token(SeparatorType),
};

static int screen()
{
extern char yytext[];
struct rwtable *low = rwtable,
*high = END(rwtable);

int c;

while ( low <= high ) {
if ((c = strcmp(low->rw_name,yytext)) == 0)
return low->rw_yylex;
low++;
}
yylval.cval = SaveString(yytext);
return token(String);
}


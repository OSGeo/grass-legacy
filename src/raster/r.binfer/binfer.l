/**/
/****/
/*** Filename: binfer.l*/
/****/
/****/
/*** Author: Kurt A. Buehler*/
/*** Date: Fri Feb 10 08:41:42 EST 1989*/
/*** Version: 1.0*/
/**(lex doesn't like my normal comment style...?)**/
/**/

%option lex-compat


%{
int s_lookup(int);
#ifdef DEBUG
#   include <assert.h>
int main()
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

#ifndef sgi
#ifndef sparc
#ifndef linux
#ifndef __FreeBSD__
#ifndef __CYGWIN__
int yylineno = 1;
#endif
#endif
#endif
#endif
#endif

#define   END(v)   (v-1 + sizeof v / sizeof v[0])
static int screen();
%}

letter           [a-zA-Z]
digit            [0-9]
letter_or_digit  [a-zA-Z0-9_.]
white_space      [ \t]
blank            [ \t]
new_line         [\n]
other            .
%%

"#".*\n        ; /* Strip out comments */

[Nn][Oo][Pp][Rr][Oo][Bb][Aa][Bb][Ii][Ll][Ii][Tt][Yy][Mm][Aa][Pp][Ss] {
return token(NO_PROBABILITY_MAPS); }

[Nn][Oo][Cc][Oo][Mm][Bb][Ii][Nn][Ee][Dd][Mm][Aa][Pp]     {
return token(NO_COMBINED_MAP); }

[Cc][Oo][Mm][Bb][Ii][Nn][Ee][Dd][Mm][Aa][Pp]  {
return token(COMBINED_MAP); }

[Aa][Ss][Pp][Ee][Cc][Tt][Cc][Oo][Ll][Oo][Rr][Ss]  {
return token(ASPECT); }

[Gg][Rr][Ee][Yy]  {
return token(GREY); }

[Gg][Rr][Aa][Yy]  {
return token(GREY); }

[Hh][Ii][Ss][Tt][Oo]  {
return token(HISTO); }

[Rr][Aa][Ii][Nn][Bb][Oo][Ww]  {
return token(RAINBOW); }

[Rr][Aa][Mm][Pp]  {
return token(RAMP); }

[Rr][Aa][Nn][Dd][Oo][Mm]  {
return token(RANDOM); }

[Rr][Yy][Gg]  {
return token(REDYELLOWGREEN); }

[Ww][Aa][Vv][Ee]  {
return token(WAVE); }

{letter}{letter_or_digit}* return screen();

\"([^"\n]|\\["\n])*\"      {
s_lookup(token(String));
return token(String);
}

"-"{digit}+   |                  
"-"{digit}*"."{digit}+  |
{digit}+   |                  
{digit}*"."{digit}+        {
s_lookup(token(Constant));
return token(Constant);
}

{white_space}+             ;

{new_line}                 {
yylineno++;
}

{other}                    return token(yytext[0]);
%%

static struct rwtable {
char   *rw_name;
int    rw_yylex;
} rwtable[] = {
{"layer",         token(LAYER)},
{"context",       token(CONTEXT)},
{"subjective",    token(SUBJECTIVE)},
{"inferred",      token(INFERRED)},
{"question",      token(QUESTION)},
{"thru",          token(THRU)}
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


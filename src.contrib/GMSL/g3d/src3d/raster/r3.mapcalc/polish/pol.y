%token NAME STRING INTEGER FLOAT FUNCTION GT GE EQ LT LE AND OR
%token COLOR_GRAY COLOR_RED COLOR_BLUE COLOR_GREEN

%left AND OR
%left GT GE EQ NE LT LE
%left '+' '-'
%left '*' '/' '%'
%right '^'
%left UMINUS
%{
#include <stdlib.h>
#include <string.h>
#include "local_proto.h"

static int nstored = 0;
static char **storage = 0;
%}


%%

stmt       : NAME '=' exp '\n'    { assign($1); return 1; }
           | error '\n'  { return 0; }
	   ;

exp        : '(' exp ')'
	   | exp AND exp               { logical ("&"); }
	   | exp OR exp                { logical ("|"); }
	   | exp GT exp                { compare (">"); }
	   | exp GE exp                { compare (">="); }
	   | exp LT exp                { compare ("<"); }
	   | exp LE exp                { compare ("<="); }
	   | exp EQ exp                { compare ("="); }
	   | exp NE exp                { compare ("!"); }
	   | exp '^' exp               { binary_opcode ("^"); }
	   | exp '*' exp               { binary_opcode ("*"); }
	   | exp '/' exp               { binary_opcode ("/"); }
	   | exp '%' exp               { binary_opcode ("%"); }
	   | exp '+' exp               { binary_opcode ("+"); }
	   | exp '-' exp               { binary_opcode ("-"); }
	   | '-' exp  %prec UMINUS     { unary_opcode ("-"); }
	   | NAME '=' exp              { define_variable($1); }
	   | NAME                      { name ($1); }
	   | STRING                    { mapname ($1,'M',0,0,0); }
	   | map '[' index ']'         { mapname ($1,'M',$3,0,0); }
	   | map '[' index ',' index ']'
				       { mapname ($1,'M',$3,$5,0); }
	   | map '[' index ',' index ',' index ']'
	                               { mapname ($1,'M',$3,$5,$7); } 
	   | map                       { mapname ($1,'M',0,0,0); }
	   | mapmod map                { mapname ($2,$1,0,0,0); }
	   | mapmod map '[' index ']'  { mapname ($2,$1,$4,0,0); }
	   | mapmod map '[' index ',' index ']'
				       { mapname ($2,$1,$4,$6,0); }
           | mapmod map '[' index ',' index ',' index ']'
	                               { mapname ($2,$1,$4,$6,$8); }
	   | FUNCTION '(' ')'          { function ($1); }
	   | FUNCTION '(' exp_list ')' { function ($1); }
	   | INTEGER                   { integer ($1); }
	   | FLOAT                     { floating_point ($1); }
	   ;

map        : NAME
	   | NAME '@' NAME
		          { char buf[1024];
			    sprintf (buf, "%s@%s", storage[$1], storage[$3]);
			    $$ = store(buf);
			  }
	   ;

mapmod     : '@'          { $$ = '@'; }
	   | COLOR_GRAY   { $$ = '#'; }
	   | COLOR_RED    { $$ = 'r'; }
	   | COLOR_GREEN  { $$ = 'g'; }
	   | COLOR_BLUE   { $$ = 'b'; }
	   ;

exp_list   : exp                   { another_arg(); }
	   | exp_list ',' exp      { another_arg(); }
	   ;

index      : INTEGER       { $$ = $1; }
           | '-' INTEGER   { $$ = -$2; }
	   ;
%%
#include "lex.yy.c"

int yywrap()
{
    return 1;
}

int yyerror(char *s)
{
    printf ("??\n");
    return 0;
}

int store (char *s)
{
    int i;
    for (i = 0; i < nstored; i++)
	if (strcmp (s, storage[i]) == 0)
	    return i;

    if(nstored++)
	storage = (char **) realloc (storage, nstored * sizeof (*storage));
    else
	storage = (char **) malloc (nstored * sizeof (*storage));

    storage[nstored-1] = malloc (strlen(s)+1);
    strcpy (storage[nstored-1],s);
    return nstored - 1;
}

static int function_level = 0;
static int nfuncs = 0;
static int *nargs = 0;

int begin_function(void)
{
    function_level++;
    if (function_level > nfuncs)
    {
	if (nfuncs)
	    nargs = (int *) realloc (nargs, function_level * sizeof (int));
	else
	    nargs = (int *) malloc (function_level * sizeof (int));
	nfuncs = function_level;
    }
    nargs[function_level-1] = 0;
    return 0;
}
int another_arg (void)
{
    nargs[function_level-1]++;
    return 0;
}
int function (int n)
{
    function_level--;
    printf ("F%s %d\n", storage[n], nargs[function_level]);
    return 0;
}

int name(int n)
{
    int i;

    if (i = find_variable(n))
	printf ("v%d\n", i-1);
    else
	mapname (n,'M',0,0,0);
    return 0;
}

int mapname (int n,char code, int row,int col, int depth)
{
    printf ("M%c %d %d %d %s\n", code, row, col, depth,  storage[n]);
    return 0;
}

int integer (int n)
{
    printf ("I%d\n",n);
    return 0;
}
int floating_point (int n)
{
    printf ("D%s\n",storage[n]);
    return 0;
}
int unary_opcode (char *s)
{
    printf ("1%s\n",s);
    return 0;
}
int binary_opcode (char *s)
{
    printf ("2%s\n",s);
    return 0;
}
int compare (char *s)
{
    printf ("C%s\n",s);
    return 0;
}
int logical (char *s)
{
    printf ("L%s\n",s);
    return 0;
}

int assign (int n)
{
    printf ("=%s\n", storage[n]);
    return 0;
}

static int *vars ;
static int nvars = 0;

int find_variable (int n)
{
    int i;

    for (i = 0; i < nvars; i++)
	if (vars[i] == n)
	    return(i+1);
    return 0;
}

int define_variable (int n)
{
    int i;

    if (i = find_variable(n))
    {
	printf ("V%d\n",i-1);
	return i;
    }
    if (nvars++)
	vars = (int *) realloc (vars, nvars * sizeof(int));
    else
	vars = (int *) malloc (sizeof(int));
    
    vars[nvars-1] = n;
    printf ("V%d\n",nvars-1);
    return nvars;
}



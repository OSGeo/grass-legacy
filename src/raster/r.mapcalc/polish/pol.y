%token NAME STRING INTEGER FLOAT FUNCTION GT GE EQ LT LE AND OR
%token COLOR_GRAY COLOR_RED COLOR_BLUE COLOR_GREEN

%left AND OR
%left GT GE EQ NE LT LE
%left '+' '-'
%left '*' '/' '%'
%left UMINUS

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
	   | exp '*' exp               { binary_opcode ("*"); }
	   | exp '/' exp               { binary_opcode ("/"); }
	   | exp '%' exp               { binary_opcode ("%"); }
	   | exp '+' exp               { binary_opcode ("+"); }
	   | exp '-' exp               { binary_opcode ("-"); }
	   | '-' exp  %prec UMINUS     { unary_opcode ("-"); }
	   | NAME '=' exp              { define_variable($1); }
	   | NAME                      { name ($1); }
	   | STRING                    { mapname ($1,'M',0,0); }
	   | map '[' index ']'         { mapname ($1,'M',$3,0); }
	   | map '[' index ',' index ']'
				       { mapname ($1,'M',$3,$5); }
	   | map                       { mapname ($1,'M',0,0); }
	   | mapmod map                { mapname ($2,$1,0,0); }
	   | mapmod map '[' index ']'  { mapname ($2,$1,$4,0); }
	   | mapmod map '[' index ',' index ']'
				       { mapname ($2,$1,$4,$6); }
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

yywrap()
{
    return 1;
}
yyerror(s) char *s;
{
    printf ("??\n");
}

static int nstored = 0;
static char **storage = 0;
char *malloc(), *realloc();

store (s) char *s;
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

begin_function()
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
}
another_arg ()
{
    nargs[function_level-1]++;
}
function (n)
{
    function_level--;
    printf ("F%s %d\n", storage[n], nargs[function_level]);
}

name(n)
{
    int i;

    if (i = find_variable(n))
	printf ("v%d\n", i-1);
    else
	mapname (n,'M',0,0);
}

mapname (n, code, row, col)
    char code;
{
    printf ("M%c %d %d %s\n", code, row, col, storage[n]);
}
integer (n)
{
    printf ("I%d\n",n);
}
floating_point (n)
{
    printf ("D%s\n",storage[n]);
}
unary_opcode (s) char *s;
{
    printf ("1%s\n",s);
}
binary_opcode (s) char *s;
{
    printf ("2%s\n",s);
}
compare (s) char *s;
{
    printf ("C%s\n",s);
}
logical (s) char *s;
{
    printf ("L%s\n",s);
}

assign (n)
{
    printf ("=%s\n", storage[n]);
}

static int *vars ;
static int nvars = 0;

find_variable (n)
{
    int i;

    for (i = 0; i < nvars; i++)
	if (vars[i] == n)
	    return(i+1);
    return 0;
}

define_variable (n)
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


#include "lex.yy.c"

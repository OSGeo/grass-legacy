/* compile --- compile the rules into the internal form */

# include <stdio.h>
# include <ctype.h>
# include "infer.h"

int Curline;
int State;

compile (fp)
register FILE *fp;
{
    register int key;
    char line[STRSIZE];
    STR *savestr ();
    struct Cell_stats *make_cats();

    Curline = 0;
    State = ANT;
    rulealloc ();
    while (fgets (line, sizeof (line), fp)) {
	Curline++;
	squeeze (line, '\n');
	/*stripbl (line);*/
	G_squeeze(line);
	if (verbose)
	    printf ("%4d    %s\n", Curline, line);
	key = getkey (line);
	if (key == NONE)
	    fprintf (stderr, "%s: no keyword found on line %d\n",
	      progname, Curline);
	else if (key == COMMENT)
	    ;
	else
	    push (key, savestr (key, line));
    }
}

/* newstate --- if switching from CON to ANT, start a new rule */

newstate (new)
register int new;
{
    if (new != ANT && new != CON) {     /* paranoia */
	fprintf (stderr, "%s: bad new val: %d\n", new);
	exit (1);
    }

    if (State != new) {
	if (State == CON)
	    rulealloc ();
	State = new;
    }
}

/* push --- add an element to this rule */

push (type, ptr)
register int type;
register STR *ptr;
{
    register ELEMENT_T *e, *last;

    if (ptr == 0)   /* keyword only, ignore */
	return;
    e = (ELEMENT_T *) emalloc ((unsigned) sizeof (ELEMENT_T));
    e->str = ptr;
    e->type = type;
    e->next = 0;
    if (State == CON) {
	if (Rule[nrules-1]->con == 0) /* first element */
	    Rule[nrules-1]->con = e;
	else                          /* place on end */
	    for (last = Rule[nrules-1]->con; last; last = last->next)
		if (last->next == 0) {
		    last->next = e;
		    break;
		}
    }
    else {
	if (Rule[nrules-1]->ant == 0)
	    Rule[nrules-1]->ant = e;
	else
	    for (last = Rule[nrules-1]->ant; last; last = last->next)
		if (last->next == 0) {
		    last->next = e;
		    break;
		}
    }
}

/* savestr --- skip 1st word, save rest of line in str buffer */

STR *
savestr (key, line)
int key ;
char *line;
{
    register char *s;
    register STR *sp;
    MAPS *map ;
    char name[64] ;
    char *nme ;
    int i ;
    int hypval ;
    short mapnum ;
    struct Cell_stats *cats;

    cats = NULL;
    for (s = line; *s; s++)     /* skip to 1st space */
	if (*s == ' ')
	    break;

    while (isspace (*s))        /* skip to 1st non-space */
	s++;

    if (*s == 0)
    {
	fprintf (stderr, "%s: line %d has nothing but a keyword\n",
	  progname, Curline);
	return (0);
    }

    if ( (key & MAPOP) && (key & HYP) )
    {
	sscanf(s, "%d", &hypval) ;
    }
    else if (key & MAPOP)
    {
	nme = name ;
	while (! isspace(*s))
		*nme++ = *s++ ;
	*nme = 0 ;

	mapnum = -1 ;
	i = 0 ;
	for(map = MAP; map; map = map->next, i++)
	    if (equal(map->p, name))
		break ;

	if (!map)
	{
	    map = (MAPS *) emalloc ((unsigned) sizeof (MAPS) ) ;
	    map->p = emalloc ((unsigned) strlen(name) + 1) ;
	    strcpy(map->p, name) ;
	    map->val = i ;
	    mapnum = i ;
	    map->next = MAP;      /* place at head */
	    MAP = map;
	}

	cats = make_cats(s) ;

	for (sp = SP; sp; sp = sp->next)    /* is string already present? */
	    if (equal (sp->p, name))
		if (! catmatch (sp->cats, cats))
		{
			freecats(cats);
			return (sp);
		}
    }
    else
    {
	for (sp = SP; sp; sp = sp->next)    /* is string already present? */
	    if (equal (sp->p, s))
		    return (sp);
    }

    sp = (STR *) emalloc ((unsigned) sizeof (STR)); /* new string */
    if ( (key & MAPOP) && (key & HYP) )
    {
	sp->hypval = hypval ;
    }
    else if (key & MAPOP)
    {
	sp->p = emalloc ((unsigned) strlen (name)+1);
	strcpy (sp->p, name);
	sp->cats = cats ;
	sp->map = map ;
    }
    else
    {
	sp->p = emalloc ((unsigned) strlen (s)+1);
	strcpy (sp->p, s);
    }
    sp->val = UNKNOWN;
    sp->next = SP;      /* place at head */
    SP = sp;
    return (sp);
}

/* getkey --- determine the keyword on this line */

getkey (line)
char *line;
{
    register int i;
    register char *s, *p;
    char word[20];
    static struct {
	char    *name;
	short   type;
	short   newstate;
    } keytab[] = {              /* these are sorted based on frequency */
	"THEN", ISTRING, CON,    /* of use in 3 files I had access to */
	"AND", ISTRING, ANT,     /* change at will */
	"IF", ISTRING, ANT,
	"ANDMAP", MAPOP, ANT,
	"ANDNOT", ISTRING|NOT, ANT,
	"THENHYP", ISTRING|HYP, CON,
	"IFMAP", MAPOP, ANT,
	"ANDIF", ISTRING, ANT,
	"IFNOT", ISTRING|NOT, ANT,
	"THENMAPHYP", MAPOP|HYP, CON,
/*	"THENMAP", MAPOP, CON,           */
	"ANDIFMAP", MAPOP, ANT,
	"ANDNOTMAP", MAPOP|NOT, ANT,
	"IFNOTMAP", MAPOP|NOT, ANT,
	"ANDTHEN", ISTRING, CON,
	"ANDTHENHYP", ISTRING|HYP, CON,
/*	"ANDTHENMAP", MAPOP, CON,        */
/*	"ANDTHENMAPHYP", MAPOP|HYP, CON, */
	0,  0, 0
    };

    if (line[0] == COMMENT_CHAR)
	return (COMMENT);

    for (p = word, s = line; *s; s++) /* copy chars into word */
	if (isupper (*s))
	    *p++ = *s;
	else
	    break;
    *p = 0;
    
    for (i = 0; keytab[i].name; i++)    /* look for match */
	if (equal (word, keytab[i].name)) {
	    newstate (keytab[i].newstate);
	    return (keytab[i].type);
	}

    return (NONE);
}

/* squeeze --- squeeze all c from s */

squeeze (s, c)
register char *s;
register char c;
{
    register char *p;

    for (p = s; *s; s++)
	if (*s != c)
	    *p++ = *s;
    *p = '\0';
}

/* stripbl --- remove trailing blanks from s */

stripbl (s)
register char *s;
{
    register char *p;

    for (p = s; *s; s++)
	if (*s != ' ')
	    p = s+1;

    *p = 0;
}

/* rulealloc --- allocate a new rule, and advance the pointer */

rulealloc ()
{
    if (nrules >= MAXRULE) {
	fprintf (stderr, "%s: too many rules!\n", progname);
	exit (1);
    }

    Rule[nrules] = (RULE_T *) emalloc ((unsigned) sizeof (RULE_T));
    Rule[nrules]->ant = 0;
    Rule[nrules]->con = 0;
    Rule[nrules]->vfy = 0;
    nrules++;
}

/* emalloc --- call malloc and check return */

char *
emalloc (n)
unsigned n;
{
    char *p, *malloc ();

    if ((p = malloc (n)) == NULL) {
	fprintf (stderr, "%s: Out of memory!\n", progname);
	exit (1);
    }
    return (p);
}

struct Cell_stats *
make_cats(s)
    char *s ;
{
    int first, last ;
    struct Cell_stats *cats;
    char word[200];

    cats = (struct Cell_stats *) emalloc ((unsigned) sizeof (*cats));

    G_init_cell_stats (cats);

    while (sscanf (s, "%s", word) == 1)
    {
	while (*s == ' ' || *s == '\t' || *s == '\n')
		s++;
	while (*s != 0 && *s != ' ' && *s != '\t' && *s != '\n')
		s++;
	switch (sscanf (word, "%d-%d", &first, &last))
	{
	case 1: last = first; /* FALLTHROUGH */
	case 2: 
		mark_cats(cats, first, last) ;
		break;
	}
    }
    return cats;
}

mark_cats(cats, first, last)
    struct Cell_stats *cats;
    int first ;
    int last ;
{
    CELL z;
    int sw ;

printf ("mark_cats(%d-%d)\n", first, last);
    if (first > last)
    {
	sw = first ;
	first = last ;
	last = sw ;
    }
    while (first <= last)
    {
	z = first++;
	G_update_cell_stats (&z, 1, cats);
    }
}

freecats(cats)
    struct Cell_stats *cats;
{
    G_free_cell_stats (cats);
    free (cats);
}

catmatch (cat1, cat2)
    struct Cell_stats *cat1, *cat2;
{
    int ok1, ok2;
    CELL z1,z2;
    long count;

    G_rewind_cell_stats (cat1);
    G_rewind_cell_stats (cat2);
    for(;;)
    {
	ok1 = G_next_cell_stat (&z1, &count, cat1);
	ok2 = G_next_cell_stat (&z2, &count, cat2);
	if (!ok1 && !ok2) /* ran out of stats at the same time */
	    return 0;

	if (!ok1 || !ok2) /* one ran out */
	    return -1;
	
	if (z1 != z2)
	    return -1;
    }
}

/* equal - compare strings */

equal (a, b)
    char *a, *b;
{
    return (a && b && (strcmp (a,b) == 0));
}

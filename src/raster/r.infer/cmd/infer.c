/* infer --- inference engine */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include "infer.h"

# define TRUTHVAL(E)    ((E->type & NOT) ? (E->str->val == TRUE) ? FALSE : TRUE : (E->str->val))

char *progname;
RULE_T *Rule[MAXRULE];
int nrules = 0;
RULE_T *why[MAXWHY];
int nwhy = 0;
STR *SP ;
MAPS *MAP = 0;
int verbose = 0 ;
static int test = 0 ;

int 
main (int argc, char **argv)
{
	FILE *fp;
	struct GModule *module;
	struct Flag *flag1 ;
	struct Flag *flag2 ;
	struct Option *opt1 ;

	SP = NULL ;

	module = G_define_module();
	module->description =
		"Outputs a raster map layer whose category values "
		"represent the application of user-specified criteria "
		"(rules statements) to other raster map layers' category values.";

	/* Define the different options */

	opt1 = G_define_option() ;
	opt1->key        = "file";
	opt1->type       = TYPE_STRING;
	opt1->required   = YES;
	opt1->description= "File containing inference instructions" ;

	/* Define the different flags */

	flag1 = G_define_flag() ;
	flag1->key         = 'v' ;
	flag1->description = "verbose mode" ;

	flag2 = G_define_flag() ;
	flag2->key         = 't' ;
	flag2->description = "test" ;
	G_gisinit(argv[0]);

	if (G_parser(argc, argv))
		exit(-1);

	verbose = flag1->answer;
	test    = flag2->answer;

	if (test)
	{
		askmap = askmap_person ;
		do_infer = test_infer ;
	}
	else
	{
		askmap = askmap_map ;
		do_infer = run_through_maps ;
		initialize_map_categories();
	}

	if ((fp = fopen (opt1->answer, "r")) == NULL)
	{
		fprintf (stderr, "%s: can't open %s (%s)\n", progname, opt1->answer,
		    strerror(errno));
		exit (1);
	}

	fprintf(stderr,"Compiling input rules from file [%s]\n", opt1->answer) ;
	compile (fp);
	fclose (fp);

	do_infer() ;

	return 0;
}

/* infer --- inference engine */

int 
infer (void)
{
	register int i;
	int proved;

	for (i = 0; i < nrules; i++)       /* verify each CON */
	{
		if (Rule[i]->con == 0)
		{
			fprintf (stderr, "%s: RULE has no THENs:\n", progname);
			prrule (Rule[i], stderr);
			exit (1);
		}
		if (TRUTHVAL (Rule[i]->con) == TRUE)
			continue;

		if (verify (Rule[i]) == TRUE)
		{
			register ELEMENT_T *e;
			for (e = Rule[i]->con; e; e = e->next)
			{
				if (e->type & MAPOP)
				{
					return(e->str->hypval) ;
					/*
		    if (e->str->val == TRUE)
			    continue;
		    if (run (e) == TRUE) 
		    {
			    e->str->val = TRUE;
			    if (e->type & HYP) 
			    {
			        return (0);
			    }
		    }
		    else 
		    {
			    e->str->val = FALSE;
		    }
	    */
				}
				else 
				{
					e->str->val = TRUE;
					proved = TRUE;
					/*
		    fprintf (stdout,"I infer that: %s\n", e->str->p);
		*/
					if (e->type & HYP)
					{
						return (0);
					}
				}
			}
		}
	}
	/*
    if (proved == FALSE) 
    {
	    fprintf (stdout,"I can't prove anything!!!\n");
	    return (1);
    }
*/
	return (0);
}

/* verify --- verify a CON.  May be called recursivly */

int 
verify (register RULE_T *rule)
{
	register ELEMENT_T *e;
	register int i;

	/*
    pushwhy (rule);
*/
	if (rule->con->str->val == TRUE)
	{
		/*
	popwhy ();
*/
		return (TRUE);
	}

	if (verbose)
		prrule (rule, stdout);

	if (rule->ant == 0)
	{
		fprintf (stderr, "%s: RULE has no IFs:\n", progname);
		prrule (rule, stderr);
		/*
	popwhy ();
*/
		return (TRUE);
	}

	for (e = rule->ant; e; e = e->next) {   /* for each ANT */
		for (i = 0; i < nrules; i++)
			if (e->str == Rule[i]->con->str)    /* this ANT is a CON */
				break;
		if (i == nrules) {      /* NOT a CON */
			if (prove (e) == TRUE)
				continue;
			else 
			{
				/*
		            popwhy ();
*/
				return (FALSE);
			}
		}
		else 
		{
			int anytrue = 0;
			for (i = 0; i < nrules; i++)
			{
				if (e->str == Rule[i]->con->str)
				{  /* match */
					int ret;
					if (Rule[i]->vfy == 1)
					{
						fprintf (stderr, "%s: Circular logic in Rules! Rules are:\n", progname);
						prcirc ();
						exit (1);
					}
					Rule[i]->vfy = 1;
					ret = verify (Rule[i]);
					Rule[i]->vfy = 0;
					if (ret == TRUE)
					{
						register ELEMENT_T *f;
						anytrue++;
						for (f = Rule[i]->con; f; f = f->next)
						{
							if (f->str->val == UNKNOWN)
							{
								/*
				                    fprintf (stdout,"I infer that: %s\n", f->str->p);
			*/
								f->str->val = TRUE;
							}
						}
						if (prove (e) == TRUE)
							break;
						else 
						{
							/*
			                      popwhy ();
		                        */
							return (FALSE);
						}
					}
				}
			}
			if (!anytrue)
				if (e->type & NOT)
					continue;
				else 
				{
					/*
		            popwhy ();
                    */
					return (FALSE);
				}
			else if (e->type & NOT)
			{
				/*
		            popwhy ();
                    */
				return (FALSE);
			}
			else
				continue;
		}
	}               /* don't pop the why stack if TRUE */
	return (TRUE);
}

/* prove --- prove this ANT (may be already proven) */

int 
prove (ELEMENT_T *e)
{
	if (e->str->val != UNKNOWN)
		return (TRUTHVAL (e));
	if (e->type & MAPOP)
		return (askmap (e));
	else
	{
		fprintf (stdout,"\nError in rules:\n") ;
		fprintf (stdout,"  An IF, AND, ANDIF, IFNOT, or ANDNOT is testing the\n") ;
		fprintf (stdout,"  string:  %s\n", e->str->p ) ;
		fprintf (stdout,"  This string is not provided by any THEN hypothesis.\n") ;
		fprintf (stdout,"  Often caused by using a statement condition (e.g. IF soils 1)\n") ;
		fprintf (stdout,"  where a map condition (e.g. IFMAP soils 1) is needed.\n") ;
		exit(1) ;
	}
}

/* askmap --- get truth from map */

int 
askmap_map (register ELEMENT_T *e)
{
	register int value;
	long count;

	value = UNKNOWN;
	if ( G_find_cell_stat (*e->str->map->cptr, &count, e->str->cats) )
		value = TRUE ;
	else
		value = FALSE ;
	e->str->val = value;
	return (TRUTHVAL (e));
}

int 
askmap_person (register ELEMENT_T *e)
{
	char line[80];
	register int value;
	CELL i ;
	long count;

	value = UNKNOWN;
	for (;;)
	{
		fprintf (stdout,"Is the following statement true?\n");
		fprintf (stdout,"%s ", e->str->p);
		G_rewind_cell_stats (e->str->cats);
		while (G_next_cell_stat (&i, &count, e->str->cats))
			fprintf (stdout,"%ld ", (long)i) ;
		fprintf (stdout,": ") ;
		if (fgets (line, sizeof (line), stdin) == NULL)
			*line = 'q';
		if (isupper (*line))
			*line = tolower (*line);
		switch (*line)
		{
		case 'y':
		case 't':
			value = TRUE;
			break;
		case 'n':
		case 'f':
			value = FALSE;
			break;
		case 'q':
			fprintf (stdout,"OK, Bye!\n");
			exit (0);
			break;
		case 'w':
			showwhy ();
			continue;
		}
		if (value != UNKNOWN)
			break;
		fprintf (stdout,"How about typing t/f/y/n?\n");
	}
	e->str->val = value;
	return (TRUTHVAL (e));
}

/* prrule --- print this rule in a readable form */

int 
prrule (register RULE_T *rule, FILE *fp)
{
	register ELEMENT_T *e;
	char *prval (), *prtype (), *prstr ();

	for (e = rule->ant; e; e = e->next)
		fprintf (fp, "%s %s (%s)\n", prtype (e->type, "IF"), prstr(e),
		    prval (e));
	for (e = rule->con; e; e = e->next)
		fprintf (fp, "%s %s (%s)\n", prtype (e->type, "THEN"), prstr(e), 
		    prval (e));
	fprintf (fp, "\n");

	return 0;
}

char *
prstr (register ELEMENT_T *e)
{
	static char buff[128] ;
	char buf[20] ;
	int num ;
	CELL i ;
	long count;

	if ( (e->type & MAPOP) && (e->type & HYP) )
	{
		sprintf(buff, "New Category Val: %d", e->str->hypval) ;
	}
	else if (e->type & MAPOP)
	{
		num = 0 ;
		strcpy(buff, e->str->p);
		G_rewind_cell_stats (e->str->cats);
		while(G_next_cell_stat (&i, &count, e->str->cats))
		{
			if (++num > 20)
				break ;
			sprintf(buf,", %ld", (long)i) ;
			strcat(buff, buf) ;
		}
	}
	else
		sprintf(buff, "%s", e->str->p) ;

	return(buff) ;
}

/* prval --- return the char representation of this value */

char *
prval (register ELEMENT_T *e)
{
	if (e->str->val == UNKNOWN)
		return ("UNKNOWN");
	else if (e->str->val == TRUE || e->str->val == FALSE)
	{
		if (TRUTHVAL (e) == TRUE)
			return ("TRUE");
		else
			return ("FALSE");
	}
	else
		return ("BADVAL");
}

/* prtype --- return the char representation of this type */

char *
prtype (int type, register char *word)
{
	static char str_type[20];

	strcpy (str_type, word);
	if (type & NOT)
		strcat (str_type, "NOT");
	if (type & MAPOP)
		strcat (str_type, "MAP");
	if (type & HYP)
		strcat (str_type, "HYP");
	return (str_type);
}

/* pushwhy --- push this rule onto the "why" stack */

int 
pushwhy (register RULE_T *r)
{
	if (nwhy >= MAXWHY)
	{
		fprintf (stderr, "%s: blew why stack!\n", progname);
		exit (1);
	}
	why[nwhy++] = r;

	return 0;
}

/* popwhy --- pop a value off of the "why" stack */

int 
popwhy (void)
{
	if (--nwhy < 0)
	{
		fprintf (stderr, "%s: why stack underflow!\n", progname);
		exit (1);
	}

	return 0;
}

/* showwhy --- print the details of the "why" stack */

int 
showwhy (void)
{
	register int i;

	for (i = 0; i < nwhy; i++)
		prrule (why[i], stdout);

	return 0;
}

/* prcirc --- print the details of the circular loop */

int 
prcirc (void)
{
	register int i;

	for (i = 0; i < nrules; i++)
		if (Rule[i]->vfy == 1)
			prrule (Rule[i], stderr);

	return 0;
}

int 
clear_truths (void)
{
	register STR *sp;

	for (sp = SP; sp; sp = sp->next)
		sp->val = UNKNOWN;

	nwhy = 0 ;

	return 0;
}

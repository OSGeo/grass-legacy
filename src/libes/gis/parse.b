
/* G_parse_command(argc, argv, variables, stash_away)
 *    int argc		      number of arguments
 *    char *argv[]		  argument array
 *    struct Command_keys variables[]  alias (string) and position values
 *		Array must end with  variables[x].alias == NULL
 *    int (*stash_away)()	   routine to call with parsed info
 *
 * This routine parses command line information of the forms:
 *    command opt1 opt2 opt3 opt4
 *	 arguments are options in correct positions
 *    command opt1 - - opt4
 *	 arguments are options in correct positions, where minuses (-)
 *	 are interpreted as "accept the default for this position"
 *    command arg2=opt2 arg4=opt4 arg3=opt3 arg1=opt1
 *	 arguments are in mixed order, but the correct position is
 *	 ascertained by looking for the "arg" string in the variables
 *	 structure.  This structure contains the "correct" position
 *	 for the option.
 *    command opt1,opt1b,opt1c
 *    command arg1=opt1a,opt1b,opt1c
 *    command arg1=opt1a  arg1=opt1b  arg1=opt1c
 *	 if the program allows: any 1 argument may have several options
 *	 they can be separated by commas or using multiple occurances of 
 *	    the (=) format
 *    command opt1 - arg4=opt4
 *	 mixed form of the above commands
 *    command opt1a,opt1b,opt1c - arg4=opt4a,opt4b,opt4c arg4=opt4d
 *	 mixed form of the above commands
 *
 * Once a position is determined, either by actual position or by deduction,
 *    the position and option are sent to the specified routine (stash_away).
 *
 * Review  gis/src/G/programs/text/{*}.[ch]  for an example.

 * If user enters 'help' in position 1, G_parse_command() will call a default
 *    USAGE display routine [G_parse_command_usage ()], which is also available
 *    to the programmer.  This routine may be overridden with 
 *    G_set_parse_command_usage ().


 * G_parse_command_usage (progname, variables, long_form)
 *    char *progname;			 argv[0] 
 *    struct Command_keys variables[];
 *    int long_form;			 use long format if USAGE_LONG
 *					 use short format if USAGE_SHORT
 *   The Short form gives just the syntax for the command while the long
 *	form also includes a list of keyword synanymns  (sp?)
 *    
 *    Will display the default USAGE message.


 * G_set_parse_command_usage (help_rout)
 *    int (*help_rout)();
 *
 *    Will override the default USAGE message and call help_rout ()
 *    with the same arguments as G_parse_command_usage ()
 *    G_parse_command_usage is then available to your help routine if desired.
 *
 * The programmer is expected to create synonyms the Commandkeys array in a 
 *    most significant to least significant order.  Order between 2 different
 *    positions is not important.
 *    
 */

#include "gis.h"

/*	this struct is defined in "gis.h"
struct Command_keys
{
    char *alias;
    int position;
};
*/


char *G_calloc();
static int (*HELP_SUBR)() = NULL;

static char *have_equal();
static int end_comma ();
static int get_alias ();
int G_parse_command_usage ();

G_parse_command(argc, argv, variables, stash_away)
    int argc;
    char *argv[];
    struct Command_keys variables[];
    int (*stash_away)();
{
    char **tokens, **p;
    char argbuf[512], *argp;
    char var[30];
    int i;
    int j;
    int in_comma;
    int using_named;
    int position;

    using_named = 0;
    in_comma = 0;
    position = 0;
    argbuf[0] = '\0';


    if (argc == 1 || strcmp(argv[1], "help") == 0)
    {
	if (HELP_SUBR == NULL || HELP_SUBR == G_parse_command_usage) 
	    G_parse_command_usage (argv[0], variables, USAGE_LONG);
	else
	    (*HELP_SUBR) (argv[0], variables, USAGE_LONG);
	return(1);
    }

    for(i=1 ; i < argc ; i++)
    {
	if (!in_comma)
	if (strcmp(argv[i], "-") == 0)
	{
	    position++;
	    continue;
	}

	strcat (argbuf, argv[i]);

	if (end_comma (argv[i]))	/* if ends w/ comma, then get next arg*/
	{
	    in_comma = 1;
	    continue;
	}
	else in_comma = 0;

	position++;

	if (argp = have_equal (argbuf))
	{
	    get_alias (argbuf, var);

	    using_named = 1;
	    position = 0;
	    for(j=0; variables[j].alias != NULL; j++)
	    {
		if (!strcmp(var, variables[j].alias))
		{
		    position = variables[j].position;
		    break;
		}
	    }
	    if (! position)
		return(-2);
	}
	else
	{
	    argp = argbuf;
	    if (using_named)
		return(-3);
	}

	tokens = G_tokenize (argp, ",");

	for (p = tokens ; *p ; p++)
	{
	    if ((*stash_away) (position, *p) != 0)
		return(-4);
	}

	G_free_tokens (tokens);
	argbuf[0] = '\0';		/* re-initialize buffer */
    }
    return(0);
}

/* if find '='  return pointer to next char  else NULL */
static char *
have_equal (str)
    char *str;
{
    for(; *str; str++)
	if (*str == '=')
	    return(str+1);
    return(NULL);
}

G_set_parse_command_usage (subr)
    int (*subr)();
{
    HELP_SUBR = subr;
}


#define KEYLEN 30

G_parse_command_usage (progname, keys, Long)
    char *progname;
    struct Command_keys keys[];
    int Long;			/* use long form if true */
{
    register int i, j, tmp;
    int maxkey;
    int longest;
    int position;
    char (*aliases)[][KEYLEN];

    /*  Get number of keys we are expecting */
    maxkey = 0;
    longest = 0;
    for (i = 0 ; keys[i].alias != NULL ; i++)
    {
	if (keys[i].position > maxkey)
	    maxkey = keys[i].position;
	if ((tmp = strlen (keys[i].alias)) > longest)
	    longest = tmp;
    }


    aliases = (char (*)[][KEYLEN]) G_calloc (maxkey+2, KEYLEN); /* create new array */

    /* get a list of primary aliases for Usage statement */
    for (j = 0 ; keys[j].alias != NULL ; j++)	/* go thru entire array */
	if ((*aliases)[keys[j].position][0] == '\0')  /* if not already found */
	{
	    strcpy((*aliases)[keys[j].position], keys[j].alias);/* save it */
	}

    fprintf (stderr, "\nUsage:  %s ", progname);
    for (i = 1 ; i <= maxkey ; i++) 
	if (!strlen ((*aliases)[i]))
	{
	    G_fatal_error ("\nInternal ERROR!  Bad Command_keys data.\n");
	}
	else
	    fprintf (stderr, "[%s] ", (*aliases)[i]);

    fprintf (stderr, "\n\t\t\t\t\t\tor\n");
    fprintf (stderr, "        %s ", progname);
    for (i = 1 ; i <= maxkey ; i++) 
	if (!strlen ((*aliases)[i]))
	{
	    G_fatal_error ("\nInternal ERROR!  Bad Command_keys data.\n");
	}
	else
	    fprintf (stderr, "[%s=opt] ", (*aliases)[i]);
   fprintf (stderr, "\n");

   if (!Long) return (0);

   fprintf (stderr, "\n    Using '-' for place holder yields default\n");
   fprintf (stderr, "\n\n    The following keyword spellings are allowed:\n\n");

    for (position = 1 ; position <= maxkey ; position++)
    {
	fprintf (stderr, "    	");
	for (j = 0 ; keys[j].alias != NULL ; j++)
	    if (keys[j].position == position)
	    {
		fprintf (stderr, "%-*s", longest+3, keys[j].alias);
#if FALSE
		if (strlen (keys[j].alias) > 7)
		    fprintf (stderr, "	");	/* 1 Tab stop */
		else
		    fprintf (stderr, "		");	/* 2 Tab stops */
#endif
	    }
	fprintf (stderr, "\n");
    }
    fprintf (stderr, "\n\n");
}

static int
end_comma (str)
    char *str;
{
    if (str[strlen(str)-1] == ',')
	return (1);
    return (0);
}

static 
get_alias (from, to)
    char *from, *to;
{
    while (*from && ((*to = *from) != '='))
	from++, to++;
    *to = '\0';
}

/***************************************************************************
 * Routines used to assist in command line parsing.  
 ***************************************************************************
 * G_define_flag()
 *
 * Returns a pointer to a flag structure.
 * Flags are always represented by single letters.  A user "turns them on"
 * at the command line using a minus sign followed by the character
 * representing the flag.
 *
 ***************************************************************************
 * G_define_option()
 *
 * Returns a pointer to a flag structure.
 * Options are provided by user on command line using the standard
 * format:  key=value
 * Options identified as REQUIRED must be specified by user on command line.
 * The option string can either specify a range of values (e.g. "10-100") or
 * a list of acceptable values (e.g. "red,orange,yellow").  Unless the option
 * string is NULL, user provided input will be evaluated agaist this string.
 *
 ***************************************************************************
 *
 * G_disable_interactive()
 *
 * Disables the ability of the parser to operate interactively.
 *
 ***************************************************************************
 *
 * G_parser(argc, argv)
 *    int argc ;
 *    char **argv ;
 *
 * Parses the command line provided through argc and argv.  Example:
 * Assume the previous calls:
 *
 *  opt1 = G_define_option() ;
 *  opt1->key        = "map",
 *  opt1->type       = TYPE_STRING,
 *  opt1->required   = YES,
 *  opt1->checker    = sub,
 *  opt1->description= "Name of an existing raster map" ;
 *
 *  opt2 = G_define_option() ;
 *  opt2->key        = "color",
 *  opt2->type       = TYPE_STRING,
 *  opt2->required   = NO,
 *  opt2->answer     = "white",
 *  opt2->options    = "red,orange,blue,white,black",
 *  opt2->description= "Color used to display the map" ;
 *
 *  opt3 = G_define_option() ;
 *  opt3->key        = "number",
 *  opt3->type       = TYPE_DOUBLE,
 *  opt3->required   = NO,
 *  opt3->answer     = "12345.67",
 *  opt3->options    = "0-99999",
 *  opt3->description= "Number to test parser" ;
 *
 * parser() will respond to the following command lines as described:
 *
 * command      (No command line arguments)
 *    Parser enters interactive mode.
 *
 * command map=map.name
 *    Parser will accept this line.  Map will be set to "map.name", the
 *    'a' and 'b' flags will remain off and the num option will be set
 *    to the default of 5.
 *
 * command -ab map=map.name num=9
 * command -a -b map=map.name num=9
 * command -ab map.name num=9
 * command map.name num=9 -ab
 * command num=9 -a map=map.name -b
 *    These are all treated as acceptable and identical. Both flags are
 *    set to on, the map option is "map.name" and the num option is "9".
 *    Note that the "map=" may be omitted from the command line if it
 *    is part of the first option (flags do not count).
 *
 * command num=12
 *    This command line is in error in two ways.  The user will be told 
 *    that the "map" option is required and also that the number 12 is
 *    out of range.  The acceptable range (or list) will be printed.
 *
 * On error, G_parser() prints call G_usage() and returns -1.
 * Otherwise returns 0
 *
 ***************************************************************************
 *
 * G_recreate_command()
 *
 * Creates a command-line that runs the current command completely
 * non-interactive
 *
 ***************************************************************************
*/

#include <string.h>
#include <unistd.h>
#include "gis.h"

#define BAD_SYNTAX  1
#define OUT_OF_RANGE    2
#define MISSING_VALUE   3

static int interactive_ok = 1 ;
static int n_opts = 0 ;
static int n_flags = 0 ;

static struct Flag first_flag;    /* First flag in a linked list      */
static struct Flag *current_flag; /* Pointer for traversing list      */

static struct Option first_option ;
static struct Option *current_option ;

static char *pgm_name = NULL;

struct Item
{
	struct Option *option ;
	struct Flag *flag ;
	struct Item *next_item ;
} ;

static struct Item first_item ;
static struct Item *current_item ;
static int n_items = 0 ;
static int show_options(int ,char *);
static int show(char *,int);
static int set_flag (int);
static int contains (char *,int);
static int set_option( char *);
static int check_opts();
static int check_an_opt( char *, int , char *,char *);
static int check_int(char *, char *);
static int check_double( char *, char *);
static int check_string( char *, char *);
static int check_required();
static int split_opts();
static int check_multiple_opts();
static int interactive( char *);
static int interactive_flag( struct Flag *);
static int interactive_option( struct Option *);
static int gis_prompt( struct Option *, char *);

int 
G_disable_interactive (void)
{
	interactive_ok = 0 ;

        return 0;
}

struct Flag *
G_define_flag (void)
{
	struct Flag *flag ;
	struct Item *item ;

	/* Allocate memory if not the first flag */

	if (n_flags)
	{
		flag = (struct Flag *)G_malloc(sizeof(struct Flag)) ;
		current_flag->next_flag = flag ;
	}
	else
		flag = &first_flag ;

	/* Zero structure */

	G_zero ((char *) flag, sizeof(struct Flag));

	current_flag = flag ;
	n_flags++ ;

	if (n_items)
	{
		item = (struct Item *)G_malloc(sizeof(struct Item)) ;
		current_item->next_item = item ;
	}
	else
		item = &first_item ;

	G_zero ((char *) item, sizeof(struct Item));
	
	item->flag = flag ;
	item->option = NULL ;

	current_item = item ;
	n_items++ ;

	return(flag) ;
}

struct Option *
G_define_option (void)
{
	struct Option *opt ;
	struct Item *item ;

	/* Allocate memory if not the first option */

	if (n_opts)
	{
		opt = (struct Option *)G_malloc(sizeof(struct Option)) ;
		current_option->next_opt = opt ;
	}
	else
		opt = &first_option ;

	/* Zero structure */
	G_zero ((char *) opt, sizeof(struct Option));

	opt->required  = NO ;
	opt->multiple  = NO ;
	opt->answer    = NULL ;
	opt->answers   = NULL ;
	opt->def       = NULL ;
	opt->checker   = NULL ;
	opt->options   = NULL ;
	opt->key_desc  = NULL ;
	opt->gisprompt = NULL ;

	current_option = opt ;
	n_opts++ ;

	if (n_items)
	{
		item = (struct Item *)G_malloc(sizeof(struct Item)) ;
		current_item->next_item = item ;
	}
	else
		item = &first_item ;

	G_zero ((char *) item, sizeof(struct Item));
	
	item->option = opt ;
	item->flag = NULL ;

	current_item = item ;
	n_items++ ;

	return(opt) ;
}

/* The main parsing routine */

/*
**  Returns  0 on success
**          -1 on error
**          
*/
int G_parser (int argc, char **argv)
{
	int need_first_opt ;
	int opt_checked = 0;
	int error ;
	char *ptr ;
	int i;
	struct Option *opt ;

	error = 0 ;
	need_first_opt = 1 ;
	i = strlen(pgm_name = argv[0]) ;
	while (--i >= 0)
	{
		if (pgm_name[i] == '/')
		{
			pgm_name += i+1;
			break;
		}
	}

	/* Stash default answers */

	opt= &first_option;
	while(opt != NULL)
	{
		if(opt->multiple && opt->answers)
		{
			opt->answer = (char *)G_malloc(strlen(opt->answers[0])+1);
			strcpy(opt->answer, opt->answers[0]);
			for(i=1; opt->answers[i]; i++)
			{
				opt->answer = G_realloc (opt->answer,
						strlen(opt->answer)+
						strlen(opt->answers[i])+2);
				strcat(opt->answer, ",");
				strcat(opt->answer, opt->answers[i]);
			}
		}
		opt->def = opt->answer ;
		opt = opt->next_opt ;
	}
	
	/* If there are NO arguments, go interactive */

	if (argc < 2 && interactive_ok && isatty(0) )
	{
		interactive(argv[0]) ;
	        opt_checked = 1; 
		/* all options have been already checked interactively */
	}
	else if (argc < 2 && isatty(0))
		{
			G_usage();
			return -1;
		}
	else if (argc >= 2)
	{

		/* If first arg is "help" give a usage/syntax message */
		if (strcmp(argv[1],"help") == 0 || strcmp(argv[1], "-help") == 0)
		{
			G_usage();
			return -1;
		}

		/* If first arg is "--interface-description" then print out
		 * a xml description of the task */
		if (strcmp(argv[1],"--interface-description") == 0)
		{
			G_usage_xml();
			return -1;
		}

		/* Loop thru all command line arguments */

		while(--argc)
		{
			ptr = *(++argv) ;

			/* If we see a flag */
			if(*ptr == '-')
			{
				while(*(++ptr))
					error += set_flag(*ptr) ;

			}
			/* If we see standard option format (option=val) */
			else if (contains(ptr, '='))
			{
				error += set_option(ptr) ;
				need_first_opt = 0 ;
			}

			/* If we see the first option with no equal sign */
			else if (need_first_opt && n_opts)
			{
				first_option.answer = G_store(ptr) ;
				need_first_opt = 0 ;
			}

  	        /* If we see the non valid argument (no "=", just argument) */
			else if (contains(ptr, '=') == 0)
			{
				fprintf(stderr, "Sorry <%s> is not a valid option\n", ptr);
				error = 1;
			}

		}
	}

	/* Split options where multiple answers are OK */
	split_opts() ;

	/* Check multiple options */
	error += check_multiple_opts() ;

	/* Check answers against options and check subroutines */
	if(!opt_checked)
	   error += check_opts() ;

	/* Make sure all required options are set */
	error += check_required() ;

	if(error)
	{
		G_usage();
		return -1;
	}
	return(0) ;
}

int G_usage (void)
{
	struct Option *opt ;
	struct Flag *flag ;
	char item[256];
	char *key_desc;
	int maxlen;
	int len, n;

	fprintf (stderr, "\nUsage:\n ");

	if (!pgm_name)		/* v.dave && r.michael */
	    pgm_name = G_program_name ();
	if (!pgm_name)
	    pgm_name = "??";

	len = show(pgm_name,1);

	/* Print flags */

	if(n_flags)
	{
		item[0] = ' ';
		item[1] = '[';
		item[2] = '-';
		flag= &first_flag;
		for(n = 3; flag != NULL; n++, flag = flag->next_flag)
			item[n] = flag->key;
		item[n++] = ']';
		item[n] = 0;
		len=show(item,len);
	}

	maxlen = 0;
	if(n_opts)
	{
		opt= &first_option;
		while(opt != NULL)
		{
			if (opt->key_desc != NULL)
				key_desc = opt->key_desc;
			else if (opt->type == TYPE_STRING)
				key_desc = "name";
			else
				key_desc = "value";

			n = strlen (opt->key);
			if (n > maxlen) maxlen = n;

			strcpy(item," ");
			if(!opt->required )
				strcat (item, "[");
			strcat (item, opt->key);
			strcat (item, "=");
			strcat (item, key_desc);
			if (opt->multiple)
			{
				strcat(item,"[,");
				strcat(item,key_desc);
				strcat(item,",...]");
			}
			if(!opt->required )
				strcat(item,"]") ;

			len = show(item,len);

			opt = opt->next_opt ;
		}
	}
	fprintf (stderr, "\n");

	/* Print help info for flags */

	if(n_flags)
	{
		fprintf (stderr, "\nFlags:\n");
		flag= &first_flag;
		while(flag != NULL)
		{
			fprintf(stderr,"  -%c   %s\n",
			    flag->key, flag->description) ;
			flag = flag->next_flag ;
		}
	}

	/* Print help info for options */

	if(n_opts)
	{
		fprintf (stderr, "\nParameters:\n");
		opt= &first_option;
		while(opt != NULL)
		{
			fprintf (stderr, "  %*s   %s\n", maxlen, opt->key,
			    opt->description);
			if(opt->options)
				show_options(maxlen, opt->options) ;
				/*
				fprintf (stderr, "  %*s   options: %s\n", maxlen, " ",
					opt->options) ;
				*/
			if(opt->def)
				fprintf (stderr, "  %*s   default: %s\n", maxlen, " ",
					opt->def) ;
			opt = opt->next_opt ;
		}
	}

        return 0;
}

int G_usage_xml (void)
{
	struct Option *opt ;
	struct Flag *flag ;
	char *type;
	char * s;

	if (!pgm_name)		/* v.dave && r.michael */
	    pgm_name = G_program_name ();
	if (!pgm_name)
	    pgm_name = "??";

	fprintf(stdout, "<task name=\"%s\">\n", pgm_name);  

	fprintf(stdout, "\t<parameter-group>\n");
	if(n_opts)
	{
		opt= &first_option;
		while(opt != NULL)
		{
			/* TODO: make this a enumeration type? */
			switch (opt->type) {
				case TYPE_INTEGER:
					type = "integer";
					break ;
				case TYPE_DOUBLE:
					type = "float";
					break ;
				case TYPE_STRING:
					type = "string";
					break ;
				default:
					type = "unknown";
					break;
			}
			fprintf (stdout, "\t\t<parameter "
				"name=\"%s\" "
				"type=\"%s\" "
				"required=\"%s\">\n",
				opt->key,
				type,
				opt->required == YES ? "yes" : "no");

			if (opt->description)
				fprintf(stdout, "\t\t\t<description>\n\t\t\t\t%s\n\t\t\t</description>\n",
					opt->description);
			if(opt->def)
				fprintf(stdout, "\t\t\t<default>\n\t\t\t\t%s\n\t\t\t</default>\n", opt->def);

			if(opt->options) {
				fprintf(stdout, "\t\t\t<values>\n");
				s = (char *)calloc(strlen(opt->options),1);
				strcpy(s, opt->options);
				s = strtok(s, ",");
				while (s) {
					fprintf(stdout, "\t\t\t\t<value>%s</value>\n", s);
					s = strtok(NULL, ",");
				}
				fprintf(stdout, "\t\t\t</values>\n");
			}

			/* TODO:
			 * add something like
			 * 	 <range min="xxx" max="xxx"/>
			 * to <values>
			 * - multiple
			 * - key_desc
			 * - there surely are some more. which ones?
			 */

			opt = opt->next_opt ;
			fprintf (stdout, "\t\t</parameter>\n");
		}
	}

	
	if(n_flags)
	{
		flag= &first_flag;
		while(flag != NULL)
		{
			fprintf (stdout, "\t\t<parameter name=\"%c\" type=\"flag\">\n", flag->key);

			if (flag->description)
				fprintf(stdout, "\t\t\t<description>\n\t\t\t\t%s\n\t\t\t</description>\n",
							flag->description);
			flag = flag->next_flag ;
			fprintf (stdout, "\t\t</parameter>\n");
		}
	}

	fprintf(stdout, "\t</parameter-group>\n");

	fprintf(stdout, "</task>\n");
    return 0;
}

/**************************************************************************
 *
 * The remaining routines are all local (static) routines used to support
 * the parsing process.
 *
 **************************************************************************/

static int show_options(int maxlen,char *str)
{
	char buff[1024] ;
	char *p1, *p2 ;
	int totlen, len ;

	strcpy(buff, str) ;
	fprintf (stderr, "  %*s   options: ", maxlen, " ") ;
	totlen = maxlen + 13 ;
	p1 = buff ;
	while(p2 = G_index(p1, ','))
	{
		*p2 = '\0' ;
		len = strlen(p1) + 1 ;
		if ((len + totlen) > 76)
		{
			totlen = maxlen + 13 ;
			fprintf(stderr, "\n %*s", maxlen + 13, " ") ;
		}
		fprintf (stderr, "%s,",  p1) ;
		totlen += len ;
		p1 = p2 + 1 ;
	}
	len = strlen(p1) ;
	if ((len + totlen) > 76 )
		fprintf(stderr, "\n %*s", maxlen + 13, " ") ;
	fprintf (stderr, "%s\n",  p1) ;

        return 0;
}

static int show (char *item, int len)
{
	int n;

	n = strlen (item)+(len>0);
	if (n + len > 76)
	{
		if (len)
			fprintf (stderr, "\n  ");
		len = 0;
	}
	fprintf (stderr, "%s", item);
	return n+len;
}

static int set_flag (int f)
{
	struct Flag *flag ;

	/* Flag is not valid if there are no flags to set */

	if(!n_flags)
	{
		fprintf(stderr,"Sorry, <%c> is not a valid flag\n", f) ;
		return(1) ;
	}

	/* Find flag with corrrect keyword */

	flag= &first_flag;
	while(flag != NULL)
	{
		if( flag->key == f)
		{
			flag->answer = 1 ;
			return(0) ;
		}
		flag = flag->next_flag ;
	}

	fprintf(stderr,"Sorry, <%c> is not a valid flag\n", f) ;
	return(1) ;
}

/* contents() is used to find things strings with characters like commas and
 * dashes.
 */
static int contains (char *s, int c)
{
	while(*s)
	{
		if(*s == c)
			return(1) ;
		s++ ;
	}
	return(0) ;
}

static int set_option (char *string)
{
	struct Option *at_opt ;
	struct Option *opt ;
	int got_one ;
	int key_len ;
	char the_key[64] ;
	char *ptr ;

	for(ptr=the_key; *string!='='; ptr++, string++)
		*ptr = *string ;
	*ptr = '\0' ;
	string++ ;

	/* Find option with best keyword match */
	got_one = 0 ;
	key_len = strlen(the_key) ;
	for(at_opt= &first_option; at_opt != NULL; at_opt=at_opt->next_opt)
	{
		if (strncmp(the_key,at_opt->key,key_len))
			continue ;

		got_one++;
		opt = at_opt ;

		/* changed 1/15/91 -dpg   old code is in parser.old */
		/* overide ambiguous check, if we get an exact match */
		if (strlen (at_opt->key) == key_len) 	
		{
		    opt = at_opt;
		    got_one = 1;
		    break;
		}
	}

	if (got_one > 1)
	{
		fprintf(stderr,"Sorry, <%s=> is ambiguous\n", the_key) ;
		return(1) ;
	}

	/* If there is no match, complain */
	if(got_one == 0)
	{
		fprintf(stderr,"Sorry, <%s> is not a valid parameter\n",
			the_key) ;
		return(1) ;
	}
		
	/* Allocate memory where answer is stored */
	if (opt->count++)
	{
		opt->answer = G_realloc (opt->answer,
			strlen (opt->answer)+strlen(string)+2);
		strcat (opt->answer, ",");
		strcat (opt->answer, string);
	}
	else
		opt->answer = G_store(string) ;
	return(0) ;
}

static int check_opts (void)
{
	struct Option *opt ;
	int error ;
	int ans ;

	error = 0 ;

	if(! n_opts)
		return(0) ;

	opt= &first_option;
	while(opt != NULL)
	{
		/* Check answer against options if any */

		if(opt->options && opt->answer)
		{
			if(opt->multiple == 0)
				error += check_an_opt(opt->key, opt->type,
				    opt->options, opt->answer) ;
			else
			{
				for(ans=0; opt->answers[ans] != '\0'; ans++)
					error += check_an_opt(opt->key, opt->type,
					    opt->options, opt->answers[ans]) ;
			}
		}

		/* Check answer against user's check subroutine if any */

		if(opt->checker)
			error += opt->checker(opt->answer) ;

		opt = opt->next_opt ;
	}
	return(error) ;
}

static int check_an_opt (char *key, int type, char *options, char *answer)
{
	int error ;

	error = 0 ;

	switch(type)
	{
	case TYPE_INTEGER:
		error = check_int(answer,options) ;
		break ;
	case TYPE_DOUBLE:
		error = check_double(answer,options) ;
		break ;
	case TYPE_STRING:
		error = check_string(answer,options) ;
		break ;
/*
	case TYPE_COORDINATE:
		error = check_coor(answer,options) ;
		break ;
*/
	}
	switch(error)
	{
	case 0:
		break ;
	case BAD_SYNTAX:
		fprintf(stderr,"\nError: illegal range syntax for parameter <%s>\n",
		    key) ;
		fprintf(stderr,"       Presented as: %s\n", options) ;
		break ;
	case OUT_OF_RANGE:
		fprintf(stderr,"\nError: value <%s> out of range for parameter <%s>\n",
		    answer, key) ;
		fprintf(stderr,"       Legal range: %s\n", options) ;
		break ;
	case MISSING_VALUE:
		fprintf(stderr,"\nError: Missing value for parameter <%s>\n",
		    key) ;
	}
	return(error) ;
}

static int check_int (char *ans, char *opts)
{
	int d, lo, hi;

	if (1 != sscanf(ans,"%d", &d))
		return(MISSING_VALUE) ;

	if (contains(opts, '-'))
	{
		if (2 != sscanf(opts,"%d-%d",&lo, &hi))
			return(BAD_SYNTAX) ;
		if (d < lo || d > hi)
			return(OUT_OF_RANGE) ;
		else
			return(0) ;
	}
	else if (contains(opts, ','))
	{
		for(;;)
		{
			if (1 != sscanf(opts,"%d",&lo))
				return(BAD_SYNTAX) ;
			if (d == lo)
				return(0) ;
			while(*opts != '\0' && *opts != ',')
				opts++ ;
			if (*opts == '\0')
				return(OUT_OF_RANGE) ;
			if (*(++opts) == '\0')
				return(OUT_OF_RANGE) ;
		}
	}
	else
	{
		if (1 != sscanf(opts,"%d",&lo))
			return(BAD_SYNTAX) ;
		if (d == lo)
			return(0) ;
		return(OUT_OF_RANGE) ;
	}
}

/*
static int
check_coor(ans, opts)
char *ans ;
char *opts ;
{
	double xd, xlo, xhi;
	double yd, ylo, yhi;

	if (1 != sscanf(ans,"%lf,%lf", &xd, &yd))
		return(MISSING_VALUE) ;

	if (contains(opts, '-'))
	{
		if (2 != sscanf(opts,"%lf-%lf,%lf-%lf",&xlo, &xhi, &ylo, &yhi))
			return(BAD_SYNTAX) ;
		if (xd < xlo || xd > xhi)
			return(OUT_OF_RANGE) ;
		if (yd < ylo || yd > yhi)
			return(OUT_OF_RANGE) ;
		return(0) ;
	}
	return(BAD_SYNTAX) ;
}
*/

static int check_double (char *ans, char *opts)
{
	double d, lo, hi;

	if (1 != sscanf(ans,"%lf", &d))
		return(MISSING_VALUE) ;

	if (contains(opts, '-'))
	{
		if (2 != sscanf(opts,"%lf-%lf",&lo, &hi))
			return(BAD_SYNTAX) ;
		if (d < lo || d > hi)
			return(OUT_OF_RANGE) ;
		else
			return(0) ;
	}
	else if (contains(opts, ','))
	{
		for(;;)
		{
			if (1 != sscanf(opts,"%lf",&lo))
				return(BAD_SYNTAX) ;
			if (d == lo)
				return(0) ;
			while(*opts != '\0' && *opts != ',')
				opts++ ;
			if (*opts == '\0')
				return(OUT_OF_RANGE) ;
			if (*(++opts) == '\0')
				return(OUT_OF_RANGE) ;
		}
	}
	else
	{
		if (1 != sscanf(opts,"%lf",&lo))
			return(BAD_SYNTAX) ;
		if (d == lo)
			return(0) ;
		return(OUT_OF_RANGE) ;
	}
}

static int check_string (char *ans, char *opts)
{
	if (*opts == '\0')
		return(0) ;

	if (contains(opts, ','))
	{
		for(;;)
		{
			if ((! strncmp(ans, opts, strlen(ans)))
			    && ( *(opts+strlen(ans)) == ','
			       ||  *(opts+strlen(ans)) == '\0'))
				return(0) ;
			while(*opts != '\0' && *opts != ',')
				opts++ ;
			if (*opts == '\0')
				return(OUT_OF_RANGE) ;
			if (*(++opts) == '\0')
				return(OUT_OF_RANGE) ;
		}
	}
	else
	{
		if (! strcmp(ans, opts))
			return(0) ;
		return(OUT_OF_RANGE) ;
	}
}

static int check_required (void)
{
	struct Option *opt ;
	int err ;

	err = 0 ;

	if(! n_opts)
		return(0) ;

	opt= &first_option;
	while(opt != NULL)
	{
		if(opt->required && opt->answer == NULL)
		{
			fprintf(stderr,"\nERROR: Required parameter <%s> not set:\n    (%s).\n",
			    opt->key, opt->description) ;
			err++ ;
		}
		opt = opt->next_opt ;
	}

	return(err) ;
}

static int split_opts (void)
{
	struct Option *opt ;
	char *ptr1 ;
	char *ptr2 ;
	int allocated ;
	int ans_num ;
	int len ;


	if(! n_opts)
		return 0;

	opt= &first_option;
	while(opt != NULL)
	{
		if (/*opt->multiple && */(opt->answer != NULL))
		{
			/* Allocate some memory to store array of pointers */
			allocated = 10 ;
			opt->answers = (char **)G_malloc(allocated * sizeof(char *)) ;

			ans_num = 0 ;
			ptr1 = opt->answer ;
			opt->answers[ans_num] = NULL ;

			for(;;)
			{
				for(len=0, ptr2=ptr1; *ptr2 != '\0' && *ptr2 != ','; ptr2++, len++)
					;

				if (len > 0)        /* skip ,, */
				{
					opt->answers[ans_num]=G_malloc(len+1) ;
					G_copy(opt->answers[ans_num], ptr1, len) ;
					opt->answers[ans_num][len] = 0;

					ans_num++ ;

					if(ans_num >= allocated)
					{
						allocated += 10 ;
						opt->answers =
						    (char **)G_realloc((char *)opt->answers,
						    allocated * sizeof(char *)) ;
					}

					opt->answers[ans_num] = NULL ;
				}

				if(*ptr2 == '\0')
					break ;

				ptr1 = ptr2+1 ;

				if(*ptr1 == '\0')
					break ;
			}
		}
		opt = opt->next_opt ;
	}

	return 0;
}

static int check_multiple_opts (void)
{
	struct Option *opt ;
	char *ptr ;
	int n_commas ;
	int n ;
	int error ;

	if(! n_opts)
		return (0) ;

	error = 0 ;
	opt= &first_option;
	while(opt != NULL)
	{
		if ((opt->answer != NULL) && (opt->key_desc != NULL))
		{
			/* count commas */
			n_commas = 1 ;
			for(ptr=opt->key_desc; *ptr!='\0'; ptr++)
				if (*ptr == ',')
					n_commas++ ;
			/* count items */
			for(n=0;opt->answers[n] != '\0';n++)
				;
			/* if not correct multiple of items */
			if(n % n_commas)
			{
				fprintf(stderr,"\nError: option <%s> must be provided in multiples of %d\n",
					opt->key, n_commas) ;
				fprintf(stderr,"       You provided %d items:\n", n) ;
				fprintf(stderr,"       %s\n", opt->answer) ;
				error++ ;
			}
		}
		opt = opt->next_opt ;
	}
	return(error) ;
}

static int interactive( char *command)
{
	struct Item *item ;

	/* Query for flags */

	if(!n_items)
	{
		fprintf(stderr,"Programmer error: no flags or options\n") ;
		exit(-1) ;
	}

	for (item= &first_item ;;)
	{
		if (item->flag)
			interactive_flag(item->flag) ;
		else if (item->option)
			interactive_option(item->option) ;
		else
			break ;

		item=item->next_item ;

		if (item == NULL)
			break ;
	}

	return 0;
}

static int interactive_flag( struct Flag *flag )
{
	char buff[1024] ;
	fprintf(stderr, "\nFLAG: Set the following flag?\n") ;
	sprintf(buff,"    %s?", flag->description) ;
	flag->answer = G_yes(buff, 0) ;

	return 0;
}

static int interactive_option(struct Option *opt )
{
	char buff[1024],*bptr ;
	char buff2[1024] ;
	int set_one ;

	fprintf(stderr,"\nOPTION:   %s\n", opt->description) ;
	fprintf(stderr,"     key: %s\n", opt->key) ;
	if (opt->key_desc)
	fprintf(stderr,"  format: %s\n", opt->key_desc) ;
	if (opt->def)
	fprintf(stderr," default: %s\n", opt->def) ;
	fprintf(stderr,"required: %s\n", opt->required ? "YES" : "NO") ;
	if (opt->multiple)
	fprintf(stderr,"multiple: %s\n", opt->multiple ? "YES" : "NO") ;
	if (opt->options)
	fprintf(stderr," options: %s\n", opt->options) ;
	/*
	show_options(0, opt->options) ;
	*/

	set_one = 0 ;
	for(;;)
	{
	   *buff='\0' ;
	   if(opt->gisprompt)
		gis_prompt(opt, buff) ;
	   else
	   {
		fprintf(stderr,"enter option > ") ;
		if(fgets(buff,1024,stdin) == 0) exit(1); ;
                bptr = buff;  /* strip newline  */
                while(*bptr) {if(*bptr=='\n') *bptr='\0'; bptr++;}

	   }

	   if(strlen(buff) != 0)
	   {
	        if(opt->options)
		/* then check option */
	        {
		    if (check_an_opt(opt->key, opt->type, opt->options, buff))
	            {
		        if (G_yes("   Try again? ", 1))
		    		continue ;
	    	        else
				exit(-1) ;
		    }
		}
		if (opt->checker)
	 	    if (opt->checker(buff))
		    {
		    	    fprintf(stderr,"Sorry, %s is not accepted.\n", buff) ;
			    *buff = '\0' ;
			    if (G_yes("   Try again? ", 1))
			    	continue ;
			    else
				exit(-1) ;
		    }

		sprintf(buff2,"%s=%s", opt->key, buff) ;
		if(! opt->gisprompt)
		{
			fprintf(stderr,"\nYou have chosen:\n  %s\n", buff2) ;
			if (G_yes("Is this correct? ", 1))
			{
				set_option(buff2) ;
				set_one++ ;
			}
		}
		else 
		{
			set_option(buff2) ;
			set_one++ ;
		}
	   } /* if strlen(buf ) !=0 */

	   if ((strlen(buff) == 0) && opt->required && (set_one == 0))
		exit(-1) ;
	   if ((strlen(buff) == 0) && (set_one > 0) && opt->multiple )
		break ;
	   if ((strlen(buff) == 0) && !opt->required)
		break ;
	   if ((set_one == 1) && !opt->multiple)
		break ;
	}
	return(0) ;
}

static int gis_prompt (struct Option *opt, char *buff)
{
	char age[64] ;
	char element[64] ;
	char desc[64] ;
	char *ptr1, *ptr2 ;

	for(ptr1=opt->gisprompt,ptr2=age; *ptr1!='\0'; ptr1++, ptr2++)
	{
		if (*ptr1 == ',')
			break ;
		*ptr2 = *ptr1 ;
	}
	*ptr2 = '\0' ;

	for(ptr1++, ptr2=element; *ptr1!='\0'; ptr1++, ptr2++)
	{
		if (*ptr1 == ',')
			break ;
		*ptr2 = *ptr1 ;
	}
	*ptr2 = '\0' ;

	for(ptr1++, ptr2=desc; *ptr1!='\0'; ptr1++, ptr2++)
	{
		if (*ptr1 == ',')
			break ;
		*ptr2 = *ptr1 ;
	}
	*ptr2 = '\0' ;
	/*********ptr1 points to current mapset description***********/

	if (opt->answer)
		G_set_ask_return_msg ("to accept the default");
	if (! strcmp("old",age))
	{
		ptr1 = G_ask_old("", buff, element, desc) ;
		if (ptr1)
		{
		    strcpy (buff, G_fully_qualified_name(buff,ptr1));
		}
	}
	else if (! strcmp("new",age))
		ptr1 = G_ask_new("", buff, element, desc) ;
	else if (! strcmp("mapset",age))
		ptr1 = G_ask_in_mapset("", buff, element, desc) ;
	else if (! strcmp("any",age))
		ptr1 = G_ask_any("", buff, element, desc, 1) ;
	else
	{
		fprintf(stderr,"\nPROGRAMMER ERROR: first item in gisprompt is <%s>\n", age) ;
		fprintf(stderr,"        Must be either new, old, mapset, or any\n") ;
		return -1;
	}
	if (ptr1 == '\0')
		*buff = '\0';

	return 0;
}

char *G_recreate_command (void)
{
	char flg[2] ;
	static char buff[1024] ;
	struct Flag *flag ;
	struct Option *opt ;
	int n ;

	/* Flag is not valid if there are no flags to set */

	*buff = '\0' ;
	strcat(buff, G_program_name()) ;

	if(n_flags)
	{
		flag= &first_flag;
		while(flag != '\0')
		{
			if( flag->answer == 1 )
			{
				strcat (buff, " -") ;
				flg[0] = flag->key ; flg[1] = '\0'; strcat (buff, flg) ;
			}
			flag = flag->next_flag ;
		}
	}

	opt= &first_option;
	while(opt != '\0')
	{
		if (opt->answer != '\0')
		{
			strcat(buff, " ") ;
			strcat(buff, opt->key) ;
			strcat(buff, "=") ;
			strcat(buff, opt->answers[0]) ;
			for(n=1;opt->answers[n] != '\0';n++)
			{
				strcat(buff, ",") ;
				strcat(buff, opt->answers[n]) ;
			}
		}
		opt = opt->next_opt ;
	}

	return(buff) ;
}

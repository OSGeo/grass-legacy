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
*/

#include "gis.h"

#define BAD_SYNTAX  1
#define OUT_OF_RANGE    2
#define MISSING_VALUE   3

static int interactive_ok = 1 ;
static int n_opts = 0 ;
static int n_flags = 0 ;

static struct Flag first_flag = NULL;    /* First flag in a linked list      */
static struct Flag *current_flag = NULL; /* Pointer for traversing list      */

static struct Option first_option ;
static struct Option *current_option ;

static char *pgm_name = "??";

G_disable_interactive()
{
	interactive_ok = 0 ;
}

struct Flag *
G_define_flag()
{
	struct Flag *flag ;

	/* Allocate memory if not the first flag */

	if (n_flags)
	{
		flag = (struct Flag *)G_malloc(sizeof(struct Flag)) ;
		current_flag->next_flag = flag ;
	}
	else
		flag = &first_flag ;

	/* Zero structure */

	G_zero (flag, sizeof(struct Flag));

	current_flag = flag ;
	n_flags++ ;
	return(flag) ;
}

struct Option *
G_define_option()
{
	struct Option *opt ;

	/* Allocate memory if not the first option */

	if (n_opts)
	{
		opt = (struct Option *)G_malloc(sizeof(struct Option)) ;
		current_option->next_opt = opt ;
	}
	else
		opt = &first_option ;

	/* Zero structure */
	G_zero (opt, sizeof(struct Option));

	opt->required = NO ;
	opt->multiple = NO ;
	opt->answer = NULL ;

	current_option = opt ;
	n_opts++ ;

	return(opt) ;
}

/* The main parsing routine */

G_parser(argc, argv)
int argc ;
char **argv ;
{
	int need_first_opt ;
	int error ;
	char *ptr ;
	int i;

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

	/* If there are NO arguments, go interactive */

	if (argc < 2 && interactive_ok)
		interactive(argv[0]) ;
	
	else
	{
		if (argc < 2)
		{
			G_usage();
			return -1;
		}

		/* If first arg is "help" give a usage/syntax message */
		if (strcmp(argv[1],"help") == 0 || strcmp(argv[1], "-help") == 0)
		{
			G_usage();
			return -1;
		}

		argc-- ;

		/* Loop thru all command line arguments */

		while(argc--)
		{
			ptr = *(++argv) ;

			/* If we see a flag */
			if(*ptr == '-')
				while(*(++ptr))
					error += set_flag(*ptr) ;

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
		}
	}

	/* Split options where multiple answers are OK */
	split_opts() ;

	/* Check answers against options and check subroutines */
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

G_usage()
{
	struct Option *opt ;
	struct Flag *flag ;
	char item[256];
	char *key_desc;
	int maxlen;
	int len, n;

	fprintf (stderr, "\nUsage:\n ");
	len = show(pgm_name,1);

	/* Print flags */

	if(n_flags)
	{
		item[0] = ' ';
		item[1] = '[';
		item[2] = '-';
		flag=&first_flag;
		for(n = 3; flag != NULL; n++, flag = flag->next_flag)
			item[n] = flag->key;
		item[n++] = ']';
		item[n] = 0;
		len=show(item,len);
	}

	maxlen = 0;
	if(n_opts)
	{
		opt=&first_option;
		while(opt != NULL)
		{
			if (opt->key_desc != NULL)
				key_desc = opt->key_desc;
			else if (opt->type == TYPE_STRING)
				key_desc = "name";
			else if (opt->type == TYPE_COORDINATE)
				key_desc = "x,y";
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
				strcat(item,"[:");
				strcat(item,key_desc);
				strcat(item,":...]");
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
		flag=&first_flag;
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
		opt=&first_option;
		while(opt != NULL)
		{
			fprintf (stderr, "  %*s   %s\n", maxlen, opt->key,
			    opt->description);
			opt = opt->next_opt ;
		}
	}
}

/**************************************************************************
 *
 * The remaining routines are all local (static) routines used to support
 * the parsing process.
 *
 **************************************************************************/

static
show(item,len)
char *item;
{
	int n;

	n = strlen (item)+(len>0);
	if (n + len > 76)
	{
		if (len)
			fprintf (stderr, " \\\n  ");
		len = 0;
	}
	fprintf (stderr, "%s", item);
	return n+len;
}

static
set_flag(f)
char f ;
{
	struct Flag *flag ;

	/* Flag is not valid if there are no flags to set */

	if(!n_flags)
	{
		fprintf(stderr,"Sorry, <%c> is not a valid flag\n", f) ;
		return(1) ;
	}

	/* Find flag with corrrect keyword */

	flag=&first_flag;
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
static
contains(s, c)
char *s ;
char c ;
{
	while(*s)
	{
		if(*s == c)
			return(1) ;
		s++ ;
	}
	return(0) ;
}

static
set_option(string)
char *string ;
{
	struct Option *opt ;

	/* Find option with same keyword */
	opt=&first_option;
	while(opt != NULL)
	{
		if( ! strncmp(opt->key,string,strlen(opt->key)) )
		{
			while(*(string++) != '=')
				;
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

		opt = opt->next_opt ;
	}

	fprintf(stderr,"Sorry, <") ;
	while(*string != '=')
		putc(*(string++), stderr) ;
	fprintf(stderr,"> is not a valid parameter\n") ;
	return(1) ;
}

static
check_opts()
{
	struct Option *opt ;
	int error ;
	int ans ;

	error = 0 ;

	if(! n_opts)
		return(0) ;

	opt=&first_option;
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
				for(ans=0; opt->answers[ans] != NULL; ans++)
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

static
check_an_opt(key, type, options, answer)
char *key ;
int type ;
char *options, *answer ;
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
	case TYPE_COORDINATE:
		error = check_coor(answer,options) ;
		break ;
	}
	switch(error)
	{
	case 0:
		break ;
	case BAD_SYNTAX:
		fprintf(stderr,"Error: illegal range syntax for parameter <%s>\n",
		    key) ;
		fprintf(stderr,"       Presented as: %s\n", options) ;
		break ;
	case OUT_OF_RANGE:
		fprintf(stderr,"Error: value <%s> out of range for parameter <%s>\n",
		    answer, key) ;
		fprintf(stderr,"       Legal range: %s\n", options) ;
		break ;
	case MISSING_VALUE:
		fprintf(stderr,"Error: Missing value for parameter <%s>\n",
		    key) ;
	}
	return(error) ;
}

static
check_int(ans, opts)
char *ans ;
char *opts ;
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
			while(*opts != NULL && *opts != ',')
				opts++ ;
			if (*opts == NULL)
				return(OUT_OF_RANGE) ;
			if (*(++opts) == NULL)
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

static
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
		if (2 != sscanf(opts,"%lf-%lf,$lf-%lf",&xlo, &xhi, &ylo, &yhi))
			return(BAD_SYNTAX) ;
		if (xd < xlo || xd > xhi)
			return(OUT_OF_RANGE) ;
		if (yd < ylo || yd > yhi)
			return(OUT_OF_RANGE) ;
		return(0) ;
	}
	return(BAD_SYNTAX) ;
}

static
check_double(ans, opts)
char *ans ;
char *opts ;
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
			while(*opts != NULL && *opts != ',')
				opts++ ;
			if (*opts == NULL)
				return(OUT_OF_RANGE) ;
			if (*(++opts) == NULL)
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

static
check_string(ans, opts)
char *ans ;
char *opts ;
{
	if (*opts == NULL)
		return(0) ;

	if (contains(opts, ','))
	{
		for(;;)
		{
			if ((! strncmp(ans, opts, strlen(ans)))
			    && ( *(opts+strlen(ans)) == ','
			       ||  *(opts+strlen(ans)) == NULL))
				return(0) ;
			while(*opts != NULL && *opts != ',')
				opts++ ;
			if (*opts == NULL)
				return(OUT_OF_RANGE) ;
			if (*(++opts) == NULL)
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
static
check_required()
{
	struct Option *opt ;
	int err ;

	err = 0 ;

	if(! n_opts)
		return(0) ;

	opt=&first_option;
	while(opt != NULL)
	{
		if(opt->required && opt->answer == NULL)
		{
			fprintf(stderr,"Required parameter <%s> not set (%s).\n",
			    opt->key, opt->description) ;
			err++ ;
		}
		opt = opt->next_opt ;
	}

	return(err) ;
}

static
split_opts()
{
	struct Option *opt ;
	char *ptr1 ;
	char *ptr2 ;
	int allocated ;
	int ans_num ;
	int len ;


	if(! n_opts)
		return ;

	opt=&first_option;
	while(opt != NULL)
	{
		if (opt->multiple && (opt->answer != NULL))
		{
			/* Allocate some memory to store array of pointers */
			allocated = 10 ;
			opt->answers = (char **)G_malloc(allocated * sizeof(char *)) ;

			ans_num = 0 ;
			ptr1 = opt->answer ;
			opt->answers[ans_num] = NULL ;

			for(;;)
			{
				for(len=0, ptr2=ptr1; *ptr2 != NULL && *ptr2 != ':'; ptr2++, len++)
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

				if(*ptr2 == NULL)
					break ;

				ptr1 = ptr2+1 ;

				if(*ptr1 == NULL)
					break ;
			}
		}

		opt = opt->next_opt ;
	}
}

static
interactive(command)
{
	struct Flag *flag ;
	struct Option *opt ;
	char buff[256] ;
	char buff2[256] ;
	int do_set ;
	int error ;

	/* Query for flags */

	if(n_flags)
	{
		printf("\nSet flags for <%s> by answering yes or no to the following:\n\n",
		   command) ;
		flag=&first_flag;
		while(flag != NULL)
		{
			sprintf(buff,"    %s?", flag->description) ;
			flag->answer = G_yes(buff, 0) ;
			flag = flag->next_flag ;
		}
	}

	/* Query for options */
	if(n_opts)
	{
		printf("\nSet options for <%s> by answering the following:\n", command) ;

		opt=&first_option;
		while(opt != NULL)
		{
			for(error=1; error;)
			{
				do
				{
					do_set = 1 ;
					printf("\n  option: %s\n", opt->description) ;
					printf("     key: %s\n", opt->key) ;
					printf("required: %s\n", opt->required ? "YES" : "NO") ;
					printf("multiple: %s\n", opt->multiple ? "YES" : "NO") ;
					printf("enter option > ") ;
					*buff=NULL ;
					gets(buff) ;
					if(strlen(buff) == 0 && opt->required == 0)
					{
						do_set = 0 ;
						break ;
					}
					sprintf(buff2,"%s=%s", opt->key, buff) ;
					printf("\nYou have chosen:\n  %s\n", buff2) ;
				} while( 0 == G_yes("Is this correct? ", 1)) ;

				if(opt->checker)
				{
					if (error = opt->checker(opt->answer))
					{
						printf("Sorry, %s is not accepted.\n", opt->answer) ;
						if (G_yes("   Try again? ", 1))
							;
						else
							exit(-1) ;
					}
				}
				else
					error = 0 ;
			}

			if(do_set)
				set_option(buff2) ;

			opt = opt->next_opt ;
		}
	}
	return(0) ;
}

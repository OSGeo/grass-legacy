/*  %W%  %G%  */

/* D_parse_commands(argc, argv, variables, n_variables, stash_away)
 *    int argc                      number of arguments
 *    char *argv[]                  argument array
 *    struct variables variables[]  alias (string) and position values
 *    int n_variables               # vars in above structure
 *    int (*stash_away)()           routine to call with parsed info
 *
 * This routine parses command line information of the forms:
 *    command opt1 opt2 opt3 opt4
 *         arguments are options in correct positions
 *    command opt1 - - opt4
 *         arguments are options in correct positions, where minuses (-)
 *         are interpreted as "accept the default for this position"
 *    command arg2=opt2 arg4=opt4 arg3=opt3 arg1=opt1
 *         arguments are in mixed order, but the correct position is
 *         ascertained by looking for the "arg" string in the variables
 *         structure.  This structure contains the "correct" position
 *         for the option.
 *    command opt1 - arg4=opt4
 *         mixed form of the above commands
 *
 * Once a position is determined, either by actual position or by deduction,
 *    the position and option are sent to the specified routine (stash_away).
 *
 * Review  gis/src/D/programs/text/*.[ch]  for an example.
 */

struct variables
{
	char *alias ;
	int position ;
} ;

D_parse_command(argc, argv, variables, n_variables, stash_away)
	int argc ;
	char *argv[] ;
	struct variables variables[] ;
	int n_variables ;
	int (*stash_away)() ;
{
	char var[32] ;
	char option[32] ;
	int i ;
	int j ;
	int using_named ;
	int position ;

	using_named = 0 ;

	for(i=1; i<argc; i++)
	{
		if (! strcmp(argv[i], "-"))
			continue ;

		if (! strcmp(argv[i], "help"))
			return(-1) ;

		if(D_have_equal(argv[i]))
		{
			if (2 != sscanf(argv[i],"%[^=]=%s", var, option))
				return(-2) ;

			using_named = 1 ;
			position = 0 ;
			for(j=0; j<n_variables; j++)
			{
				if (!strcmp(var, variables[j].alias))
				{
				    position = variables[j].position ;
				    break ;
				}
			}
			if (! position)
				return(-2) ;
		}
		else
		{
			if (using_named)
				return(-3) ;
			position = i ;
			strcpy(option, argv[i]) ;
		}
		
		if ((*stash_away)(position, option))
			return(-4) ;
	}
	return(0) ;
}

static
D_have_equal(str)
	char *str ;
{
	for(; *str; str++)
		if (*str == '=')
			return(1) ;
	return(0) ;
}

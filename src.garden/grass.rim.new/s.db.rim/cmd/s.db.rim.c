# include <stdio.h>

main(argc,argv)
	int	argc;
	char	**argv;
{
	int	quit=0,
		numarg=argc-1,
		i=1,
		retval;
	char	ar0[256],
		ar1[256];

	(void)strcpy(ar0,argv[0]);
	if ( i<argc && strcmp(argv[i],"-q")==0 ) {
		--numarg;
		quit = 1;
		++i;
	}
	if ( i<argc )
		(void)strcpy(ar1,argv[i]);
	else
		*ar1 = '\0';

	retval = toplevel_(&numarg, &quit, ar0, ar1, strlen(ar0), strlen(ar1));
	exit(0);
}

# include <stdio.h>

main(argc,argv)
	int	argc;
	char	**argv;
{
	int	numarg=argc-1,
		retval;
	char	ar0[256],
		ar1[256];

	(void)strcpy(ar0,argv[0]);
	if ( argc>1 )
		(void)strcpy(ar1,argv[1]);
	else
		*ar1 = '\0';
	retval = toplevel_(&numarg, ar0, ar1, strlen(ar0), strlen(ar1));
	exit(0);
}

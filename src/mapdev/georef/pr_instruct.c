
#include	<stdio.h>

pr_instructions()
{

    int   i ;
    char  command[82] ;

/*  This is simpler then trying to keep track of the path to a help file  */

printf("\n\n\n") ;
printf("  The map will have to be registrated to the digitizer:\n\n") ;
printf("  On the following screen type the coordinates (Easting/Northing)  of the\n") ;
printf("  registration points from the map.  There must be at least four registration\n  points.\n\n") ;
printf("  After typing the points hit the escape key <Esc>.\n\n") ;
printf("  The next screen will walk thru each set of registration coordinates and you\n") ;
printf("  will mark each coordinate on the map with the digitizer cursor.\n") ;

	for (i=11; i<22; i++)
		printf("\n") ;

    printf("\n              Hit <RETURN> to continue:") ;
    if (gets( command) == NULL)
	clearerr(stdin) ;

    return(0) ;
}

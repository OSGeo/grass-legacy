
#include	<stdio.h>

int 
pr_instructions (void)
{

    int   i ;
    char  command[82] ;

/*  This is simpler then trying to keep track of the path to a help file  */

fprintf (stdout,"\n\n\n") ;
fprintf (stdout,"  The map will have to be registrated to the digitizer:\n\n") ;
fprintf (stdout,"  On the following screen type the coordinates (Easting/Northing)  of the\n") ;
fprintf (stdout,"  registration points from the map.  There must be at least four registration\n  points.\n\n") ;
fprintf (stdout,"  After typing the points hit the escape key <Esc>.\n\n") ;
fprintf (stdout,"  The next screen will walk thru each set of registration coordinates and you\n") ;
fprintf (stdout,"  will mark each coordinate on the map with the digitizer cursor.\n") ;

	for (i=11; i<22; i++)
		fprintf (stdout,"\n") ;

    fprintf (stdout,"\n              Hit <RETURN> to continue:") ;
    if(fgets(command,82,stdin)== NULL)
	clearerr(stdin) ;

    return(0) ;
}

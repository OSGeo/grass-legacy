/*
 * MASK allows the user to:
 *    Remove the current mask
 *    Change the current mask
 */

#include <unistd.h>
#include <stdlib.h>
#include "gis.h"
#include "r.reclass.h"
static int title(void);

int 
main (int argc, char *argv[])
{
    char buf[80]  ;

/* Initialize GIS routines ****************************************************/
    G_gisinit(argv[0]) ;

/* Dump the advertising *******************************************************/
    title();

/* Check to see if user wants to remove or change the current mask */
    for(;;)
    {
	fprintf (stdout,"\n\nOptions:\n") ;
	fprintf (stdout,"    1      Remove the current mask\n") ;
	fprintf (stdout,"    2      Identify a new mask\n") ;
	fprintf (stdout,"  RETURN   Exit program\n") ;

	fprintf (stdout,"\n> ") ;
        fflush(stdout);

	if (!G_gets(buf)) continue ;

	G_strip(buf);
	if (strcmp (buf,"1") == 0)
	{
	    if (system ("g.remove rast=MASK") == 0) {
	    	sleep (1);
	    } else {
	    	sprintf(buf, "%s: Error removing MASK", G_program_name());
	    	G_fatal_error(buf);
	    	exit(1);
	    }
	}
	else if (strcmp (buf,"2") == 0)
	    makemask() ;
	else if (*buf == 0)
	    exit(0) ;
	else
	    continue;

	title();
    }
}

static int title(void)
{
    G_clear_screen() ;
    fprintf (stdout,"MASK:  Program for managing current GIS mask\n\n") ;
    maskinfo();

    return 0;
}

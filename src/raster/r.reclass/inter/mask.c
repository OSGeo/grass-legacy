/*
 * MASK allows the user to:
 *    Remove the current mask
 *    Change the current mask
 */

#include "gis.h"

main(argc, argv) char *argv[];
{
    char buf[80]  ;

/* Initialize GIS routines ****************************************************/
    G_gisinit(argv[0]) ;

/* Dump the advertising *******************************************************/
    title();

/* Check to see if user wants to remove or change the current mask */
    for(;;)
    {
	printf("\n\nOptions:\n") ;
	printf("    1      Remove the current mask\n") ;
	printf("    2      Identify a new mask\n") ;
	printf("  RETURN   Exit program\n") ;

	printf("\n> ") ;
	if (!G_gets(buf)) continue ;

	G_strip(buf);
	if (strcmp (buf,"1") == 0)
	{
	    system ("g.remove rast=MASK");
	    sleep (3);
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
static
title()
{
    G_clear_screen() ;
    printf("MASK:  Program for managing current GIS mask\n\n") ;
    maskinfo();
}

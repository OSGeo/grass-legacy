#include <stdio.h>
#include <unistd.h>
#include "imagery.h"
/********************************************************/
/* NAME:	get_tapename				*/
/*							*/
/* FUNCTION:	set up the name of the device that user */
/*		want to use				*/
/*							*/
/* USAGE:	get_tapename(name)			*/
/*							*/
/* INPUT:	"*name" as string of tape device name	*/
/*							*/
/* OUTPUT:	none					*/
/********************************************************/
int get_tapename (char *name)
{
    for(;;)
    {
	if (!I_ask ("enter tape device name: ", name, 1))
	    exit(0);
	if (access (name, 4) == 0)
		return 1;
	fprintf (stderr, "%s - ",name);
	if (access (name, 0) != 0)
	    fprintf (stderr, "no such device\n");
	else
	    fprintf (stderr, "read permission denied\n");
    }
}

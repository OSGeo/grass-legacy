/* %W% %G% */
/**************************************************************/
/*                                                            */
/*     stash.c           in    ~/src/Ginterp                  */
/*                                                            */
/*     This routine assigns the values of the command line    */
/*     arguments to the appropriate variables depending       */
/*     on the positions occupied by those arguments on the    */
/*     command line.                                          */
/*                                                            */
/**************************************************************/

#include "stash.h"

stash(n, s)
char *s;
{
    switch(n) {
        case INPUT:
	    strcpy (input, s);
	    break;
        case OUTPUT:
	    strcpy (output, s);
	    break;
        case GOAL:
            if (sscanf(s,"%d", &goal) != 1 || goal <= 0) {
            	fprintf (stderr, "number of points must be positive\n");
            	return -1;
        	}
            break;
        default:
	    return -1;
        }
    return 0;
}

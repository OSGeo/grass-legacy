/* Function: Raster_chr		P.W. Carlson		1/90 	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Raster_char(num, nrows, array, withzeros, color_type)
int num, nrows, withzeros, color_type;
unsigned char *array;
{
    register int i;
    register unsigned char *rptr, *aptr;
    unsigned char buff[640];

    /* if overlaying... */
    if (!withzeros)
    {	
	/* one row at a time */
	while (nrows--)
	{   
	    /* get the display raster into the buffer */
	    getraster(current.y, buff);

	    /* overlay the buffer with non-zero values */
    	    rptr = buff + current.x;
    	    aptr = array;
	    if (color_type)
    	    {	for (i = 0; i < num; i++, rptr++, aptr++) 
	            if (*aptr) 
		    	*rptr = (unsigned char)_get_color_index((int)(*aptr));
            }
	    else
            {	for (i = 0; i < num; i++, rptr++, aptr++) 
	            if (*aptr) 
		    	*rptr = *aptr;
            }

	    /* display the buffer */
	    putraster(current.x, current.y, current.x + num - 1, 
			current.y, buff + current.x);
	    current.y++;
	}
    }

    /* if not overlaying... */
    else
    {	
	/* get the data into the buffer */
	rptr = buff;
	aptr = array;
	if (color_type)
       	{   for (i = 0; i < num; i++, rptr++, aptr++) 
		*rptr = (unsigned char)_get_color_index((int)(*aptr));
	    putraster(current.x, current.y, current.x + num - 1,
		current.y + nrows - 1, buff);
	}
	else 
	    putraster(current.x, current.y, current.x + num - 1,
		current.y + nrows - 1, array);
    }
}

/* Function: Raster_chr		P.W. Carlson		5/89 	*/

#include "ega_io.h"

Raster_char(num, nrows, array, withzeros, color_type)
int num, nrows, withzeros, color_type;
unsigned char *array;
{
    register int i;
    register unsigned char *rptr, *aptr;

    /* if overlaying... */
    if (!withzeros)
    {	
	/* one row at a time */
	while (nrows--)
	{   
	    /* overlay the buffer with non-zero values */
    	    rptr = raster_buff;
    	    aptr = array;
	    if (color_type)
    	    {	for (i = 0; i < num; i++, rptr++, aptr++) 
	        {   if (*aptr) 
		    	*rptr = (unsigned char)_get_color_index((int)(*aptr));
		    else *rptr = 255;
		}
            }
	    else
            {	for (i = 0; i < num; i++, rptr++, aptr++) 
	        {   if (*aptr) 
		    	*rptr = *aptr;
		    else *rptr = 255;
		}
            }

	    /* display the buffer */
    	    args.arg1 = cur_x;
    	    args.arg2 = cur_x + num - 1;
    	    args.arg3 = cur_y;
   	    args.ptr1  = raster_buff;
    	    ioctl(egafd, EGA_DITHOVER, &args);
	    cur_y++;
	}
    }

    /* if not overlaying... */
    else
    {	
	/* get the data into the buffer */
	rptr = raster_buff;
	aptr = array;
    	args.arg1 = cur_x;
    	args.arg2 = cur_y;
    	args.arg3 = cur_x + num - 1;
    	args.arg4 = cur_y + nrows - 1;
	if (color_type)
       	{   for (i = 0; i < num; i++, rptr++, aptr++) 
		*rptr = (unsigned char)_get_color_index((int)(*aptr));
    	    args.ptr1  = raster_buff;
	}
	else args.ptr1  = array;
    	ioctl(egafd, EGA_PUTDITH, &args);
    }
}

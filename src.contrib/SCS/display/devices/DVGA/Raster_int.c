/* Function: Raster_int		P.W. Carlson		12/88	*/

#include "vio_driver.h"

Raster_int(num, nrows, array, withzeros, color_type)
int num, nrows, withzeros, color_type;
unsigned int *array;
{
    register int i;
    register unsigned char *rptr;
    register unsigned int *aptr;

    /* if overlaying... */
    if (!withzeros)
    {	
	/* one row at a time */
	while (nrows--)
	{   
	    /* get the display raster into the buffer */
	    args.arg1 = cur_x;
    	    args.arg2 = cur_x + num - 1;
    	    args.arg3 = cur_y;
    	    args.ptr  = raster_buff;
    	    ioctl(viofd, VIO_GETRAST, &args);

	    /* overlay the buffer with non-zero values */
    	    rptr = raster_buff;
    	    aptr = array;
	    if (color_type)
            {	for (i = 0; i < num; i++, rptr++, aptr++) 
	            if (*aptr) 
		    	*rptr = (unsigned char)_get_color_index(*aptr);
            }
	    else
            {	for (i = 0; i < num; i++, rptr++, aptr++) 
	            if (*aptr) 
		    	*rptr = (unsigned char)(*aptr);
            }

	    /* display the buffer */
    	    args.arg1 = cur_x;
    	    args.arg2 = cur_y;
    	    args.arg3 = cur_x + num - 1;
    	    args.arg4 = cur_y;
   	    args.ptr  = raster_buff;
    	    ioctl(viofd, VIO_PUTRAST, &args);
	    cur_y++;
	}
    }

    /* if not overlaying... */
    else
    {	
	/* get the data into the buffer */
	rptr = raster_buff;
	aptr = array;
	if (color_type)
       	    for (i = 0; i < num; i++, rptr++, aptr++) 
		*rptr = (unsigned char)_get_color_index(*aptr);
	else
    	    for (i = 0; i < num; i++, rptr++, aptr++) 
		*rptr = (unsigned char)(*aptr);

	/* display the buffer nrows times */
    	args.arg1 = cur_x;
    	args.arg2 = cur_y;
    	args.arg3 = cur_x + num - 1;
    	args.arg4 = cur_y + nrows - 1;
    	args.ptr  = raster_buff;
    	ioctl(viofd, VIO_PUTRAST, &args);
    }
}

/* Function: Text		P.W. Carlson		5/89	*/
#include "ega_io.h"

Text(text)
char *text;
{
    /* if text does not start with @, use stroked font */
    if (*text != '@') 
	soft_text(cur_x, cur_y, _text_size_x, _text_size_y, 
	    _text_rotation, text);
    else 

    /* use bitmapped characters */
    {	text++;

	/* pass the data to the device driver */
    	args.arg1 = cur_x;
    	args.arg2 = cur_y + 1;
    	args.arg3 = strlen(text);
    	args.ptr1 = (unsigned char *)text;
    	ioctl(egafd, EGA_BITTEXT, &args);
    }
} 

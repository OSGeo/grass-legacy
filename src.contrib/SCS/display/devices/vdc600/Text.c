/* Function: Text		P.W. Carlson		1/90	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Text(text)
char *text;
{
    /* if text does not start with @, use stroked font */
    if (*text != '@') 
	soft_text(current.x, current.y, _text_size_x, _text_size_y, 
	    _text_rotation, text);
    else 

    /* use bitmapped characters */
    {	text++;
    	bit_text(current.x, current.y + 1, text);
    }
} 

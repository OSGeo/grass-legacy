/* Function: bit_text
**
** Author: Paul W. Carlson		Jan. 1990
*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"
#include "charmap.h"

void bit_text(x, y, text) 
register int x, y; 
char *text; 
{
    int pat_off, char_num, col, row, leng, x_off;
    unsigned char *ptr; 

    leng = strlen(text);
 
    y -= 13;
    for (char_num = 0; char_num < leng; char_num++) 
    {   pat_off = text[char_num] * 14; 
        ptr = char_map + pat_off; 
        for (col = 0; col < 8; col++) 
	{   x_off = x + col;
            for (row = 0; row < 14; row++) 
                if ((*(ptr+row)) & (bit_val[col])) 
    		{   video.total_offset = H_RES * (y + row) + x_off;
		    CHECK_SEG();
	    	    *(graphics_base + video.segment.offset) = current.color;
		}
        } 
        x += 8; 
    } 
} 

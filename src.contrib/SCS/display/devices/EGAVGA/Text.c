/* Function: Text	P.W. Carlson		April 1990	*/

#include <stdio.h>
#include "driver.h"

Text(text)
char *text ;
{
    int leng;

    if (*text != '@') soft_text(cur_x, cur_y, 
		_text_size_x, _text_size_y, _text_rotation, text) ;
    else
    {	put_chr('T');
    	put_int(cur_x);
    	put_int(cur_y + 1);
    	leng = strlen(text+1);
    	put_int(leng);
    	fwrite(text+1, 1, leng, stdout);
    	fflush(stdout);
    }
}

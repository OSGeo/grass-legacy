/*---------- Function: get_int ----------*/
#include <stdio.h>
get_int()
{
    	unsigned char c1, c2;
    	char buf[5];
    	int num;

	gets(buf);
	c1 = buf[0];
	c2 = buf[1];
	num = (c1 & 31) | ((c2 & 31) << 5);
    	return(num);
}

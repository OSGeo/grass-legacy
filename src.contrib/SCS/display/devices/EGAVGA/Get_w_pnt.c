/* Function: Get_location_with_pointer	Paul W. Carlson 	April 1990  */
#include <stdio.h>

Get_location_with_pointer(wx, wy, button)
int *wx, *wy, *button;
{
    char c, c1, c2, buf[10], *gets();

    put_chr('B');
    put_int(*wx);
    put_int(*wy);

    gets(buf);
    c1 = buf[0];
    c2 = buf[1];
    *wx = (c1 & 31) | ((c2 & 31) << 5);
    c1 = buf[2];
    c2 = buf[3];
    *wy = (c1 & 31) | ((c2 & 31) << 5);
    c = buf[4];
    switch (c)
    {	case 'l': *button = 1;  break;
    	case 'm': *button = 2;  break;
    	case 'r': *button = 3;  break;
    }
}

#include "P.h"

Pdata (buf, n)
    unsigned char *buf;
{
	unsigned char value;
	sprintf(buffer,"*b%dW", n*2);
	esc(buffer);
	while (n-- > 0)  { 
		value = *buf++;
		sprintf(buffer,"%c%c",0x00,value);
		Pout(buffer,2);
	}
}

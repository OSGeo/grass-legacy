#include "P.h"
#include "Paintlib.h"
#include "local_proto.h"

int Prle (unsigned char *buf, int n)
{
    int *d;
    int i;
    unsigned char value, repeat;

    d = data;
    i = 0;
    while (n-- > 0)
    {
	repeat = *buf++;
	value = *buf++;
	while (repeat-- > 0)
	{
	    *d++ = value;
	    i++;
	}
    }
    send_data (i);

    return 0;
}

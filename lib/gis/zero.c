#include <string.h>
#include "gis.h"
/****************************************************************
 *  G_zero (buf, i)
 *     char *buf           buffer to be zeroed
 *     int i               number of bytes to be zeroed
 *
 *  Zeros out a buffer to 'i' bytes
 ****************************************************************/

int G_zero ( register void *buf , register int i )
{
	memset(buf,0,i);

    return 0;
}

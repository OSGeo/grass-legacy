#include <string.h>
#include <stdlib.h>
#include "local_proto.h"

int center (char *buf, int width)
{
	char *temp;
	char *t;
	int lead;

	lead = (width - strlen(buf))/2;
	if (lead <= 0)
		return 0;
	
	temp = xalloc (width+1);
	for (t = temp; lead-- > 0; t++)
		*t = ' ';
	strcpy (t, buf);
	strcpy (buf, temp);
	free (temp);

	return 0;
}

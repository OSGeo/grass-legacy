#include <string.h>
#include <stdio.h>

int inform (char *msg)
{
    static int len = 0;
    if (msg)
    {
	len += strlen (msg);
	fprintf (stdout,"%s", msg);
	fflush (stdout);
    }
    else
    {
	fprintf (stdout,"%*s", 45-len, "");
	len = 0;
    }

    return 0;
}

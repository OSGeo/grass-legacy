#include <stdio.h>
inform (msg)
    char *msg;
{
    static int len = 0;
    if (msg)
    {
	len += strlen (msg);
	printf ("%s", msg);
	fflush (stdout);
    }
    else
    {
	printf ("%*s", 45-len, "");
	len = 0;
    }
}

#include <string.h>
#include <stdio.h>

static int backsp = 0;

int counter_reset (char *msg, int before)
{
    backsp = 0;
    fprintf (stdout,"%s", msg);
    if (before)
	while (*msg++)
	    fprintf (stdout,"\b");
    fflush (stdout);

    return 0;
}

int counter (int n)
{
    char buf[30];
    int len;
    int extra;


    sprintf (buf, "%d", n);
    len = strlen (buf);
    extra = backsp - len;

    while(backsp-- > 0)
        fprintf (stdout,"\b");
    backsp = len;

    fprintf (stdout,"%s", buf);

    for (len = extra; len > 0; len--)
        fprintf (stdout," ");
    for (len = extra; len > 0; len--)
        fprintf (stdout,"\b");
    fflush (stdout);

    return 0;
}

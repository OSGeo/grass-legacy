#include <stdio.h>

static int backsp = 0;

counter_reset (msg, before)
    char *msg;
{
    backsp = 0;
    printf("%s", msg);
    if (before)
	while (*msg++)
	    printf("\b");
    fflush (stdout);
}

counter(n)
{
    char buf[30];
    int len;
    int extra;


    sprintf (buf, "%d", n);
    len = strlen (buf);
    extra = backsp - len;

    while(backsp-- > 0)
        printf("\b");
    backsp = len;

    printf ("%s", buf);

    for (len = extra; len > 0; len--)
        printf (" ");
    for (len = extra; len > 0; len--)
        printf ("\b");
    fflush (stdout);
}

#include <stdio.h>
extern FILE *tracefd;

error (a,b,c)
    char *a, *b, *c;
{
    char msg[2000];

    sprintf (msg, "%s%s%s : %s", a, *b?" ":"",b, c);

    if (tracefd != NULL
    && !(isatty(fileno(tracefd)) && isatty(fileno(stderr))))
	fprintf (tracefd, "## error: %s\n", c);

    if (isatty(0))
	fprintf (stderr, "%s\n", msg);
    else
	G_fatal_error (msg);
    reject();
}

/* @(#)adate.c	2.1   6/26/87 */

#include <time.h>
main(argc, argv) char *argv[];
{
    int i;
    long clock;
    char *adate();

    for (i = 1; i < argc; i++)
    {
	sscanf (argv[i], "%ld", &clock);
	printf ("%s = %s\n", argv[i], adate(clock));
    }
}

char *
adate(clock)
    long clock;
{
    struct tm *local;
    char *date;
    char *d;

    char *asctime();
    struct tm *localtime();


    local = localtime(&clock);
    date = asctime(local);
    for (d = date; *d; d++)
	    if (*d == '\n')
		    *d = 0;
    return date;
}

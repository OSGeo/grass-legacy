#include <stdio.h>
#include <time.h>

static  long watch;

int start_clock (long *save)
{
    if (save != NULL)
	*save = time (NULL);
    else
	watch = time (NULL);

    return 0;
}

int stop_clock (long *save)
{
    register int mins, secs;
    long stop, diff;

    stop = time (NULL);

    if (save != NULL)
	diff = stop - *save;
    else
	diff = stop - watch;

    mins = diff / 60;
    secs = diff  % 60;
    fprintf (stdout," %3d min%c %02d sec%c\n", 
	mins, mins == 1 ? ' ' : 's', secs, secs == 1 ? ' ' : 's');

    return 0;
}

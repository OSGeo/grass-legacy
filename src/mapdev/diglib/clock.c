#include <stdio.h>
#include <time.h>

static  long watch;
long time();

dig_start_clock (save)
    long *save;
{
    if (save != NULL)
	*save = time (NULL);
    else
	watch = time (NULL);
}

dig_stop_clock (save)
    long *save;
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
    printf (" %3d min%c %02d sec%c\n", 
	mins, mins == 1 ? ' ' : 's', secs, secs == 1 ? ' ' : 's');
}

char *
dig_stop_clock_str (save)
    long *save;
{
    register int mins, secs;
    long stop, diff;
    static char buf[200];

    stop = time (NULL);

    if (save != NULL)
	diff = stop - *save;
    else
	diff = stop - watch;

    mins = diff / 60;
    secs = diff  % 60;
    sprintf (buf, " %3d min%c %02d sec%c", 
	mins, mins == 1 ? ' ' : 's', secs, secs == 1 ? ' ' : 's');

    return buf;
}

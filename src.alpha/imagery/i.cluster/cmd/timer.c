#include <stdio.h>
print_time(t)
    long t;
{
    int H, M, S;

    if (t < 0) t = 0;

    H = t / 3600;
    t -= H*3600;
    M  = t / 60;
    S = t - M*60;

    if (H)
    {
	fprintf (stderr, "%dh%02dm%02ds", H, M, S);
    }
    else if (M)
    {
	fprintf (stderr, "%dm%02ds", M, S);
    }
    else
    {
	fprintf (stderr, "%ds", S);
    }
    fflush (stderr);
}

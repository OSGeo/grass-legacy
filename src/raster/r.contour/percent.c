#include <stdio.h>

int print_level (int value, int curr, int total)
{

    fprintf (stderr,"Current level is %4d  %3d out of %4d\37b",
				  value, curr, total);
    fflush (stderr);

    return 0;
}

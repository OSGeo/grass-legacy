#include <stdio.h>


print_level (value, curr, total)
{

    fprintf (stderr,"Current level is %4d  %3d out of %4d\37b",
				  value, curr, total);
    fflush (stderr);
}

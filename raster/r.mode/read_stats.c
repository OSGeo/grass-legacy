#include <stdlib.h>
#include <stdio.h>

int 
read_stats (FILE *fd, long *cat1, long *cat2, double *value)
{
    char buf[1024];

    if (fgets(buf, sizeof buf, fd) == NULL) return 0;

    if (sscanf (buf, "%ld %ld %lf", cat1, cat2, value) == 3)
	    return 1;
    
    fprintf (stderr, "ERROR reading r.stats output\n");
    exit(1);
}

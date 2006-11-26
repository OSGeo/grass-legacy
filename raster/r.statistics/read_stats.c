#include <stdlib.h>
#include <stdio.h>
#include <grass/glocale.h>

int 
read_stats (FILE *fd, long *cat1, long *cat2, double *value)
{
    char buf[1024];

    if (fgets(buf, sizeof buf, fd) == NULL) return 0;

    
    if (sscanf (buf, "%ld %ld %lf", cat1, cat2, value) == 3)
    {
/*      fprintf(stdout,"base: %ld  cover: %ld  val: %lf\n", *cat1, *cat2, *value);
/**/
      return 1;
    }

    
    G_fatal_error (_("Reading r.stats output"));
    exit(EXIT_FAILURE);
}

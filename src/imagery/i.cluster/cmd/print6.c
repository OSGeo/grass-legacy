#include "imagery.h"
print_distribution (fd,C)
    FILE *fd;
    struct Cluster *C;
{
    int cat;

    fprintf (fd, "class distribution");
    for (cat = 0; cat < C->nclasses; cat++)
    {
	fprintf (fd, "%s %10ld",cat%5?"":"\n",(long) C->count[cat]);
    }
    fprintf (fd, "\n");
}

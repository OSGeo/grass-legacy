#include "glob.h"

static char **vectorfiles = NULL;

char *
get_barrier_name(i)
    int i;
{
    if (vectorfiles)
	return vectorfiles[i];
    return NULL;
}

void
init_barriers (names)
    char **names;
{
    int i;
    char *nm;

    vectorfiles = names;

/* make sure that all the vector files are there */
    for (i = 0; nm = get_barrier_name(i); i++)
	find_vector(nm);
}


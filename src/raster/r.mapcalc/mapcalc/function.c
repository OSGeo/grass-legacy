#include <stdio.h>
#include <string.h>
#include "function.h"


int 
find_function (char *name, int *n)
{
    char *fname;
    for (*n = 0; (fname = function_list[*n].name) && *fname; (*n)++)
	if (strcmp (name, fname) == 0)
	    return 1;
    return 0;
}

int 
print_function_names (void)
{
    int n;
    char *fname;
    fprintf (stderr, "known functions:");
    for (n = 0; (fname = function_list[n].name) && *fname; n++)
	fprintf (stderr, "%s%-10s", n%7?" ":"\n",fname);
    fprintf (stderr, "\n");

    return 0;
}

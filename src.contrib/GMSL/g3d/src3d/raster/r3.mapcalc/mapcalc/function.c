#include <stdio.h>
#include "function.h"


find_function (name, n)
    char *name;
    int *n;
{
    char *fname;
    for (*n = 0; (fname = function_list[*n].name) && *fname; (*n)++)
	if (strcmp (name, fname) == 0)
	    return 1;
    return 0;
}

print_function_names()
{
    int n;
    char *fname;
    fprintf (stderr, "known functions:");
    for (n = 0; (fname = function_list[n].name) && *fname; n++)
	fprintf (stderr, "%s%-10s", n%7?" ":"\n",fname);
    fprintf (stderr, "\n");
}

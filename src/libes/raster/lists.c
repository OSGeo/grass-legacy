#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "raster.h"

int _get_list ( char ***list, int *count)
{
    char **a;
    int n;
    char *buf;

    *list = NULL;
    *count = 0;

    buf = _get_text_2 ();
    for (n = 0; *buf != 0 ; n++)
    {
	if (n == 0)
	    a = (char **) malloc (sizeof(char *));
	else
	    a = (char **) realloc (a, (n+1) * sizeof(char *));
	if (a == NULL)
	{
	    fprintf (stderr, "out of memory");
	    return 0;
	}
	a[n] = strdup(buf);
	if (a[n] == NULL)
	{
	    fprintf (stderr, "out of memory");
	    return 0;
	}
	buf = _get_text_2 ();
    }

    *list = a ;
    *count = n;

    return 1;
}

int R_pad_freelist ( char **list, int count)
{
    int i;

    if (count > 0)
    {
	for (i = 0; i < count; i++)
	    free (list[i]);
	free (list);
    }

    return 0;
}

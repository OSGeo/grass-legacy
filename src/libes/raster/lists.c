
#include <stdio.h>

_get_list (list, count)
    char ***list;
    int *count;
{
    char *malloc(), *realloc();

    char **a;
    int n;
    char buf[1024];

    *list = NULL;
    *count = 0;

    _get_text (buf);
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
	a[n] = malloc (strlen(buf) + 1);
	if (a[n] == NULL)
	{
	    fprintf (stderr, "out of memory");
	    return 0;
	}
	strcpy (a[n], buf);
	_get_text (buf);
    }

    *list = a ;
    *count = n;

    return 1;
}

R_pad_freelist (list, count)
    char **list;
    int count;
{
    int i;

    if (count > 0)
    {
	for (i = 0; i < count; i++)
	    free (list[i]);
	free (list);
    }
}

#include <stdio.h>
int 
divider (FILE *out)
{
    int i;

    fprintf (out, ".divider\n");
    fprintf (out, "\n");
    for (i = 0; i < 79; i++)
	fprintf (out, "=");
    fprintf (out, "\n\n");
    fprintf (out, ".end\n");

    return 0;
}

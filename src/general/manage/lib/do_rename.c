#include "list.h"

int do_rename (int n, char *old, char *new)
{
    int i;
    int len;

    fprintf (stdout,"RENAME [%s] to [%s]\n", old, new);
    if (strcmp (old,new) == 0) return 1;

    len = get_description_len(n);

    hold_signals(1);
    for (i = 0; i < list[n].nelem; i++)
    {
	fprintf (stdout," %-*s ", len, list[n].desc[i]);
	fflush (stdout);

	G_remove(list[n].element[i], new);
	switch (G_rename (list[n].element[i], old, new))
	{
	case -1: fprintf (stdout,"COULD NOT RENAME"); break;
	case  0: fprintf (stdout,"MISSING"); break;
	}
	fprintf (stdout,"\n");
    }
    if (strcmp (list[n].element[0], "cell") == 0)
    {
	char colr2[50];
	sprintf (colr2, "colr2/%s", G_mapset());
	G_remove (colr2, new);
	G_rename (colr2, old, new);
    }
    hold_signals(0);

    return 0;
}

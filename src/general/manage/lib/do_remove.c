#include "list.h"
do_remove(n,old)
    char *old;
{
    int i;
    int len;

    printf ("REMOVE [%s]\n", old);

    len = get_description_len(n);

    hold_signals(1);
    for (i = 0; i < list[n].nelem; i++)
    {
	printf (" %-*s ", len, list[n].desc[i]);
	fflush (stdout);

	switch (G_remove (list[n].element[i], old))
	{
	case -1: printf ("COULD NOT REMOVE"); break;
	case  0: printf ("MISSING"); break;
	}
	printf("\n");
    }
    if (strcmp (list[n].element[0], "cell") == 0)
    {
	char colr2[50];
	sprintf (colr2, "colr2/%s", G_mapset());
	G_remove (colr2, old);
    }
    hold_signals(0);
}

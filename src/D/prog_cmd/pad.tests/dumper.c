/* @(#)dumper.c	2.1   6/26/87 */

main()
{
    char **pads;
    char **items;
    char **list;
    int npads;
    int nitems;
    int count;
    int p;
    int i;
    int n ;
    int stat ;

    R_open_driver();

    R_pad_list (&pads, &npads);
    for (p = -1; p < npads; p++)
    {
	printf ("PAD: ");
	if (p < 0)
	{
	    printf ("") ;
	    stat = R_pad_select ("");
	}
	else
	{
	    printf ("%s", pads[p]);
	    stat = R_pad_select (pads[p]);
	}
	printf ("\n");

	if (stat)
	{
	    R_pad_perror ("    ERROR", stat);
	    continue;
	}

	stat = R_pad_list_items (&items, &nitems);
	if (stat)
	{
	    R_pad_perror ("    ERROR", stat);
	    continue;
	}

	for (i = 0; i < nitems; i++)
	{
	    printf ("    ITEM: %s\n", items[i]);
	    stat = R_pad_get_item (items[i], &list, &count);
	    if (stat)
	    {
		R_pad_perror ("          ERROR", stat);
		continue;
	    }
	    for (n = 0; n < count; n++)
		printf ("          %s\n", list[n]);
	    R_pad_freelist (list,count);
	}
    }

    R_close_driver();
}

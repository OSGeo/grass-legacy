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
	if (p < 0)
	{
	    printf ("SCREEN STATUS:\n");
	    stat = R_pad_select ("");
	}
	else
	{
	    printf ("FRAME: %s\n", pads[p]);
	    stat = R_pad_select (pads[p]);
	}

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
	    printf ("    %8s:", items[i]);
	    stat = R_pad_get_item (items[i], &list, &count);
	    if (stat)
	    {
		R_pad_perror ("          ERROR", stat);
		continue;
	    }
	    for (n = 0; n < count; n++)
	    {
		if (n == 0)
		    printf ("%s\n", list[n]);
		else
		    printf ("             %s\n", list[n]);
	    }
	    R_pad_freelist (list,count);
	}
    }

    R_close_driver();
    exit(0);
}

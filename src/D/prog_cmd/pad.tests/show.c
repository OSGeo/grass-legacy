/* @(#)show.c	2.1   6/26/87 */

main(argc,argv) char *argv[];
{
    char **list;
    int count;
    int n ;
    int i;
    int stat ;

    R_open_driver();

    if (argc == 1)
    {
	R_pad_list (&list, &count);
	for (i = 0; i < count; i++)
	    printf ("%s\n", list[i]);
    }
    else
    {
	stat = R_pad_select (argv[1]);
	if (stat)
	    R_pad_perror (argv[1]);
	else if (argc == 2)
	{
	    stat = R_pad_list_items (&list, &count);
	    if (stat)
		R_pad_perror (argv[1]);
	    else
	    {
		for (i = 0; i < count; i++)
		    printf ("%s\n", list[i]);
	    }
	}
	else
	{
	    for (n = 2; n < argc; n++)
	    {
		stat = R_pad_get_item (argv[n], &list, &count);
		if (stat)
		    R_pad_perror (argv[n], stat);
		else
		{
		    for (i = 0; i < count; i++)
			printf ("%s\n", list[i]);
		    R_pad_freelist (list,count);
		}
	    }
	}
    }

    R_close_driver();
}

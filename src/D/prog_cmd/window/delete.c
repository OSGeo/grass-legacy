/*  %W%  %G%  */

main(argc, argv) char *argv[];
{
    int stat;
    int i;

    if (argc < 2)
	D_usage (argv[0], "pad [item]");

    R_open_driver();

    stat = R_pad_select (argv[1]);
    if (stat)
	R_pad_perror (argv[1], stat);
    else if (argc == 2)
    {
	stat = R_pad_delete ();
	if (stat)
	    R_pad_perror (argv[1], stat);
	else
	    printf ("pad %s deleted\n", argv[1]);
    }
    else
	for (i = 2; i < argc; i++)
	{
	    stat = R_pad_delete_Item (argv[i]);
	    if (stat)
		R_pad_perror (argv[i], stat);
	    else
		printf ("item %s deleted\n", argv[i]);
	}

    R_close_driver();
}

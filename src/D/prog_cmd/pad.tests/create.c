/* @(#)create.c	2.1   6/26/87 */

main(argc, argv) char *argv[];
{
    int i;
    int stat;
    char name[100];

    if (argc > 2)
	D_usage (argv[0], "[pad]");

    R_open_driver();

    if (argc == 1)
    {
	R_pad_invent (name);
	argc = 2;
	argv[1] = name;
    }
    for (i = 1; i < argc; i++)
    {
	stat = R_pad_create (argv[i]);
	if (stat)
	    R_pad_perror (argv[i], stat);
	else
	    printf ("%s created\n", argv[i]);
    }

    R_close_driver();
}

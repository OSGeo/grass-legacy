/* @(#)append.c	2.1   6/26/87 */

main(argc,argv) char *argv[];
{
    int stat;
    int i;
    char buf[1024];

    if (argc < 4)
	D_usage (argv[0], "%s pad item value");

    *buf = 0;
    for (i = 3; i < argc; i++)
    {
	if (*buf) strcat (buf, " ");
	strcat (buf, argv[i]);
    }

    R_open_driver();

    stat = R_pad_select (argv[1]);
    if (stat)
	R_pad_perror (argv[1], stat);
    else
    {
	stat = R_pad_append_item (argv[2], buf);
	if (stat)
	    R_pad_perror (argv[2], stat);
    }

    R_close_driver();
}

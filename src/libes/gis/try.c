main(argc, argv) char *argv[];
{
int i;
	for (i = 1; i < argc; i++)
	{
	    printf ("%s=>", argv[i]);
	    G_tolcase(argv[i]);
	    printf ("%s\n", argv[i]);
	    }}

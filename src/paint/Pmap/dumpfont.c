main(argc, argv) char *argv[];
{
    int i,j,k;
    char c;
    unsigned char *X, *Y;
    int n;

    select_font (argv[1]);

    for (i = 2; i < argc; i++)
    {
	for (j = 0; c = argv[i][j]; j++)
	{
	    printf ("[%c]\n", c);
	    get_font_char (c, &n, &X, &Y);
	    printf (" %d vectors\n",n);
	    for (k = 1; k < n; k++)
	    {
		printf ("  %d:(%d,%d)=(%c,%c)->(%d,%d)\n", k,
			X[k], Y[k], X[k], Y[k],
			10 + X[k] - 'R',
			10 - Y[k] + 'R');
	    }
	    printf ("  move:(%d,%d)=(%c,%c)->(%d,%d)\n",
			X[k], Y[k], X[k], Y[k],
			10 + X[k] - 'R',
			10 - Y[k] + 'R');
	}
    }
}

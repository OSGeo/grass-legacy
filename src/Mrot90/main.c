#include <stdio.h>

main(argc,argv) char *argv[];
{
    char infile[500];
    char outfile[500];
    int in,out;
    int i;

    static int verbose = 0;
    static int nrows = 0;
    static int ncols = 0;
    static int bpc = 0;
    static if_flag = 0;
    static of_flag = 0;
    static row_flag = 0;
    static col_flag = 0;
    static bpc_flag = 0;

    for (i = 1; i < argc; i++)
    {
	if (strcmp (argv[i], "-v") == 0)
	{
	    verbose = 1;
	    continue;
	}
	if (sscanf (argv[i], "if=%s", infile) == 1)
	{
	    if (if_flag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i], "of=%s", outfile) == 1)
	{
	    if (of_flag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i], "rows=%d", &nrows) == 1)
	{
	    if (row_flag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i], "cols=%d", &ncols) == 1)
	{
	    if (col_flag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i], "bpc=%d", &bpc) == 1)
	{
	    if (bpc_flag++) usage(argv[0]);
	    continue;
	}
	usage(argv[0]);
    }
    if (!if_flag || !of_flag || !bpc_flag)
	usage(argv[0]);
    if (nrows <= 0 || ncols <= 0 || bpc <= 0)
	usage(argv[0]);

/* output file must not exist */
    if (access (outfile,0) == 0)
    {
	fprintf (stderr, "%s: %s - file exists. Sorry!\n", argv[0], outfile);
	exit(1);
    }

/* open the files */
    in = open (infile,0);
    if (in < 0)
    {
	perror (infile);
	exit(1);
    }
    out = creat (outfile, 0666);
    if (out < 0)
    {
	perror (outfile);
	exit(1);
    }

/* do the rotation */
    rotate (in, out, nrows, ncols, bpc, verbose);

/* close the files and leave */
    close(in);
    close(out);

    exit(0);
}

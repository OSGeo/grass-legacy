#include <stdio.h>

char *pgm;

main(argc, argv) char *argv[];
{
    int irows, icols, max;
    int nrows, ncols;
    int prows, pcols;
    unsigned char *red, *grn, *blu;
    extern char *xalloc();
    char *input;
    int force;
    int toowide;
    int toolong;
    int die;

    pgm = "paint.ppm";
    force = 0;
    die = 0;

    if (argc > 1 && strcmp (argv[1],"-f") == 0)
    {
	force = 1;
	argc--;
	argv++;
    }
    if (argc > 1)
    {
	if( NULL == freopen (input = argv[1], "r", stdin))
	{
	    fprintf (stderr, "\n%s - ", pgm);
	    perror (input);
	    exit(1);
	}
    }
    else
    {
	input = NULL;
	if (isatty(0))
	{
	    fprintf (stderr, "\nUsage: %s [-f] ppm_file\n", pgm);
	    fprintf (stderr, "  or   %s [-f] <ppm_file\n", pgm);
	    exit(1);
	}
    }

    if (!header (&irows, &icols, &max))
    {
	fprintf (stderr, "\n%s - %s not in PPM format\n",
	    pgm, input ? input : "input");
	exit(1);
    }
printf ("input  cols %d, rows %d, max color level %d\n", icols, irows, max);

    red = (unsigned char *)xalloc (ncols);
    grn = (unsigned char *)xalloc (ncols);
    blu = (unsigned char *)xalloc (ncols);

/* connect to the paint driver and get printer width */
    Pconnect();
    Popen();
    Pnpixels (&prows, &pcols);
    printf ("output cols=%d", pcols);
    if (prows)
	printf (", rows=%d", prows);
    printf ("\n");

    if (prows && irows > prows)
    {
	toolong = 1;
	nrows = prows;
    }
    else
    {
	toolong = 0;
	nrows = irows;
    }


    if (icols > pcols)
    {
	toowide = 1;
	ncols = pcols;
    }
    else
    {
	toowide = 0;
	ncols = icols;
    }

    if (toolong || toowide)
    {
	double x,y;

	x = (double)pcols/(double)icols;
	y = (double)prows/(double)irows;

	fprintf (stderr, "\n%s - %s: image is ", pgm,force?"NOTE":"ERROR");
	if (toolong && toowide)
	{
	    if (x>y) y = x;
	    fprintf (stderr, "too long and too wide\n\n");
	}
	}
	else if (toowide)
	{
	    fprintf (stderr, "too wide\n\n");
	    y = x;
	}
	else if (toolong)
	{
	    fprintf (stderr, "too long\n\n");
	}
	fprintf (stderr, "This may be corrected by:\n");
	fprintf (stderr, "  scaling the image %g using ppmscale\n", y);
	if ((!prows || (prows && icols <= prows)) && irows <= pcols)
	    fprintf (stderr, "  or by rotating the image using ppmrotate\n");
	if (force)
	    fprintf (stderr, "\nImage will be printed, but clipped\n");
	else 
	{
	    fprintf (stderr, "  or use -f option to force printing\n");
	    die = 1;
	}
    }

    if (!die)
    {
	begin_paint(nrows, ncols);
	while (nrows-- > 0)
	{
	    readcolors (red, grn, blu, icols, max);
	    paint(red, grn, blu, nrows, ncols);
	}
    }
    Pclose();
    Pdisconnect();
    exit(die);
}

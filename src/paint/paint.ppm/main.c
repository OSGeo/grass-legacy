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
    double x,y,scale;

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

    red = (unsigned char *)xalloc (icols);
    grn = (unsigned char *)xalloc (icols);
    blu = (unsigned char *)xalloc (icols);

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
	x = (double)pcols/(double)icols;
	y = (double)prows/(double)irows;

	fprintf (stderr, "\n%s - %s: image is ", pgm,force?"NOTE":"ERROR");
	if (toolong && toowide)
	{
	    fprintf (stderr, "too long and too wide\n\n");
	    scale = x>y?y:x;
	}
	else if (toowide)
	{
	    fprintf (stderr, "too wide\n\n");
	    scale = x;
	}
	else if (toolong)
	{
	    fprintf (stderr, "too long\n\n");
	    scale = y;
	}
	fprintf (stderr, "This may be corrected by:\n");
	fprintf (stderr, "  scaling the image using ppmscale %.2lf\n", scale-.005);
	if ((!prows || (prows && icols <= prows)) && irows <= pcols)
	    fprintf (stderr, "  or by rotating the image using ppmrotate 90\n");
	if (force)
	    fprintf (stderr, "\nImage will be printed, but clipped\n");
	else 
	{
	    fprintf (stderr, "  or using the -f option to force printing\n");
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

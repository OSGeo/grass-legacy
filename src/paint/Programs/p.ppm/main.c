#include <unistd.h>
#include "gis.h"
#include "local_proto.h"
#include "Paintlib.h"

char *pgm;

int 
main (int argc, char *argv[])
{
	struct GModule *module;
    int irows, icols, max;
    int nrows, ncols;
    int prows, pcols;
    unsigned char *red, *grn, *blu;
    char *input;
    int force;
    int toowide;
    int toolong;
    int die;
    double x,y,scale;
    struct Option *ppmfile;
    struct Flag *flagi, *flagf;

    G_no_gisinit();
    /* dpg 2/92
    G_gisinit (argv[0]);
    */

	module = G_define_module();
	module->description =
		"Reads portable pixmap (ppm) files created by PPM utilities.";

    pgm = G_program_name();

    ppmfile = G_define_option();
    ppmfile->key = "input";
    ppmfile->type = TYPE_STRING;
    ppmfile->description = "input file in ppm format";

    flagf = G_define_flag();
    flagf->key = 'f';
    flagf->description = "force print, even if output is too wide for printer";

    die = 0;

    if (!isatty(0))
	G_disable_interactive();
    if (G_parser (argc, argv))
	exit(1);

    force = flagf->answer;
    if (input = ppmfile->answer)
    {
	if(NULL == freopen (input, "r", stdin))
	{
	    fprintf (stderr, "\n%s - ", pgm);
	    perror (input);
	    exit(1);
	}
    }

    if (!header (&irows, &icols, &max))
    {
	fprintf (stderr, "\n%s - %s not in PPM format\n",
	    pgm, input ? input : "input");
	exit(1);
    }
fprintf (stdout,"input  cols %d, rows %d, max color level %d\n", icols, irows, max);

    red = (unsigned char *)xalloc (icols);
    grn = (unsigned char *)xalloc (icols);
    blu = (unsigned char *)xalloc (icols);

/* connect to the paint driver and get printer width */
    Pconnect();
    Popen();
    Pnpixels (&prows, &pcols);
    fprintf (stdout,"output cols=%d", pcols);
    if (prows)
	fprintf (stdout,", rows=%d", prows);
    fprintf (stdout,"\n");

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
	fprintf (stderr, "  scaling the image using ppmscale %.2f\n", scale-.005);
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

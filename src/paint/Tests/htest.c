/* @(#)htest.c	2.3   10/14/87 */
/***********************************************************
test: test the horizontal resolution of the painter

usage:	the user is prompted for the horizontal resolution of the
	printer (pixels per inch)

	then the user is prompted for the length of the horizontal
	bar to be printed (in inches)

	test produces a horizontal bar which should be the correct
	length
************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "Paintlib.h"

int main (int argc, char *argv[])
{
    char buf[100];
    double v;
    double inches;

    int r;
    static int nrows = 6;
    int pixels;
    int ok;
    int repeat;
    unsigned char BLACK;

    G_gisinit(argv[0]);
    Pconnect();
    Plock();
    Popen();
    BLACK = Pcolornum (0.0, 0.0, 0.0);
    for (repeat = 1; repeat; )
    {
	fprintf (stdout,"horizontal resolution set at %f\n", Phres());
	do
	{
	    fprintf (stdout,"enter horizontal resolution (pixels per inch): ");
	    if(!fgets (buf,80,stdin)) exit(0);

	    v = atof (buf);
	} while (v <= 0.0);

	do
	{
	    fprintf (stdout,"enter length of horizontal bar (in inches): ");
	    if(!fgets (buf,80,stdin)) exit(0);

	    inches = atof (buf);
	} while (inches <= 0.0);

	fprintf (stdout,"%f inches at %f pixels per inch\n",inches,v);
	for (ok = 0; !ok; )
	{
	    fprintf (stdout,"look ok? (y/n) ");
	    if(!fgets (buf,80,stdin)) exit(0);
	    switch (*buf)
	    {
	    case 'y':
	    case 'Y':	ok = 1; repeat = 0; break;

	    case 'n':
	    case 'N':	ok = 1; break;
	    }
	}
    }

    Palpha ();
    sprintf (buf,"%f pixels per inch (horizontal), %f inches", v, inches);
    Ptext(buf);
    Ptext("");

    pixels = (inches * v);

    fprintf (stdout,"will paint %d pixels\n", pixels);
    Praster ();
    Ppictsize (nrows, pixels);

    for (r = 0; r < nrows; r++)
    {
	    Prle_begin ();
	    Prle (BLACK, pixels);
	    Prle_end ();
    }

    Pclose ();
    Pdisconnect();

    exit(0);
}

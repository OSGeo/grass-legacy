/* @(#)vtest.c	2.3   10/14/87 */
/***********************************************************
test: test the vertical resolution of the painter II printer

usage:	the user is prompted for the vertical resolution of the
	printer (pixels per inch)

	then the user is prompted for the length of the vertical
	bar to be printed (in inches)

	test produces a vertical bar which should be the correct
	length
************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "Paintlib.h"

int main(int argc,char *argv[])
{
    char buf[100];
    double v;
    double inches;

    int r;
    int nrows;
    int ok;
    int repeat;
    unsigned char BLACK;

    G_gisinit(argv[0]);
    Pconnect();
    Plock();
    Popen();
    BLACK = Pcolornum (0.0,0.0,0.0);

    for (repeat = 1; repeat; )
    {
	fprintf (stdout,"vertical resolution set at %f\n", Pvres());
	do {
	    fprintf (stdout,"enter vertical resolution (pixels per inch): ");
	    if(!fgets (buf,100,stdin)) exit(0);

	    v = atof (buf);
	} while (v <= 0.0);

	do {
	    fprintf (stdout,"enter length of vertical bar (in inches): ");
	    if(!fgets (buf,100,stdin)) exit(0);

	    inches = atof (buf);
	} while (inches <= 0.0);

	fprintf (stdout,"%f inches at %f pixels per inch\n",inches,v);
	for (ok = 0; !ok; )
	{
	    fprintf (stdout,"look ok? (y/n) ");
	    if(!fgets (buf,100,stdin)) exit(0);
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
    sprintf (buf,"%f pixels per inch (vertical), %f inches", v, inches);
    Ptext(buf);
    Ptext("");

    nrows = (inches * v);

    fprintf (stdout,"will paint %d rows\n", nrows);
    Praster ();
    Ppictsize (nrows, 8);

    for (r = 0; r < nrows; r++)
    {
	if (r%100 == 0) fprintf(stderr,"%d ",r);
	Prle_begin ();
	Prle (BLACK, 8);
	Prle_end ();
    }
    fprintf(stderr,"\n");

    Pclose ();
    Pdisconnect ();

    exit (0);
}

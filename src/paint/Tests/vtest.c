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
main(argc,argv) char *argv[];
{
    char buf[100];
    double atof ();
    double v;
    double inches;
    double Pvres();

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
	printf("vertical resolution set at %lf\n", Pvres());
	do {
	    printf ("enter vertical resolution (pixels per inch): ");
	    if(!gets (buf)) exit();

	    v = atof (buf);
	} while (v <= 0.0);

	do {
	    printf ("enter length of vertical bar (in inches): ");
	    if(!gets(buf))exit(0) ;

	    inches = atof (buf);
	} while (inches <= 0.0);

	printf ("%lf inches at %lf pixels per inch\n",inches,v);
	for (ok = 0; !ok; )
	{
	    printf ("look ok? (y/n) ");
	    if (!gets(buf)) exit(0);
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
    sprintf (buf,"%lf pixels per inch (vertical), %lf inches", v, inches);
    Ptext(buf);
    Ptext("");

    nrows = (inches * v);

    printf("will paint %d rows\n", nrows);
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
}

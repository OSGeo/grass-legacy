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
main(argc,argv) char *argv[];
{
    char buf[100];
    double atof ();
    double v;
    double inches;
    double Phres();

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
	printf("horizontal resolution set at %lf\n", Phres());
	do
	{
	    printf ("enter horizontal resolution (pixels per inch): ");
	    if(!gets (buf)) exit();

	    v = atof (buf);
	} while (v <= 0.0);

	do
	{
	    printf ("enter length of horizontal bar (in inches): ");
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
    sprintf (buf,"%lf pixels per inch (horizontal), %lf inches", v, inches);
    Ptext(buf);
    Ptext("");

    pixels = (inches * v);

    printf("will paint %d pixels\n", pixels);
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
}

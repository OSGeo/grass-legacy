/* @(#)ptest.c	2.2   10/14/87 */

/***********************************************************
test: test the number of pixels per line of the painter

	The user is prompted for the number of pixels.
	The test produces hash marks every 2 pixels. Those
	at 10 are a little higher, and at 100 are even higher.
************************************************************/

int pixels;
unsigned char white;
unsigned char black;

#include <stdio.h>

main(argc,argv) char *argv[];
{
    char buf[100];

    int ok;
    int repeat;

    G_gisinit(argv[0]);
    Pconnect();
    Plock();
    Popen();
    white = Pcolornum (1.0,1.0,1.0);
    black = Pcolornum (0.0,0.0,0.0);

    do
    {
	printf ("enter number of pixels across printer: ");
	gets (buf);

	pixels = atoi (buf);
    } while (pixels <= 0);

    printf ("%d pixels\n",pixels);


    Palpha ();
    sprintf (buf,"%d pixels", pixels);
    Ptext(buf);
    Ptext("");

    Praster ();
    Ppictsize (16, pixels);

    ticks (500) ;
    ticks (100) ;
    ticks (10);
    ticks (2);

    Pclose ();
    Pdisconnect ();

}
ticks (n)
{
    register int r, c;

    for (r = 0; r < 4; r++)
    {
	Prle_begin ();
	for (c = 1; c <= pixels; c++)
	    if (c%n) Prle (white, 1);
	    else     Prle (black, 1);
	Prle_end ();
    }
}

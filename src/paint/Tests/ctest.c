/***********************************************************
test: test the number of chars per line of the painter

	The user is prompted for the number of chars.
************************************************************/

int nchars;

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "Paintlib.h"

int nchars;
int ticks (int, char *);

int main (int argc, char *argv[])
{
    char buf[100];
    char *cbuf;
    int ok;
    int repeat;

    G_gisinit(argv[0]);
    Pconnect();
    Plock();
    Popen();

    do
    {
	fprintf (stdout,"enter number of chars across printer: ");
	fgets (buf,100,stdin);

	nchars = atoi (buf);
    } while (nchars <= 0);

    fprintf (stdout,"%d chars\n",nchars);


    Palpha ();

    cbuf = malloc (nchars+1);

    ticks (1000,cbuf) ; Ptext(cbuf);
    ticks (100,cbuf) ; Ptext(cbuf);
    ticks (10,cbuf); Ptext(cbuf);
    ticks (1,cbuf); Ptext(cbuf);

    Pclose ();
    Pdisconnect ();

    exit(0);
}

int ticks (int n, char *cbuf)
{
    int r,d, c;

    for (c = 1; c <= nchars; c++)
    {
	if (c%n == 0)
	{
	    r = c / n;
	    d = r % 10;
	    if (d || c >= n)
		*cbuf++ = d + '0';
	    else
		*cbuf++ = ' ';
	}
	else
	    *cbuf++ = ' ';
    }
    *cbuf = 0;

    return 0;
}

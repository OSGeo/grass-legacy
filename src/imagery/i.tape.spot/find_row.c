/********************************************************/
/* NAME:	find_row				*/
/*							*/
/* FUNCTION:	find the band and row number		*/
/*							*/
/* USAGE:	find_row(band,row)			*/
/*							*/
/* INPUT:	band -- current band number		*/
/*		row -- current row number		*/
/*							*/
/* OUTPUT:	1 -- correct band & row number		*/
/*		0 -- missing row			*/
/********************************************************/
#include "tape.h"
find_row (band, row)
{
    int b;
    int r;
    unsigned char *j;
    register int i;

    G_zero (tape.tapebuf, tape.tapebufsize);
    while ( (tape.n = read (tape.fd, tape.tapebuf, tape.tapebufsize)) > 0 )
    {
    j=tape.tapebuf;
    for (i=1; i< 33; i++)
    	tape.buf[i] = *(j++);  
  	tape.buf[i] = (char) 0;
	tape.record_type = record_type();
	if (tape.record_type == IMAGE_DATA)
	{
	    b = number (19,20);	/* get band number */
	    r = number (13,16);	/* get row number */
	    if (b == band && r == row)
		    return (1);
	    if ( r > row )
	    {
		printf("** WARNING: band %d row %d missing from tape\n", band, row);
		return 0;
	    }
	}
    G_zero (tape.tapebuf, tape.tapebufsize);
    }
    return 0;
}

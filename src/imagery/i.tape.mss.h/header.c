#include "tape.h"

#define integer(x) buf[x] * 256 + buf[x+1]

int header (unsigned char buf[])
{
    int i;

    num_bands = buf[120];

    fprintf (stdout,"\tMISSION: %c\n", buf[7]) ;
    fprintf (stdout,"\tTIME OF OBS (relative to launch):\n");
    fprintf (stdout,"\t\tDAY:  %c%c%c%c\n",
	    buf[8], buf[9], buf[10], buf[11]) ;
    fprintf (stdout,"\t\tHOUR: %c%c\n", buf[12], buf[13]) ;
    fprintf (stdout,"\t\tMIN:  %c%c\n", buf[14], buf[15]) ;

    i = integer(57);
    fprintf (stdout,"\tNOMINAL PIXELS/SCAN: %d\n", i);
    i = integer(130);
    fprintf (stdout,"\tPIXELS/SCAN:         %d\n", i);

    fprintf (stdout,"\tWRS DESIGNATOR:\n");
    i = integer(72);
    fprintf (stdout,"\t\tSCAN LINE:   %d\n", i);
    i = integer(74);
    fprintf (stdout,"\t\tPIXEL NUMBER: %d\n", i);

    fprintf (stdout,"\tTIME OF EXPOSURE:\n");
    fprintf (stdout,"\t\tYEAR: %c%c\n", buf[76], buf[77]) ;

    fprintf (stdout,"\t\tDAY:  %c%c%c\n", buf[78], buf[79], buf[80]) ;

    fprintf (stdout,"\t\tHOUR: %c%c\n", buf[81], buf[82]) ;

    fprintf (stdout,"\tGEOMETRIC CORRECTIONS:\t\t%sapplied\n",
	    buf[106] ? "" : "not ") ;

    fprintf (stdout,"\tGEOMETRIC CORRECTION DATA:\t%spresent\n",
	    buf[107] ? "" : "not ") ;

    fprintf (stdout,"\tRADIOMETRIC CORRECTIONS:\t%sapplied\n",
	    buf[108] ? "" : "not ") ;

    fprintf (stdout,"\tRADIOMETRIC CORRECTION DATA:\t%spresent\n",
	    buf[109] ? "" : "not ") ;

    i = integer (110);
    fprintf (stdout,"\tIMAGE RECORD LENGTH:\t\t%d\n", i);

    fprintf (stdout,"\tINTERLEAVING TYPE:\t\t%s\n", buf[119] ? "bil" : "bsq") ;
    fprintf (stdout,"\tBIL INTERLEAVING COUNT:\t\t%d\n", num_bands) ;

    fprintf (stdout,"\tRESAMPLING APPLIED:\t\t") ;
    switch(buf[122])
    {
    case 0300:
	    fprintf (stdout,"none\n") ;
	    break ;
    case 011:
	    fprintf (stdout,"cubic convolution\n") ;
	    break ;
    case 022:
	    fprintf (stdout,"nearest neighbor\n") ;
	    break ;
    default:
	    fprintf (stdout,"**unknown** (0%o)\n", buf[122]) ;
	    break ;
    }


    fprintf (stdout,"\tMAP PROJECTION APPLIED:\t\t") ;
    switch(buf[123])
    {
    case 0300:
	    fprintf (stdout,"none\n") ;
	    break ;
    case 011:
	    fprintf (stdout,"universal tranverse mercator\n") ;
	    break ;
    case 022:
	    fprintf (stdout,"polar sterographic\n") ;
	    break ;
    default:
	    fprintf (stdout,"**unknown** (0%o)\n", buf[123]) ;
	    break ;
    }
    return 0;
}

#include "tape.h"

#define integer(x) buf[x] * 256 + buf[x+1]

header(buf)
    unsigned char buf[] ;
{
    int i;

    num_bands = buf[120];

    printf("\tMISSION: %c\n", buf[7]) ;
    printf("\tTIME OF OBS (relative to launch):\n");
    printf("\t\tDAY:  %c%c%c%c\n",
	    buf[8], buf[9], buf[10], buf[11]) ;
    printf("\t\tHOUR: %c%c\n", buf[12], buf[13]) ;
    printf("\t\tMIN:  %c%c\n", buf[14], buf[15]) ;

    i = integer(57);
    printf("\tNOMINAL PIXELS/SCAN: %d\n", i);
    i = integer(130);
    printf("\tPIXELS/SCAN:         %d\n", i);

    printf("\tWRS DESIGNATOR:\n");
    i = integer(72);
    printf("\t\tSCAN LINE:   %d\n", i);
    i = integer(74);
    printf("\t\tPIXEL NUMBER: %d\n", i);

    printf("\tTIME OF EXPOSURE:\n");
    printf("\t\tYEAR: %c%c\n", buf[76], buf[77]) ;

    printf("\t\tDAY:  %c%c%c\n", buf[78], buf[79], buf[80]) ;

    printf("\t\tHOUR: %c%c\n", buf[81], buf[82]) ;

    printf("\tGEOMETRIC CORRECTIONS:\t\t%sapplied\n",
	    buf[106] ? "" : "not ") ;

    printf("\tGEOMETRIC CORRECTION DATA:\t%spresent\n",
	    buf[107] ? "" : "not ") ;

    printf("\tRADIOMETRIC CORRECTIONS:\t%sapplied\n",
	    buf[108] ? "" : "not ") ;

    printf("\tRADIOMETRIC CORRECTION DATA:\t%spresent\n",
	    buf[109] ? "" : "not ") ;

    i = integer (110);
    printf("\tIMAGE RECORD LENGTH:\t\t%d\n", i);

    printf("\tINTERLEAVING TYPE:\t\t%s\n", buf[119] ? "bil" : "bsq") ;
    printf("\tBIL INTERLEAVING COUNT:\t\t%d\n", num_bands) ;

    printf("\tRESAMPLING APPLIED:\t\t") ;
    switch(buf[122])
    {
    case 0300:
	    printf("none\n") ;
	    break ;
    case 011:
	    printf("cubic convolution\n") ;
	    break ;
    case 022:
	    printf("nearest neighbor\n") ;
	    break ;
    default:
	    printf("**unknown** (0%o)\n", buf[122]) ;
	    break ;
    }


    printf("\tMAP PROJECTION APPLIED:\t\t") ;
    switch(buf[123])
    {
    case 0300:
	    printf("none\n") ;
	    break ;
    case 011:
	    printf("universal tranverse mercator\n") ;
	    break ;
    case 022:
	    printf("polar sterographic\n") ;
	    break ;
    default:
	    printf("**unknown** (0%o)\n", buf[123]) ;
	    break ;
    }
}

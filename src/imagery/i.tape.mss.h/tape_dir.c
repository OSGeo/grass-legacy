#include "tape.h"

tape_dir (buf)
    unsigned char buf[];
{

    vol = tape_vol (buf[18]);
    tape_type = buf[30] ? BIL : BSQ ;
    record_size = buf[31] ;
    record_size <<= 8 ;
    record_size += buf[32] ;

    mission[0] = buf[7];
    mission[1] = 0;

    printf("\tVOLUME: %d\n",vol);
    printf("\tMISSION: ") ;
    switch(buf[6])
    {
    case 'L':
    case 'l':
	    printf("landsat\n") ;
	    break ;
    default:
	    printf("**unknown**\n") ;
	    break ;
    }
    printf("\tMISSION NUM: %c\n", buf[7]) ;
    printf("\tSENSOR TYPE: ") ;
    switch(buf[8])
    {
    case 'M':
    case 'm':
	    printf("mss\n") ;
	    break ;
    case 'R':
    case 'r':
	    printf("rbv\n") ;
	    break ;
    default:
	    printf("**unknown**\n") ;
	    break ;
    }
    printf("\tDATA CODED: ") ;
    switch(buf[10])
    {
    case 'P':
    case 'p':
	    printf("without") ;
	    break ;
    case 'A':
    case 'a':
	    printf("with") ;
	    break ;
    default:
	    printf("??") ;
	    break ;
    }
    printf(" geometric corrections\n") ;
    printf("\tDATE OF TAPE:\n");
    printf("\t\tYEAR: %c%c\n", buf[11],buf[12]) ;
    printf("\t\tDAY: %c%c%c\n", buf[13],buf[14],buf[15]) ;
    printf("\tCCT VOLUME %c of %c\n", buf[18], buf[19]) ;
    printf("\tTAPE GENERATION DATE:\n");
    printf( "\t\tYEAR:  %d\n", buf[28]);
    printf( "\t\tMONTH: %d\n", buf[27]);
    printf( "\t\tDAY:   %d\n", buf[26]);
    printf("\tINTERLEAVING TYPE:  %s\n",
	    buf[30] ? "mss bil" : "mss bsq & rbv");
    printf("\tRECORD LENGTH: %d\n", record_size) ;
    printf("\tSOURCE HDT: ") ;
    switch(buf[33])
    {
    case ' ':
	    printf("n/a\n") ;
	    break ;
    case 'C':
    case 'c':
	    printf("corrected data\n") ;
	    break ;
    case 'U':
    case 'u':
	    printf("uncorrected data\n") ;
	    break ;
    default:
	    printf("**unknown**\n") ;
	    break ;
    }
    printf("\tSCENE ID:\n");
    printf("\t\tMISSION: %c\n", buf[36]);
    printf("\t\tDAY:     %c%c%c%c\n", buf[37],buf[38],buf[39],buf[40]);
    printf("\t\tHOUR:    %c%c\n", buf[41],buf[42]);
    printf("\t\tMINUTE:  %c%c\n", buf[43],buf[44]);
}

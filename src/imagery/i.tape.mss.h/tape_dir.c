#include "tape.h"

int tape_dir (unsigned char buf[])
{

    vol = tape_vol (buf[18]);
    tape_type = buf[30] ? BIL : BSQ ;
    record_size = buf[31] ;
    record_size <<= 8 ;
    record_size += buf[32] ;

    mission[0] = buf[7];
    mission[1] = 0;

    fprintf(stderr, "\tVOLUME: %d\n",vol);
    fprintf(stderr, "\tMISSION: ") ;
    switch(buf[6])
    {
    case 'L':
    case 'l':
	    fprintf(stderr, "landsat\n") ;
	    break ;
    default:
	    fprintf(stderr, "**unknown**\n") ;
	    break ;
    }
    fprintf(stderr, "\tMISSION NUM: %c\n", buf[7]) ;
    fprintf(stderr, "\tSENSOR TYPE: ") ;
    switch(buf[8])
    {
    case 'M':
    case 'm':
	    fprintf(stderr, "mss\n") ;
	    break ;
    case 'R':
    case 'r':
	    fprintf(stderr, "rbv\n") ;
	    break ;
    default:
	    fprintf(stderr, "**unknown**\n") ;
	    break ;
    }
    fprintf(stderr, "\tDATA CODED: ") ;
    switch(buf[10])
    {
    case 'P':
    case 'p':
	    fprintf(stderr, "without") ;
	    break ;
    case 'A':
    case 'a':
	    fprintf(stderr, "with") ;
	    break ;
    default:
	    fprintf(stderr, "??") ;
	    break ;
    }
    fprintf(stderr, " geometric corrections\n") ;
    fprintf(stderr, "\tDATE OF TAPE:\n");
    fprintf(stderr, "\t\tYEAR: %c%c\n", buf[11],buf[12]) ;
    fprintf(stderr, "\t\tDAY: %c%c%c\n", buf[13],buf[14],buf[15]) ;
    fprintf(stderr, "\tCCT VOLUME %c of %c\n", buf[18], buf[19]) ;
    fprintf(stderr, "\tTAPE GENERATION DATE:\n");
    fprintf(stderr, "\t\tYEAR:  %d\n", buf[28]);
    fprintf(stderr, "\t\tMONTH: %d\n", buf[27]);
    fprintf(stderr, "\t\tDAY:   %d\n", buf[26]);
    fprintf(stderr, "\tINTERLEAVING TYPE:  %s\n",
	    buf[30] ? "mss bil" : "mss bsq & rbv");
    fprintf(stderr, "\tRECORD LENGTH: %d\n", record_size) ;
    fprintf(stderr, "\tSOURCE HDT: ") ;
    switch(buf[33])
    {
    case ' ':
	    fprintf(stderr, "n/a\n") ;
	    break ;
    case 'C':
    case 'c':
	    fprintf(stderr, "corrected data\n") ;
	    break ;
    case 'U':
    case 'u':
	    fprintf(stderr, "uncorrected data\n") ;
	    break ;
    default:
	    fprintf(stderr, "**unknown**\n") ;
	    break ;
    }
    fprintf(stderr, "\tSCENE ID:\n");
    fprintf(stderr, "\t\tMISSION: %c\n", buf[36]);
    fprintf(stderr, "\t\tDAY:     %c%c%c%c\n", buf[37],buf[38],buf[39],buf[40]);
    fprintf(stderr, "\t\tHOUR:    %c%c\n", buf[41],buf[42]);
    fprintf(stderr, "\t\tMINUTE:  %c%c\n", buf[43],buf[44]);

    return 0;
}

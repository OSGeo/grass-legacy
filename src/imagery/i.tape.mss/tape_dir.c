#include "tape.h"

tape_dir (buf)
    unsigned char buf[];
{
    int i;

    tape_type = buf[30] ? BIL : BSQ ;
    record_size = buf[31] ;
    record_size <<= 8 ;
    record_size += buf[32] ;

    switch(buf[6])
    {
    case 'L':
    case 'l':
	    sprintf(mission,"landsat %c ", buf[7]) ;
	    break ;
    default:
	    sprintf(mission,"") ;
	    break ;
    }
    switch(buf[8])
    {
    case 'M':
    case 'm':
	    strcat(mission,"mss") ;
	    break ;
    case 'R':
    case 'r':
	    strcat(mission,"rbv") ;
	    break ;
    }

    for (i = 36; i <= 44; i++)
	scene_id[i-36] = buf[i];
    scene_id[i-36] = 0;
}

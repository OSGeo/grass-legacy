#include "tape.h"

annotation(buf)
    unsigned char buf[] ;
{
    int i;

    switch(buf[81])
    {
    case 'U':
	    correction = UNCORRECTED ;
	    break ;
    case 'G':
    case 'R':
    case 'S':
	    correction = CORRECTED ;
	    break ;
    default:
	    correction = UNKNOWN ;
	    break ;
    }

/************** landsat date **************/

    sprintf(date,"%c%c%c%c%c%c%c%c", 
	  buf[6], buf[7], buf[8], buf[9], buf[10], buf[11], buf[12], buf[13]) ;

/************** sun angle at center of scene **************/

    for (i = 68; i <= 81; i++)
	    sun_angle[i-68] = buf[i-1];
    sun_angle[i] = 0;


/* image scale */
/*
    switch(buf[82])
    {
    case '1':
	    strcpy(image_scale,"185 km x 185 km") ;
	    break ;
    case '2':
	    strcpy(image_scale,"99 km x 99 km") ;
	    break ;
    case '3':
	    strcpy(image_scale,"185 km x 170 km") ;
	    break ;
    default:
	    strcpy(image_scale,"") ;
	    break ;
    }
*/

/* projection */
/*
    switch(buf[82])
    {
    case 'L':
	    strcpy (projection,"lambert") ;
	    break ;
    case 'P':
	    strcpy (projection,"polar stereographic") ;
	    break ;
    case 'S':
	    strcpy (projection,"space oblique mercator") ;
	    break ;
    case 'U':
	    strcpy (projection,"universal transverse mercator") ;
	    break ;
    case 'H':
	    strcpy (projection,"hotine oblique mercator") ;
	    break ;
    default:
	    strcpy (projection,"") ;
	    break ;
    }
*/
}

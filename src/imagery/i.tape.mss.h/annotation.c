#include "tape.h"

int annotation (unsigned char buf[])
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

/* date */
    sprintf(date,"%c%c%c%c%c%c%c%c", 
	  buf[6], buf[7], buf[8], buf[9], buf[10], buf[11], buf[12], buf[13]) ;

/************** sun angle at center of scene **************/

    for (i = 68; i <= 81; i++)
	sun_angle[i-68] = buf[i-1];
    sun_angle[i] = 0;

    fprintf (stdout,"\tDAY OF EXPOSURE:\t\t%s\n", date) ;
    fprintf (stdout,"\tSUN ANGLES:\t\t\t%s\n", sun_angle) ;

    fprintf (stdout,"\tCENTER LATITUDE AND LONGITUDE:\t") ; 
    fprintf (stdout,"%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c\n", 
	    buf[14], buf[15], buf[16], buf[17], 
	    buf[18], buf[19], buf[20], buf[21],
	    buf[22], buf[23], buf[24], buf[25], 
	    buf[26], buf[27], buf[28], buf[29],
	    buf[30] ) ;

    fprintf (stdout,"\tNOMINAL LATITUDE AND LONGITUDE:\t") ; 
    fprintf (stdout,"%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c\n", 
	    buf[40], buf[41], buf[42], buf[43], 
	    buf[44], buf[45], buf[46], buf[47],
	    buf[48], buf[49], buf[50], buf[51], 
	    buf[52], buf[53], buf[54], buf[55],
	    buf[56] ) ;

    fprintf (stdout,"\tTYPE OF CORRECTION APPLIED:\t") ;
    switch(buf[81])
    {
	case 'U':
		fprintf (stdout,"uncorrected\n") ;
		break ;
	case 'S':
		fprintf (stdout,"system\n") ;
		break ;
	case 'G':
		fprintf (stdout,"geometric based on geometric gcp's\n") ;
		break ;
	case 'R':
		fprintf (stdout,"geometric based on relative gcp's\n") ;
		break ;
	default:
		fprintf (stdout,"**unknown**\n") ;
		break ;
    }
    fprintf (stdout,"\tSCALE OF IMAGE:\t\t\t") ;
    switch(buf[82])
    {
	case '1':
		fprintf (stdout,"185 km x 185 km\n") ;
		break ;
	case '2':
		fprintf (stdout,"99 km x 99 km\n") ;
		break ;
	case '3':
		fprintf (stdout,"185 km x 170 km\n") ;
		break ;
	default:
		fprintf (stdout,"**unknown**\n") ;
		break ;
    }
    fprintf (stdout,"\tPROJECTION:\t\t\t") ;
    switch(buf[82])
    {
	case 'L':
		fprintf (stdout,"lambert\n") ;
		break ;
	case 'P':
		fprintf (stdout,"polar stereographic\n") ;
		break ;
	case 'S':
		fprintf (stdout,"space oblique mercator\n") ;
		break ;
	case 'U':
		fprintf (stdout,"universal transverse mercator\n") ;
		break ;
	case 'H':
		fprintf (stdout,"hotine oblique mercator\n") ;
		break ;
	default:
		fprintf (stdout,"**unknown** (%c)\n",buf[82]) ;
		break ;
    }

    return 0;
}

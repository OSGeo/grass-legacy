/********************************************************/
/* NAME:	record_type				*/
/*							*/
/* FUNCTION: 	figure out record type			*/
/*							*/
/* USAGE:	record_type()				*/
/*							*/
/* INPUT:	none					*/
/*							*/
/* OUTPUT:	string of anonym of "-1" as error	*/
/********************************************************/
#include "tape.h"

static unsigned char type_table[] = {
	0300, 0300,  022,  022, VOLUME_DESCRIPTOR,
	0333, 0300,  022,  022, FILE_POINTER,
	 022,  077,  022,  022, TEXT,
	 077, 0300,  022,  022, FILE_DESCRIPTOR,
	 022,  022,  022,  022, SCENE_HEADER,
	0366,  044,  022,  022, EPHEMERIS_ATTITUDE_ANCILLARY,
	 077,  044,  022,  022, RADIOMETRIC_CALIBRATION_ANCILLARY,
	0300,  044,  022,  022, HISTOGRAMS_ANCILLARY,
	 044,  044,  022,  022, MAP_PROJECTION_ANCILLARY,
	 011,  044,  022,  022, CONTROL_POINT_DATA,
	 022, 0333,  022,  022, ANNOTATION,
	0355, 0355,  022,  022, IMAGE_DATA,
	 022, 0366,  022,  022, TRAILER,
	0300, 0300,  077,  022, NULL_VOLUME_DESCRIPTOR
};

record_type ()
{
    unsigned char *t;
    unsigned char *t_end;

/* record type is in tape.buf[5-8] of the first 12 bytes of all */
/* records  on a SPOT tape 					*/

    t_end = type_table + sizeof(type_table);
    for (t = type_table; t < t_end; t += 5)
	if (t[0] == tape.buf[5] &&
	    t[1] == tape.buf[6] &&
	    t[2] == tape.buf[7] &&
	    t[3] == tape.buf[8])
		    return (t[4]);
    return (-1);
}

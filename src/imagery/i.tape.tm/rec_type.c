#include "tape.h"

static unsigned char type_table[] = {
	0300, 0300,  022,  022, VOLUME_DESCRIPTOR,
	0333, 0300,  022,  022, FILE_POINTER,
	 022,  077,  022,  022, TEXT,
	 077, 0300,  022,  022, FILE_DESCRIPTOR,
	 022,  022,  022,  011, SCENE_HEADER,
	 044,  044,  022,  011, MAP_PROJECTION_ANCILLARY,
	 077,  044,  022,  011, RADIOMETRIC_CALIBRATION_ANCILLARY,
	0355, 0355, 0333,  011, IMAGE_DATA,
	 022, 0366,  022,  011, TRAILER,
	 022,  022, 0222,  022, SCENE_DEFINITION,
	 043,  044, 0222,  022, UNPROCESSED_SCD,
	 011,  044, 0222,  022, CONTROL_POINT_DATA,
	0355,  044, 0222,  022, GEOMETRIC_MODELING_DATA,
	0144,  044, 0222,  022, HIGH_FREQUENCY_MATRICES,
	 022, 0333, 0222,  022, ANNOTATION,
	 055,  022,  044,  022, BAND_QUALITY_DATA,
	0300, 0300,  077,  022, NULL_VOLUME_DESCRIPTOR
};

record_type ()
{
    unsigned char *t;
    unsigned char *t_end;

/* record type is in tape.buf[5-8] */

    t_end = type_table + sizeof(type_table);
    for (t = type_table; t < t_end; t += 5)
	if (t[0] == tape.buf[5] &&
	    t[1] == tape.buf[6] &&
	    t[2] == tape.buf[7] &&
	    t[3] == tape.buf[8])
		    return (t[4]);
    return (-1);
}

/*======================================================================
Filename:   rec_type.c
Module:	    i.landsat.tm
Author:	    Michael Shapiro
======================================================================*/

#include "landsat.h"

/****
static unsigned char type_table[] = {
0140, 0300, 0011, 0022, VOLUME_DESCRIPTOR,
0155, 0700, 0011, 0022, FILE_POINTER,
0011, 0077, 0011, 0022, TEXT,
0037, 0700, 0011, 0022, FILE_DESCRIPTOR,
0011, 0022, 0011, 0011, SCENE_HEADER,
0022, 0044, 0011, 0011, MAP_PROJECTION_ANCILLARY,
0037, 0444, 0011, 0011, RADIOMETRIC_CALIBRATION_ANCILLARY,
0166, 0755, 0155, 0411, IMAGE_DATA
};
*****/

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


/*======================================================================
			     RecordType

Return the record type from buffer[5-8], or -1 if the buffer matches no
known record type.
======================================================================*/
int 
RecordType (unsigned char *buffer)
{
    unsigned char *t;
    unsigned char *t_end;

    t_end = type_table + sizeof(type_table);
    for (t = type_table; t < t_end; t += 5)
	if (t[0] == buffer[5] &&
	    t[1] == buffer[6] &&
	    t[2] == buffer[7] &&
	    t[3] == buffer[8])
		    return (t[4]);
    return (-1);
}

#include "dxf2vect.h"

dxf_make_header (vect_file)
FILE	*vect_file;
{
	fprintf (vect_file, "ORGANIZATION: US Army Const. Eng. Rsch. Lab\n");
	fprintf (vect_file, "DIGIT DATE:   04/01/89\n"); 
	fprintf (vect_file, "DIGIT NAME:   Chuck Ehlschlaeger\n");
	fprintf (vect_file, "MAP NAME:     Quick and Dirty\n");
	fprintf (vect_file, "MAP DATE:     02/29/89\n");
	fprintf (vect_file, "MAP SCALE:    24000\n");
	fprintf (vect_file, "OTHER INFO:    \n");
	fprintf (vect_file, "ZONE:         5\n");
	fprintf (vect_file, "WEST EDGE:    ");
	layers[num_layers].w_off = ftell (vect_file);
	fprintf (vect_file, "%-60s\n", "0");
	fprintf (vect_file, "EAST EDGE:    ");
	layers[num_layers].e_off = ftell (vect_file);
	fprintf (vect_file, "%-60s\n", "0");
	fprintf (vect_file, "SOUTH EDGE:   ");
	layers[num_layers].s_off = ftell (vect_file);
	fprintf (vect_file, "%-60s\n", "0");
	fprintf (vect_file, "NORTH EDGE:   ");
	layers[num_layers].n_off = ftell (vect_file);
	fprintf (vect_file, "%-60s\n", "0");
	fprintf (vect_file, "MAP THRESH:   00.00\n");
	fprintf (vect_file, "VERTI:         \n");
}

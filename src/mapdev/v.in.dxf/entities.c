/* modified 1998-OCT-06 Benjamin Horner-Johnson - 80->256 char dxf_line */
#include "dxf2vect.h"

int 
dxf_entities (FILE *dxf_file)
{
	dxf_fgets (dxf_line, 256, dxf_file); 
	while (strcmp (dxf_line, entitie) != 0) 
	{ 
		dxf_fgets (dxf_line, 256, dxf_file); 
		if (feof (dxf_file)) 
		{ 
			fprintf (stderr, "end of file while looking"); 
			fprintf (stderr, " for ENTITIES\n"); 
			return (-1); 
		} 
	} 
	return (0);
}

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <grass/Vect.h>
#include <grass/gis.h>
#include <grass/glocale.h>

int writeHead(FILE * fp, struct Map_info *Map)
{
    G_debug(3, _("writeVTKHeader: Writing VTK-Header"));
    fprintf(fp, "# vtk DataFile Version 3.0\n");
    fprintf(fp, "GRASS 6 vector map: %s date: %s\n", Vect_get_map_name(Map),
	    Vect_get_map_date(Map));
    fprintf(fp, "ASCII\n");
    fprintf(fp, "DATASET POLYDATA\n");	/*We are using polydata. */
    return (0);
}

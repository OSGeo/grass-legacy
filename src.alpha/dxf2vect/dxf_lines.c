#include "dxf2vect.h"

dxf_find_lines (dxf_file)
FILE	*dxf_file;
{
	dxf_entities (dxf_file);
	pt_array = (POINT *) malloc (ARRAY_INCR * sizeof (POINT));
	arr_max = ARRAY_INCR;
	arr_size = 0;
	n = e = DBL_MIN;
	w = s = DBL_MAX;
	fgets (dxf_line, 80, dxf_file);
	while (feof (dxf_file) == 0)
	{
		if (strcmp (dxf_line, polyline) == 0)
		{
			dxf_add_polyline (dxf_file);
			arr_size = 0;
		}
		else if (strcmp (dxf_line, line) == 0)
		{
			dxf_add_line (dxf_file);
			arr_size = 0;
		}
		else if (strcmp (dxf_line, point) == 0)
		{
			dxf_add_point (dxf_file);
			arr_size = 0;
		}
		fgets (dxf_line, 80, dxf_file);
	}
}

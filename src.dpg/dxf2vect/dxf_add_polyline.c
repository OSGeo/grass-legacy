#include "dxf2vect.h"

dxf_add_polyline (dxf_file)
FILE	*dxf_file;
{
	int	count;
	FILE	*layer_fd, *dxf_which_layer();

	fgets (dxf_line, 80, dxf_file);
	while (strcmp (dxf_line, eeight) != 0)
	{
	    if (strcmp (dxf_line, seqend) == 0)
	    {
		fprintf (stderr, "error looking for layername\n");
		exit (-1);
	    }
	    fgets (dxf_line, 80, dxf_file);
	}
	fgets (dxf_line, 80, dxf_file);
	layer_fd = dxf_which_layer (dxf_line, DXF_ASCII);
	while (strcmp (dxf_line, seqend) != 0)
	{
		if (strcmp (dxf_line, vertex) == 0)
		{
			if (arr_size == arr_max)
			{
				arr_max += ARRAY_INCR;
				pt_array = (POINT *) realloc
					(pt_array, arr_max * sizeof (POINT));
			}
			fgets (dxf_line, 80, dxf_file);
			while (strcmp (dxf_line, tten) != 0)
			  {
			  if (strcmp (dxf_line, vertex) == 0 ||
				strcmp (dxf_line, zzero) == 0)
			  {
				fprintf (stderr, "error looking for ten\n");
				exit (-1);
			  }
			  fgets (dxf_line, 80, dxf_file);
			}
			fgets (dxf_line, 80, dxf_file);
			sscanf (dxf_line, "%lf", &(pt_array[arr_size].x));
			fgets (dxf_line, 80, dxf_file);
			while (strcmp (dxf_line, ttwenty) != 0)
			{
				if (strcmp (dxf_line, vertex) == 0 ||
					strcmp (dxf_line, zzero) == 0)
				{
					fprintf (stderr, "error looking for twenty\n");
					exit (-1);
				}
				fgets (dxf_line, 80, dxf_file);
			}
			fgets (dxf_line, 80, dxf_file);
			sscanf (dxf_line, "%lf", &(pt_array[arr_size].y));
			dxf_check_ext (pt_array[arr_size].x,
				pt_array[arr_size].y);
			arr_size++;
		}
		fgets (dxf_line, 80, dxf_file);
	}
	fprintf (layer_fd, "L  %d\n", arr_size);
	for (count = 0; count < arr_size; count++)
	{
		fprintf (layer_fd, " %12.2lf %12.2lf\n", 
			pt_array[count].y, pt_array[count].x);
	}
}

#include "dxf2vect.h"

dxf_add_point (dxf_file)
FILE	*dxf_file;
{
	int	count;
	FILE	*layer_fd, *dxf_which_layer();

	fgets (dxf_line, 80, dxf_file);
	while (strcmp (dxf_line, eeight) != 0)
	{
	    if (strcmp (dxf_line, zzero) == 0)
	    {
		fprintf (stderr, "error looking for layername\n");
		exit (-1);
	    }
	    fgets (dxf_line, 80, dxf_file);
	}
	fgets (dxf_line, 80, dxf_file);
	layer_fd = dxf_which_layer (dxf_line, DXF_ASCII);
	fgets (dxf_line, 80, dxf_file);
	while (strcmp (dxf_line, tten) != 0)
	{
	        if (strcmp (dxf_line, zzero) == 0)
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
		if (strcmp (dxf_line, zzero) == 0)
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
	fprintf (layer_fd, "P  2\n");
	for (count = 0; count < 2; count++)
	{
		fprintf (layer_fd, " %12.2lf %12.2lf\n", 
			pt_array[0].y, pt_array[0].x);
	}
}

#include "dxf2vect.h"

FILE *
dxf_open_layer (type)
    int type;
{
    char	whole_name[300];
    char	layer_name[40];
    FILE	*layer_fd;

    /* dpg
    strcpy (whole_name, dig_path);
    strcat (whole_name, "/");
    */
    sscanf (layers[num_layers].name, "%[^\n]", layer_name);
    if (type == DXF_ASCII)
	sprintf (whole_name, "%s/dig_ascii/%s.%s", dig_path, basename, layer_name);
    else
	sprintf (whole_name, "%s/dig_att/%s.%s", dig_path, basename, layer_name);
    /*
    strcat (whole_name, layer_name);
    */
    layer_fd = fopen (whole_name, "w");
    if (layer_fd == NULL)
    {
	fprintf (stderr, "error: unable to open dig file\n");
	exit (-1);
    }
    layers[num_layers].fd = layer_fd;
    if (type == DXF_ASCII)
	dxf_make_header (layer_fd);
    return (layer_fd);
}

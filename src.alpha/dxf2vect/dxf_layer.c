#include "dxf2vect.h"

FILE *
dxf_open_layer ()
{
    char	whole_name[300];
    char	layer_name[40];
    FILE	*layer_fd;

    strcpy (whole_name, dig_path);
    strcat (whole_name, "/");
    sscanf (layers[num_layers].name, "%[^\n]", layer_name);
    strcat (whole_name, layer_name);
    layer_fd = fopen (whole_name, "w");
    if (layer_fd == NULL)
    {
	fprintf (stderr, "error: unable to open dig file\n");
	exit (-1);
    }
    layers[num_layers].fd = layer_fd;
    dxf_make_header (layer_fd);
    return (layer_fd);
}

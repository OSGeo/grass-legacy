#include "dxf2vect.h"
#include "gis.h"

FILE *
dxf_open_layer (type,count)
    int type;
    int count;
{
    char	filename[300];
    FILE	*layer_fd;


    sprintf (filename, "%s.%s", basename, layers[count].name);

    if (type == DXF_ASCII)
	layer_fd = G_fopen_new ("dig_ascii", filename);
    else
	layer_fd = G_fopen_new("dig_att",filename);

    if (layer_fd == NULL)
    {
	fprintf(stderr,"error: unable to open dig file %s\n",filename);
	exit (-1);
    }

    if (type == DXF_ASCII)
    {
	dxf_make_header (layer_fd);
    }

    layers[count].fd = layer_fd;
    return (layer_fd);
}

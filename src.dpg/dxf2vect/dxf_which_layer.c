#include "dxf2vect.h"

FILE *
dxf_which_layer (layer_name, type)
char	*layer_name;
int type;
{
    int	count;
    FILE *layer_fd, *dxf_open_layer();

    for (count = 0; count < num_layers; count++)
    {
	if (type == layers[count].type && 
	    strcmp (layer_name, layers[count].name) == 0)
	{
	    layer_fd = layers[count].fd;
	    return (layer_fd);
	}
    }
    if (num_layers == 0)
    {
	layers = (DXF_DIG *) malloc (sizeof (DXF_DIG));
    }
    else
    {
	layers = (DXF_DIG *) realloc (layers,
				(num_layers + 1) * sizeof (DXF_DIG));
    }
    if (layers == NULL)
    {
	fprintf (stderr, "error: not enough memory for all layer names\n");
	exit (-1);
    }
    layers[num_layers].name = (char *) malloc (sizeof (char) * strlen (layer_name));

    layers[num_layers].type = type;
    strcpy (layers[num_layers].name, layer_name);

    layer_fd = dxf_open_layer (type);
    num_layers++;
    return (layer_fd);
}

#include "dxf2vect.h"

FILE *
dxf_which_layer (layer_name)
char	*layer_name;
{
    int	count;
    FILE *layer_fd, *dxf_open_layer();

    for (count = 0; count < num_layers; count++)
    {
	if (strcmp (layer_name, layers[count].name) == 0)
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
    layers[num_layers].name = (char *) malloc (sizeof (char) *
					(1 + strlen (layer_name)));
    strcpy (layers[num_layers].name, layer_name);
    layer_fd = dxf_open_layer ();
    num_layers++;
    return (layer_fd);
}

/* 1/28/98 change from Jacques Bouchard <bouchard@onera.fr> */
#include "dxf2vect.h"

/*#define DEBUG*/

int 
dxf_open_layer (int type, int count)
{
    char	filename[300];
    FILE	*layer_fd;


    sprintf (filename, "%s.%s", base_name, layers[count].name);
#ifdef DEBUG
	fprintf(stderr,"%d opening layername %s \n",count,layers[count].name);
#endif

    layers[count].Map = NULL;

    if (type == DXF_ASCII)
    {
	if (!ascii_flag->answer) /* FOR USE IN BINARY FILE */
	{
	    layers[count].Map = 
		(struct Map_info *) G_malloc (sizeof (struct Map_info));
	    if (0 > Vect_open_new (layers[count].Map, filename))
		G_fatal_error ("Could not open new vector file");
	    layer_fd = layers[count].Map->dig_fp;	/* so we can get thru the bottom of routine */
	}
	else
	{
	    layer_fd = G_fopen_new ("dig_ascii", filename);
	    layers[count].fd = layer_fd;
	}
    }
    else
    {
	layer_fd = G_fopen_new("dig_att",filename);
	layers[count].fd = layer_fd;
    }

    if (layer_fd == NULL)
    {
	fprintf(stderr,"error: unable to open file %s\n",filename);
	exit (-1);
    }

    if (type == DXF_ASCII)
    {
	dxf_make_header (&(layers[count]));
    }

    return 0;
}

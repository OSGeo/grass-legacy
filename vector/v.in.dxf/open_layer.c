/* 1/28/98 change from Jacques Bouchard <bouchard@onera.fr> */
#include "global.h"

int open_layer(int type, int count)
{
    char buf[300];
    FILE *layer_fd;


    sprintf(buf, "%s_%s", base_name, layers[count].name);
#ifdef DEBUG
    fprintf(stderr, "%d opening layername %s \n", count, layers[count].name);
#endif

    layers[count].Map = NULL;

    if (type == DXF_ASCII) {
	layers[count].Map =
	    (struct Map_info *)G_malloc(sizeof(struct Map_info));
	if (0 > Vect_open_new(layers[count].Map, buf, 1))
	    G_fatal_error("Could not open new vector file");
	layer_fd = layers[count].Map->dig_fp.file;	/* so we can get thru the bottom of routine */
    }
    else {
	layer_fd = G_fopen_new("dig_att", buf);
	layers[count].fd = layer_fd;
    }

    if (layer_fd == NULL) {
	fprintf(stderr, "error: unable to open file %s\n", buf);
	exit(-1);
    }

    if (type == DXF_ASCII)
	make_header(&(layers[count]));

    return 0;
}

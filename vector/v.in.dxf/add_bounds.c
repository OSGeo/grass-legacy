/* 1/28/98 change from Jacques Bouchard <bouchard@onera.fr> */
/* revised by J Moorman 7/23/90
 ** original by Chuck Ehschlaeger
 */

#include "dxf2vect.h"


/* NOTE.  This does not seem to be currently used! */


#ifdef FOO
int dxf_add_boundaries(void)
{
    int count;
    char buf[300];
    int afd;
    FILE *fp;

    dxf_head.W = w;
    dxf_head.E = e;
    dxf_head.S = s;
    dxf_head.N = n;

    for (count = 0; count < num_open_layers; count++) {
	if (layers[count].type != DXF_ASCII)
	    continue;

	/* Current opened layers opened using G_fopen_append to find lines
	 * and append info.
	 * However, G_fopen_modify should be used to update header infomation
	 * which is located starting part of a file.
	 */
	sprintf(buf, "%s/%s_%s", GRASS_VECT_DIRECTORY, base_name,
		layers[count].name);

	fclose(layers[count].Map->dig_fp);
	layers[count].Map->dig_fp =
	    G_fopen_modify(buf, GRASS_VECT_COOR_ELEMENT);

	if (layers[count].Map->dig_fp == NULL) {
	    fprintf(stderr, "error: unable to open dig file\n");
	    exit(-1);
	}

	dig_write_head_binary(layers[count].fd, &dxf_head);
	fclose(layers[count].fd);
    }

    /* NOW FOR CLOSED LAYERS */
    for (count = 0; count < num_closed_layers; count++) {
#ifdef DEBUG
	fprintf(stderr, "%d closed_layers.name %s\n",
		closed_layers[count].name);
#endif
	if (closed_layers[count].status < 0)
	    continue;
	if (closed_layers[count].type != DXF_ASCII)
	    continue;

	/* temporarily reopen file */
	sprintf(buf, "%s/%s_%s", GRASS_VECT_DIRECTORY, base_name,
		closed_layers[count].name);
	fp = G_fopen_modify(buf, GRASS_VECT_COOR_ELEMENT);

	if (fp == NULL) {
	    fprintf(stderr, "error: unable to open dig file\n");
	    exit(-1);
	}
	/* print the extents to this file */
	dig_write_head_binary(fp, &dxf_head);
	fclose(fp);
    }
}
#endif

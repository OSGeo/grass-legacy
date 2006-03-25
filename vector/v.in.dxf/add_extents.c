/* 1/28/98 change from Jacques Bouchard <bouchard@onera.fr> */

#include "global.h"

int add_extents(void)
{
    int count;
    char buf[300];
    FILE *fp;

    head.plus.box.W = w;
    head.plus.box.E = e;
    head.plus.box.S = s;
    head.plus.box.N = n;

    for (count = 0; count < num_open_layers; count++) {
#ifdef DEBUG
	fprintf(stderr, "%d open_layers %s\n", count, layers[count].name);
#endif
	if (layers[count].type != DXF_ASCII)
	    continue;

	/* Current opened layers opened using G_fopen_append to find lines
	 * and append info.
	 * However, G_fopen_modify should be used to update header infomation
	 * which is located starting part of a file.
	 */
	sprintf(buf, "%s/%s_%s", GRASS_VECT_DIRECTORY, base_name,
		layers[count].name);

	fclose(layers[count].Map->dig_fp.file);
	layers[count].Map->dig_fp.file =
	    G_fopen_modify(buf, GRASS_VECT_COOR_ELEMENT);

	if (layers[count].Map->dig_fp.file == NULL) {
	    fprintf(stderr, "error: unable to open dig file\n");
	    exit(-1);
	}

	Vect_copy_head_data(&head, layers[count].Map);
	Vect_close(layers[count].Map);
    }
    for (count = 0; count < num_closed_layers; count++) {
	if (closed_layers[count].type != DXF_ASCII)
	    continue;
	if (closed_layers[count].status < 0)
	    continue;

#ifdef DEBUG
	fprintf(stderr, "open_layers %s\n", closed_layers[count].name);
#endif
	sprintf(buf, "%s/%s_%s", GRASS_VECT_DIRECTORY, base_name,
		closed_layers[count].name);

	/* temporarily reopen file */
	fp = G_fopen_modify(buf, GRASS_VECT_COOR_ELEMENT);
	closed_layers[count].Map->dig_fp.file = fp;


	if (fp == NULL) {
	    fprintf(stderr, "error: unable to open dig file %s\n", buf);
	    exit(-1);
	}

	/* print the extents to this file */
	Vect_copy_head_data(&head, closed_layers[count].Map);
	Vect_close(closed_layers[count].Map);
    }

    return 0;
}

int check_ext(double x, double y)
{
    if (y < s) {
	s = y;
    }
    if (y > n) {
	n = y;
    }
    if (x < w) {
	w = x;
    }
    if (x > e) {
	e = x;
    }

    return 0;
}

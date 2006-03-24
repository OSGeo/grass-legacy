/* 1/28/98 change from Jacques Bouchard <bouchard@onera.fr> */
/* written by J Moorman
 * 7/23/90
 */

#include "dxf2vect.h"

#define DEBUG

int dxf_reopen_layer(int type, int cl_count, int o_count)
{
    char buf[300];
    FILE *fp;

    sprintf(buf, "%s/%s_%s", GRASS_VECT_DIRECTORY, base_name,
	    closed_layers[cl_count].name);

    if (type == DXF_ASCII) {
	fp = G_fopen_append(buf, GRASS_VECT_COOR_ELEMENT);
    }
    else
	fp = G_fopen_append("dig_att", buf);

    if (fp == NULL) {
	fprintf(stderr, "error: unable to open dig file\n");
	exit(-1);
    }
    /* COPY INFO FROM CLOSED_LAYERS STRUCT TO LAYERS STRUCT */
    layers[o_count].fd = fp;
    layers[o_count].name = closed_layers[cl_count].name;
    layers[o_count].type = closed_layers[cl_count].type;
    layers[o_count].status = 0;	/* INDICATES OPENED MOST RECENTLY */
    layers[o_count].Map = closed_layers[cl_count].Map;
    if (type == DXF_ASCII) {
	layers[o_count].Map->dig_fp.file = fp;
	layers[o_count].fd = NULL;
    }

    /* CHANGE THE STATUS OF CLOSED FILE POSITION TO INACTIVE */
    closed_layers[cl_count].status = -2;


    return 0;
}

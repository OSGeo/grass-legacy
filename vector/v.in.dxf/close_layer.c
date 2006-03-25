#include "global.h"

int close_layer(int o_count)
{
    int found_flag = 0;		/* Reinitilized each time */
    int count;


    /* LOOKING FOR A LOCATION IN CLOSED_LAYERS[] THAT IS AVAILABLE */
    for (count = 0; count < num_closed_layers; count++) {
	if (closed_layers[count].status < 0) {	/*if available set to -2 in reopen */
	    found_flag = 1;
	    break;
	}
    }
    if (!num_closed_layers) {	/* this only happens the first time */
	closed_layers = (struct dxf_dig *)G_malloc(sizeof(struct dxf_dig));
	num_closed_layers++;
	found_flag = 1;
    }

    if (!found_flag) {
	closed_layers = (struct dxf_dig *)G_realloc(closed_layers,
						    (num_closed_layers +
						     1) *
						    sizeof(struct dxf_dig));
	count = num_closed_layers;
	num_closed_layers++;
    }

    /* PLACE INFO FROM LAYERS STRUCT INTO CLOSED_LAYERS STRUCT */

    /* dpg
     * closed_layers[count].name = G_store(layers[o_count].name);
     */

    closed_layers[count].name = layers[o_count].name;
    closed_layers[count].type = layers[o_count].type;
    closed_layers[count].status = layers[o_count].status;
    closed_layers[count].Map = layers[o_count].Map;

#ifdef DEBUG
    fprintf(stderr, "%d closing %s\n", count, closed_layers[count].name);
#endif

    if (layers[o_count].Map != NULL)	/* dig file */
	fclose(layers[o_count].Map->dig_fp.file);
    else
	fclose(layers[o_count].fd);

    return 0;
}

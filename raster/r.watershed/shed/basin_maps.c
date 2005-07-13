#include <stdlib.h>
#include "watershed.h"

int 
basin_maps (INPUT *input, OUTPUT *output)
{
	char	*mapset, map_layer[48];
	int	i;

	fprintf (stdout,"\n\nPlease indicate which map layers you wish to use in the lumped \n");
	fprintf (stdout,"parameter hydrologic/soil erosion model.  Continue inputing cell map \n");
	fprintf (stdout,"layers, one at a time, until all desired map layers are in.\n");
	fprintf (stdout,"You can have %s include a list of categories in each.\n", G_program_name());
	fprintf (stdout,"\nHit <return> at the map prompt to continue with %s\n", G_program_name());
	mapset = G_ask_old ("", map_layer,"cell","cell");
	while (mapset != NULL)	{
		output->num_maps++;
		if (output->num_maps == 1) 
			output->maps = (MAP *) G_malloc (sizeof(MAP));
		else	output->maps = (MAP *) G_realloc (output->maps, output->num_maps * sizeof(MAP));
		output->maps[output->num_maps-1].mapset = mapset;
		output->maps[output->num_maps-1].name = G_store (map_layer);
		output->maps[output->num_maps-1].do_cats = G_yes ("Complete list of categories?", 1);
		mapset = G_ask_old ("", map_layer,"cell","cell");
	}
	fprintf (stdout,"\nThe output from %s will be divided into watershed \n", G_program_name());
	fprintf (stdout,"basins.  There are two possible methods of tabulating the information: \n");
	fprintf (stdout,"1) by only including data pertaining to the basin itself, or 2) using \n");
	fprintf (stdout,"data from the basin, and all basins upstream of it.\n");
	do {
		fprintf (stdout,"\nWould you like the data organized:\n");
		fprintf (stdout,"1) Basin only\n2) Upstream only\n3) Both\nOR 0) to cancel program\n");
		fprintf (stdout,"\nYour choice: ");
		G_gets (map_layer);
		sscanf (map_layer, "%d", &i);
	} while (i > 3 || i < 0);
	switch (i) {
		case 0:	exit(0);
			break;
		case 1: output->do_basin = 1;
			output->do_accum = 0;
			break;
		case 2: output->do_basin = 0;
			output->do_accum = 1;
			break;
		case 3: output->do_basin = 1;
			output->do_accum = 1;
			break;
	}
	if (input->fast) {
		fprintf (stdout,"\nOK, %s should start running now ", RAM_NAME);
		fprintf (stdout,"using the following form:\n%s\n", input->com_line_ram);
	} else	{
		fprintf (stdout,"\nOK, %s should start running now ", SEG_NAME);
		fprintf (stdout,"using the following form:\n%s\n", input->com_line_seg);
	}

	return 0;
}

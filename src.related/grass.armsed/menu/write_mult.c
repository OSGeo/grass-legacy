
#define EXTERN

#include "menu.h"

write_mult(fd) FILE *fd;
{

    if (elev_name[0])
        fprintf(fd,"elev:        %s in %s\n", elev_name, elev_mapset);
    else
        fprintf(fd,"elev:        None\n");

    if (extthin_name[0])
        fprintf(fd,"extthin:     %s in %s\n", extthin_name, extthin_mapset);
    else
	fprintf(fd,"extthin:     None\n");

    if (basin_name[0])
        fprintf(fd,"basin:       %s in %s\n", basin_name, basin_mapset);
    else
        fprintf(fd,"basin:       None\n");

    if (part_name[0])
        fprintf(fd,"part:        %s in %s\n", part_name, part_mapset);
    else
        fprintf(fd,"part:        None\n");

    if (soils_name[0])
        fprintf(fd,"soils:        %s in %s\n", soils_name, soils_mapset);
    else
        fprintf(fd,"soils:        None\n");

    if (cover_name[0])
        fprintf(fd,"cover:        %s in %s\n", cover_name, cover_mapset);
    else
        fprintf(fd,"cover:        None\n");

    if (sim_title[0])
        fprintf(fd,"sim_title:   %s in %s\n", sim_title, sim_mapset);
    else
        fprintf(fd,"sim_title:   None\n");

    return;

}

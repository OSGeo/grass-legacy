#define EXTERN extern

#include "menu.h"

get_proj(proj_fd) FILE *proj_fd;
{

    int i;
    int error;

    int basin_flag=0;
    int extthin_flag=0;
    int part_flag=0;
    int elev_flag=0;
    char buf[80];

    char readbuf[1024];

    sprintf(buf,"Error reading project file -- incorrect format\n");

    fgets(readbuf,1024,proj_fd);

    do 
    {
        if (sscanf (readbuf,"elev:%[^\n]", elev_name) == 1)
        {
            if (elev_flag++)
            {
                G_fatal_error(buf);
                exit(-2);
            }
            continue;
        }
        if (sscanf (readbuf,"basin:%[^\n]", basin_name) == 1)
        {
            if (basin_flag++)
            {
                G_fatal_error(buf);
                exit(-2);
            }
            continue;
        }
        if (sscanf (readbuf,"extthin:%[^\n]", extthin_name) == 1)
        {
            if (extthin_flag++)
            {
                G_fatal_error(buf);
                exit(-2);
            }
            continue;
        }
    } while (fgets(readbuf,1024,proj_fd));

    G_strip(elev_name);
    G_strip(basin_name);
    G_strip(extthin_name);

    error = 0;

    elev_mapset =  G_find_cell2(elev_name,"");
    if (!elev_mapset)
    {
	fprintf(stderr,"Error -- map layer [%s] not found\n", elev_name);
	error = 1;
    }

    extthin_mapset =  G_find_cell2(extthin_name,"");
    if (!extthin_mapset)
    {
	fprintf(stderr,"Error -- map layer [%s] not found\n", extthin_name);
	error = 1;
    }

    basin_mapset =  G_find_cell2(basin_name,"");
    if (!basin_mapset)
    {
	fprintf(stderr,"Error -- map layer [%s] not found\n", basin_name);
	error = 1;
    }

    if (error)
    {
	fprintf(stderr,
	  "Cannot continue with this project as map layers are missing\n");

	printf("\nHit return to continue...");
	G_gets(buf);
	exit(-2);
    }

    return;
}


#define EXTERN

#include "menu.h"

read_mult(fd) FILE *fd;
{

    char buf[128];
    char label[25];
    char filename[40];
    char directory[40];
    int warn;
    char in[20];
    char mapset[31];

    warn = 0;

    if (fgets(buf,sizeof(buf),fd) == NULL)
    {
        fclose(fd);
        return -1;
    }

    do
    {

        sscanf(buf,"%[^:]:%[^\n]", label, filename);
        if (! strncmp(label, "elev", 4))
	{
            G_strip(filename);
            if (strncmp(filename,"None",4))
            {
	        *in = 0;
		*mapset = 0;
	        sscanf(filename,"%s %s %s",elev_name, in, mapset);
                elev_mapset = G_find_cell2(elev_name,mapset);
                if (!elev_mapset)
                {
                    fprintf(stderr,
	            "Warning -- cannot find elevation map layer [%s] in [%s]\n",
                      elev_name, mapset);
		    elev_mapset = G_malloc(31);
		    strcpy(elev_mapset,mapset);
                    warn = 1;
		}
            }
            else
                elev_name[0] = 0;
	}
        else if (! strncmp(label, "extt", 4))
	{
            G_strip(filename);
            if (strncmp(filename,"None",4))
            {
	        *in = 0;
		*mapset = 0;
	        sscanf(filename,"%s %s %s",extthin_name, in, mapset);
                extthin_mapset = G_find_cell2(extthin_name,mapset);
                if (!extthin_mapset)
                {
                    fprintf(stderr,
	            "Warning -- cannot find streams map layer [%s] in [%s]\n",
                      extthin_name, mapset);
		    extthin_mapset = G_malloc(31);
		    strcpy(extthin_mapset,mapset);
                    warn = 1;
		}
            }
            else
                extthin_name[0] = 0;
	}
        else if (! strncmp(label, "basi", 4))
	{
            G_strip(filename);
            if (strncmp(filename,"None",4))
            {
	        *in = 0;
		*mapset = 0;
	        sscanf(filename,"%s %s %s",basin_name, in, mapset);
                basin_mapset = G_find_cell2(basin_name,mapset);
                if (!basin_mapset)
                {
                    fprintf(stderr,
	            "Warning -- cannot find sub-basin map layer [%s] in [%s]\n",
                      basin_name, mapset);
		    basin_mapset = G_malloc(31);
		    strcpy(basin_mapset,mapset);
                    warn = 1;
		}
            }
            else
                basin_name[0] = 0;
	}
        else if (! strncmp(label, "part", 4))
	{
            G_strip(filename);
            if (strncmp(filename,"None",4))
            {
	        *in = 0;
		*mapset = 0;
	        sscanf(filename,"%s %s %s",part_name, in, mapset);

                sprintf(directory,"multsed/input/%s",part_name);

                part_mapset = G_find_cell2(part_name,mapset);
                if (!part_mapset)
                {
                    fprintf(stderr,
	            "Warning -- cannot find sub-basin map layer [%s] in [%s]\n",
                      part_name, mapset);
		    part_mapset = G_malloc(31);
		    strcpy(part_mapset,mapset);
                    warn = 1;
		}
            }
            else
                part_name[0] = 0;
	}
        else if (! strncmp(label, "soil", 4))
	{
            G_strip(filename);
            if (strncmp(filename,"None",4))
            {
	        *in = 0;
		*mapset = 0;
	        sscanf(filename,"%s %s %s",soils_name, in, mapset);
                soils_mapset = G_find_cell2(soils_name,mapset);
                if (!soils_mapset)
                {
                    fprintf(stderr,
	            "Warning -- cannot find soils map layer [%s] in [%s]\n",
                      soils_name, mapset);
		    soils_mapset = G_malloc(31);
		    strcpy(soils_mapset,mapset);
                    warn = 1;
		}
            }
            else
                soils_name[0] = 0;
	}
        else if (! strncmp(label, "cove", 4))
	{
            G_strip(filename);
            if (strncmp(filename,"None",4))
            {
	        *in = 0;
		*mapset = 0;
	        sscanf(filename,"%s %s %s",cover_name, in, mapset);
                cover_mapset = G_find_cell2(cover_name,mapset);
                if (!cover_mapset)
                {
                    fprintf(stderr,
	            "Warning -- cannot find cover map layer [%s] in [%s]\n",
                      cover_name, mapset);
		    cover_mapset = G_malloc(31);
		    strcpy(cover_mapset,mapset);
                    warn = 1;
		}
            }
            else
                cover_name[0] = 0;
	}
        else if (! strncmp(label, "sim_", 4))
	{
            G_strip(filename);
            if (strncmp(filename,"None",4))
            {
	        *in = 0;
		*mapset = 0;
	        sscanf(filename,"%s %s %s",sim_title, in, mapset);
                sim_mapset = G_find_file2(directory,sim_title,mapset);
                if (!sim_mapset)
                {
                    fprintf(stderr,
	            "Warning -- cannot find simulation directory [%s] in [%s]\n",
                      sim_title, mapset);
		    sim_mapset = G_malloc(31);
		    strcpy(sim_mapset,mapset);
                    warn = 1;
		}
            }
            else
                sim_title[0] = 0;
	}

    } while (fgets(buf, sizeof(buf), fd));

    if (warn)
    {
        printf("\nHit return to continue...");
        G_gets(buf);
    }

    return 0;

}

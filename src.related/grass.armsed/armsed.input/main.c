#define EXTERN

#include "inter.h"

/*********************************************************************
 * usage if creating new simulation input:
 *  Garmsed.input -n div=layerinput streams=layerinput sim=simtitle
 *
 * where div is the map layer showing partitions of the waterhsed
 * (from 'watershed' program) and streams is the extended stream
 * network.  simtitle is the desired name for the simulation title,
 * and must fall within the limits of a legal GRASS filename.
 * The statistics file (output from Garmsed.stat) for the partitioned
 * map layer must be available, as well as the drainage pattern file
 * (from step 'coding stream segments' in program 'watershed')
 * for the stream network map layer.
 *
 * usage if modifying an old simulation input:
 *  Garmsed.input -o sim=simtitle
 * NOTE: above option is not yet implemented!
 *
 * all other input is interactive and is primarily entered through
 * the use of V_ask menus
 *********************************************************************/

main(argc,argv) char *argv[];
{

    char div_name[50], *div_mapset;
    int div_flag=0;
    int extthin_flag=0;
    int old_flag=0;
    int sim_flag=0;
    int flagcnt=0;
    char *mapset;
    char answer;
    char get_answer();
    int i;
    struct Cell_head window;
    FILE *proj_fd;

    char *stats_mapset;
    FILE *stats_fd;
    char readbuf[1024];
    char command[1024];

    G_gisinit(argv[0]);

    old_flag=0;
    wat_flag = 0;
    rain_flag = 0;
    sed_flag = 0;
    sed_default = 0;
    depres_flag = 0;
    stats_flag = 0;
    NWS = 0;
    NPL = 0;
    num_sizes = 0;

/* parse command line */
    for (i=1; i< argc; i++)
    {
	if (strcmp(argv[i], "-n") == 0)
	{
	    flagcnt++;
	    continue;
	}

	if (strcmp(argv[i], "-o") == 0)
	{
	    old_flag=1;
	    flagcnt++;
	    continue;
	}

        if (sscanf(argv[i], "div=%[^\n]", div_name) == 1)
        {
            if (div_flag++)
            {
                fprintf(stderr,"problem with division file\n");
                usage(argv[0]);
            }
	    continue;
        }

        if (sscanf(argv[i], "streams=%[^\n]", extthin_name) == 1)
        {
            if (extthin_flag++)
            {
                fprintf(stderr,"problem with streams file\n");
                usage(argv[0]);
            }
	    continue;
        }

        if (sscanf(argv[i], "sim=%[^\n]", sim_title) == 1)
        {
            if (sim_flag++)
            {
                fprintf(stderr,"problem with simulation title\n");
                usage(argv[0]);
            }
	    continue;
        }
        fprintf(stderr,"illegal argument\n");
        usage(argv[0]);
        exit(-1);
    }

    if ((flagcnt != 1) || (!sim_flag))
    {
	usage(argv[0]);
	exit(-1);
    }

    if (!old_flag)
    {
        if ((!div_flag) || (!extthin_flag))
        {
            usage(argv[0]);
            exit(-1);
	}
    }
 
    if (old_flag)
    {
	fprintf(stderr,"option for -o flag not available\n");
	exit(-4);
    }

/* note that div_name here is the completely partitioned map layer
   including partitioning into left/right half planes */

/*
    if (old_flag)
    {
	stats_mapset = G_find_file2("multsed/input",div_name,"");
	if (!stats_mapset)
	{
	    fprintf(stderr,
	      "No existing input files for partitioned map layer [%s]\n",
	      div_name);
	    fprintf("  cannot use modify option\n");
            exit(2);
	}
    }
    else
*/
    {
        stats_mapset = G_find_file2("multsed/stats",div_name,"");
        if (!stats_mapset)
        {
            fprintf(stderr,"Cannot find stats file for [%s]\n",div_name);
            fprintf(stderr,"  run statistics collection routine and try again\n");
            exit(2);
	}
	stats_flag = 1;
    }

    sprintf(dir,"multsed/input/%s",div_name);

    temp1_name = G_tempfile();
    temp2_name = G_tempfile();
    temp3_name = G_tempfile();
    temp4_name = G_tempfile();

    temp1_fd = fopen(temp1_name,"w");
    if (!temp1_fd)
    {
        fprintf(stderr,"Error in opening temporary input file\n");
        exit(9);
    }

    temp2_fd = fopen(temp2_name,"w");
    if (!temp2_fd)
    {
        fprintf(stderr,"Error in opening temporary input file\n");
        exit(9);
    }

    temp3_fd = fopen(temp3_name,"w");
    if (!temp3_fd)
    {
        fprintf(stderr,"Error in opening temporary input file\n");
        exit(9);
    }

    temp4_fd = fopen(temp4_name,"w");
    if (!temp4_fd)
    {
        fprintf(stderr,"Error in opening temporary input file\n");
        exit(9);
    }

/*
    if (old_flag)
    {
        if (!G_find_file2(dir,sim_title,""))
        {
            fprintf(stderr,
              "Unable to find any simulation files for division file [%s]\n",
              div_name);
	    exit(-3);
        }
    }
*/

    sprintf(path,"%s/%s/%s/%s/%s", G_gisdbase(), G_location(), G_mapset(),
      dir, sim_title);

    sprintf(tape1_name,"%s/tape1", path);
    sprintf(tape2_name,"%s/tape2", path);
    sprintf(tape3_name,"%s/tape9", path);
    sprintf(tape4_name,"%s/tape10", path);

/*
    if (old_flag)
    {

        strcat(dir,"/");
        strcat(dir,sim_title);

	oldsim();

    }
    else
*/
    {

        extthin_mapset = G_find_file2("watershed/drain",extthin_name,"");
        if (!extthin_mapset)
        {
            fprintf(stderr,"Error -- drainage file missing for [%s]\n",extthin_name);
            exit(2);
        }

        stats_fd = G_fopen_old("multsed/stats",div_name,stats_mapset);
        if (!stats_fd)
        {
            fprintf(stderr,"Error -- cannot open stats file: %s\n",div_name);
            exit(2);
        }

        strcat(dir,"/");
        strcat(dir,sim_title);

	newsim(stats_fd);
	fclose(stats_fd);

    }

    fprintf(stdout,"Do you want to run ARMSED with these inputs? [y/n] ");
    answer = get_answer();

    if (answer == 'y')
    {
        sprintf(command,"cd %s; armsed1; armsed2; armsed3", path);
        system(command);
    }

}

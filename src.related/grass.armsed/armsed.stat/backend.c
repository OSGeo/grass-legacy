/* %W% %G% */
#define EXTERN
#define fabs(x) ((x)<0?-(x):(x))

#include "stat.h"

/**********************************************************************
 * input is from command line.
 * usage: 
 *   armsed.stat -w div=file [soils=file] [cover=file] [stream=file] [elev=file]
 *
 * if divisions are by subwatershed (using the "Larry Band" process
 *   of development), the -w flag should be used.  Statistics will
 *   be collected and output by half plane, and statistics will
 *   also be given for the channel segment
 *
 * if divisions are "random", the -w flag should NOT be given.
 * Statistics will be collected and output by division number.
 *
 * Based on the divisions provided in the division file, stats will
 * be collected for each piece.  If an elevation file is provided, an
 * average slope will be calculated; if a soils file is provided,
 * statistics will be given for each piece based on percents of 
 * various soil types; and if a cover file is given, percents of
 * cover will be given.  If the given division file has been created
 * from a subwatershed division file, some statistics will be provided
 * for the channel segments as well.
 *********************************************************************/

    FILE *fslope();


main (argc, argv) char *argv[];
{

    int div_fd = -1;
    int cover_fd = -1 ;
    int soils_fd = -1;
    int elev_fd = -1;
    int nodes_fd = -1;
    FILE *proj_fd;

    int div_flag = 0;
    int elev_flag = 0;
    int proj_flag = 0;
    int wat_flag = 0;

    char elev_name[50];
    char proj_name[50];
    char cover_name[50];
    char soils_name[50];
    char div_name[50];
    char extthin_name[50];

    char *elev_mapset;
    char *slope_mapset;
    char *nodes_mapset;
    char *div_mapset;
    char *soils_mapset;
    char *cover_mapset;

    FILE *node_fd = NULL;
    FILE *slope_fd = NULL;
    FILE *table_fd;
    char soil_table[150];

    int ret_num;
    int index;

    float *slope_row,*sl;

    typedef struct
    {
        int r1;
        int c1;
        int num;
        int rlast;
        int clast;
        double chlength;
        double chslope;
    } topo;

    topo *node;

    int nnodes;

    typedef struct
    {
        int leftnum;
        int rightnum;
    } recode;

    recode *planes;
    int max,planenum;

    CELL *div_cell, *dc;
    CELL *cover_cell, *cc ;
    CELL *soils_cell, *sc ;
    CELL *elev_cell, *e1, *e2;
    int i;
    int seg_num;
    int value;

    char buf[300];
    char *mapset;
    int col, row;
    int datacount;
    static int catnum;
    int *table;          /* internal "reclass" table */
    double flowlength;  /* overland flow length for plane */
    double avslope;    /* average overland slope for plane */
    double out_length;

    G_gisinit (argv[0]);

    soils_flag=0;
    cover_flag=0;
    nodes_flag=0;

/* parse command line */
    for (i = 1; i < argc; i++)
    {
	if (strcmp (argv[i], "-w") == 0)
	{
	    wat_flag = 1;
	    continue;
	}
        if (sscanf (argv[i], "project=%[^\n]", proj_name) == 1)
        {
            if (proj_flag++)
            {
                fprintf(stderr,"problem with project file\n");
                usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "div=%[^\n]", div_name) == 1)
        {
            if (div_flag++)
            {
                fprintf(stderr,"problem with division file\n");
                usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "soils=%s", soils_name) == 1)
        {
            if (soils_flag++)
            {
                fprintf(stderr,"problem with soils file\n");
                usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "cover=%s", cover_name) == 1)
        {
            if (cover_flag++)
            {
                fprintf(stderr,"problem with cover file\n");
                usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "elev=%s", elev_name) == 1)
        {
            if (elev_flag++)
            {
                fprintf(stderr,"problem with elevation file\n");
                usage(argv[0]);
            }
            continue;
        }
        if (sscanf (argv[i], "stream=%[^\n]", extthin_name) == 1)
        {
            if (nodes_flag++)
            {
                fprintf(stderr,"problem with stream file\n");
                usage(argv[0]);
            }
            continue;
        }
        fprintf(stderr,"illegal argument\n");
        usage(argv[0]);
        exit(1);
    }

    if (!div_flag && !proj_flag)
    {
        fprintf(stderr,"Need either project file or division file\n");
         usage(argv[0]);
         exit(1);
    }

    if (!soils_flag && !cover_flag && (*elev_name == 0))
    {
        fprintf(stderr,"Error -- no products can be calculated without");
        fprintf(stderr," at least one of the following:\n");
        fprintf(stderr,"   soils file, cover file, elevation file\n");
        exit(5);
    }

/* check division file existence */
    mapset = G_find_cell2 (div_name, "");
    if (!mapset)
    {
        sprintf (buf, "division file [%s] not found\n", div_name);
        G_fatal_error (buf);
        exit(1);
    }

/* set the window from the header for the division file */
    if (G_get_cellhd (div_name, mapset, &window) < 0)
    {
	sprintf(buf, "problem with cell header for [%s]\n", div_name);
	G_fatal_error(buf);
	exit(1);
    }
    G_set_window (&window);

    nrows = G_window_rows();
    ncols = G_window_cols();

    datacount=0;

    if ((catnum = G_number_of_cats(div_name,mapset))<0)
    {
        fprintf(stderr,"error getting number of cats for [%s]\n", div_name);
        exit(3);
    }

    table = (int *)G_calloc((catnum+1),sizeof(int));
    G_zero(table,(catnum+1)*sizeof(int));

/* open the division file for reading */
    div_fd = G_open_cell_old (div_name, mapset);
    if (div_fd < 0)
    {
        fprintf(stderr,"Problem opening division file [%s]\n", div_name);
        exit(1);
    }
    div_cell = G_allocate_cell_buf();

/* check for file linking divide cell file with nodes file and elev file */

    if (nodes_flag)
    {
        nodes_mapset = G_find_file2("watershed/nodes",extthin_name,"");
        if (!nodes_mapset)
        {
            fprintf(stderr,"unable to find nodes file %s\n", extthin_name);
            exit(7);
        }

        node_fd = G_fopen_old("watershed/nodes",extthin_name,nodes_mapset);
        if (!node_fd)
        {
            fprintf(stderr,"Error -- couldn't open nodes file [%s]\n",
             extthin_name);
            exit(7);
        }

        if (fscanf (node_fd, "%d\n", &nnodes) != 1)
        {
            fprintf(stderr,"error reading number of nodes\n");
            exit(6);
        }

        max = nnodes + 1;

        planes = (recode *)G_calloc(nnodes+1, sizeof(recode));
	G_zero(planes,(nnodes+1)*sizeof(recode));
        node = (topo *)G_calloc(nnodes+1,sizeof(topo));
	G_zero(node,(nnodes+1)*sizeof(topo));

        for (i=1; i<=nnodes; i++)
        {
            if (fscanf(node_fd,"%*d %d %d %d %d %d %*d %lf\n",
              &node[i].r1,&node[i].c1,&node[i].num,
              &node[i].rlast,&node[i].clast, &node[i].chlength) != 6)
            {
                fprintf(stderr,"Error in reading nodes file\n");
                exit(7);
            }
        }
    }

    if (elev_flag)
    {
        elev_mapset = G_find_cell2(elev_name,"");
        if (!elev_mapset)
        {
            fprintf(stderr,"warning -- elevation file [%s] not", elev_name);
            fprintf(stderr,"found...no slope products are possible\n");
        }
        else
        {
            elev_fd = G_open_cell_old(elev_name,elev_mapset);
            if (elev_fd < 0)
            {
                fprintf(stderr,"error opening cell file %s\n",
                  elev_name);
                exit(6);
            }

            slope_fd = fslope(elev_fd);

            if (nodes_flag)
            {
                elev_cell = G_allocate_cell_buf();
                e1 = (CELL *)G_malloc(sizeof(CELL));
                e2 = (CELL *)G_malloc(sizeof(CELL));

                for (i=1; i<=nnodes; i++)
                {
                    if (!G_get_map_row(elev_fd,elev_cell,node[i].r1))
                    {
                        fprintf(stderr,"Error reading elevation file\n");
                        exit(6);
                    }
                    *e1 = *(elev_cell+node[i].c1);

                    if (!G_get_map_row(elev_fd,elev_cell,node[i].rlast))
                    {
                        fprintf(stderr,"Error reading elevation file\n");
                        exit(6);
                    }
                    *e2 = *(elev_cell+node[i].clast);

                    if (node[i].chlength == 0)
                        node[i].chslope = 0;
                    else
                    {
                        node[i].chslope = (double)(*e1 - *e2) /
                            node[i].chlength;
                        node[i].chslope = fabs(node[i].chslope);
                    }
                }
            }

            G_close_cell(elev_fd);
        }

    }


/* create internal reclass table for this routine */

/* if nodes file exists, store cat values for left and right planes */

    for (row=0; row<nrows; row++)
    {
        G_get_map_row(div_fd,div_cell,row);
        dc = div_cell;
        for (col=0; col<ncols; col++)
        {
            if (*dc>0)
            {
                if (nodes_flag)
                {
                    planenum = (*dc)%max;
                    if ((planes[planenum].leftnum != *dc) &&
                        (planes[planenum].rightnum != *dc))
                    {
                        if (planes[planenum].leftnum == 0)
                            planes[planenum].leftnum = *dc;
                        else if (planes[planenum].rightnum == 0)
                            planes[planenum].rightnum = *dc;
                        else
                        {
                            fprintf(stderr,"error from division file:");
                            fprintf(stderr," more than 2 half planes\n");
                            fprintf(stderr,"leftnum %d  rightnum %d \n",
                              planes[planenum].leftnum,planes[planenum].rightnum);
                            fprintf(stderr,"planenumber %d  new value %d\n",
                              planenum, *dc);
                            exit(7);
                        }
                    }
                }

                if (table[*dc] == 0) table[*dc] = ++datacount;
            }
            dc++;
        }
    }

/*    for (i=0; i<=catnum; i++)
      fprintf(stdout,"table[%d] is %d \n",i,table[i]); */

    watshed = (struct subws *)G_calloc(datacount,sizeof(struct subws));
    G_zero(watshed,datacount*sizeof(struct subws));

    if (soils_flag)
    {
        mapset = G_find_file2 ("cell", soils_name, "");
        if (!mapset)
        {
            sprintf (buf, "soils file [%s] not found\n", soils_name);
/* may want to allow for this case later, asking if they want the
   other stats collected anyhow */
            G_fatal_error (buf);
            exit(1);
        }
        soils_fd = G_open_cell_old(soils_name,mapset);
        if (soils_fd<0)
        {
            fprintf(stderr,"Error opening soils file %s\n",soils_name);
            exit(3);
        }

        ret_num = G_number_of_cats(soils_name,mapset);
        if (ret_num > 11)
        {
            fprintf(stderr,"error in soils file -- must have ");
            fprintf(stderr,"less than or equal to 11 cats\n");
            exit(7);
        }
        soils_cell = G_allocate_cell_buf();
        G_zero_cell_buf (soils_cell);
    }
    if (cover_flag)
    {
        mapset = G_find_file2 ("cell", cover_name, "");
        if (!mapset)
        {
            sprintf (buf, "cover file [%s] not found\n", cover_name);
/* may want to allow for this case later, asking if they want the
   other stats collected anyhow */
            G_fatal_error (buf);
            exit(1);
        }
        cover_fd = G_open_cell_old (cover_name,mapset);
        if (cover_fd<0)
        {
            fprintf(stderr,"Error opening cover file %s\n",cover_name);
            exit(3);
        }
        ret_num = G_number_of_cats(cover_name,mapset);
        if (ret_num > 3)
        {
            fprintf(stderr,"error in cover file -- must have ");
            fprintf(stderr,"less than or equal to 3 cats\n");
            exit(7);
        }
        cover_cell = G_allocate_cell_buf();
        G_zero_cell_buf (cover_cell);
    }


    if (elev_flag)
        slope_row = (float *)G_calloc(ncols,sizeof(float));

    for (row=0; row<nrows; row++)
    {
        if (!G_get_map_row(div_fd,div_cell,row))
        {
            fprintf(stderr,"Error reading division file %s\n",div_name);
            exit(6);
        }
        dc = div_cell;

        if (soils_flag)
        {
            if (!G_get_map_row(soils_fd,soils_cell,row))
            {
                fprintf(stderr,"Error reading soils file %s\n",soils_name);
                exit(6);
            }
            sc = soils_cell;
        }

        if (cover_flag)
        {
            if (!G_get_map_row(cover_fd,cover_cell,row))
            {
                fprintf(stderr,"Error reading cover file %s\n",cover_name);
                exit(6);
            }
            cc = cover_cell;
        }

        if (elev_flag)
        {
            if (fread(slope_row,sizeof(float),ncols,slope_fd) != ncols)
            {
                fprintf(stderr,"error reading slope file\n");
                exit(6);
            }
            sl = slope_row;
        }

        for (col=0; col<ncols; col++)
        {
            value = table[*dc++];
            watshed[value].count++;
            if (elev_flag) watshed[value].slope += (double)*sl++;
            if (soils_flag) watshed[value].soils[*sc++]++;
            if (cover_flag) watshed[value].cover[*cc++]++;
        }
    }

    if (elev_flag) fclose(slope_fd);
    if (soils_flag) G_close_cell(soils_fd);
    if (cover_flag) G_close_cell(cover_fd);
    G_close_cell(div_fd);

    output_fd = G_fopen_new("multsed/stats",div_name);
    if (!output_fd)
    {
        fprintf(stderr,"error in opening output file\n");
        exit(6);
    }

    sprintf(soil_table,"%s/etc/soils.table",G_gisbase());

    table_fd = fopen(soil_table,"r");
    if (!table_fd)
    {
        fprintf(stderr,"Error:  soils table file missing\n");
        exit(8);
    }

    for (i=0; i<11; i++)
    {
        if (fscanf(table_fd,"%f %d %f %f %f %f %f %f\n",&Ktable[i],
          &PItable[i], &Doftable[i],&Kwtable[i],&Yctable[i],
          &Portable[i], &fin1table[i],&fin2table[i]) != 8)
        {
            fprintf(stderr,"error reading soils table file\n");
            exit(8);
        }
    }

    fclose(table_fd);

/*    for (i=0; i<=datacount; i++)
    {
        fprintf(stdout,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %lf\n",
          watshed[i].count,watshed[i].soils[0],watshed[i].soils[1],watshed[i].soils[2],
          watshed[i].soils[3],watshed[i].soils[4],watshed[i].soils[5],
          watshed[i].soils[6],watshed[i].soils[7],watshed[i].soils[8],
          watshed[i].soils[9],watshed[i].soils[10],watshed[i].soils[11],
          watshed[i].cover[0],watshed[i].cover[1],watshed[i].cover[2],
          watshed[i].cover[3],watshed[i].slope);
    } */

    if (wat_flag)
    {

        fprintf(output_fd,"watershed\n");

        fprintf(output_fd,"number of segments:  %d\n",nnodes);

        for (i=1; i<=nnodes; i++)
        {
            fprintf(output_fd,"segment:  %d\n",i);

/*  look for correct channel segment */
            for (seg_num = 1; seg_num<=nnodes; seg_num++)
            {
                if (node[seg_num].num == i)
                    break;
            }
            if ((seg_num == nnodes) && (node[seg_num].num != i))
            {
                fprintf(stderr,"Error in reading nodes file - segment");
                fprintf(stderr," number %d not found\n",i);
            }

/*  convert meters to feet */

            out_length = node[seg_num].chlength * 3.28084;
            fprintf(output_fd,"chlength:  %lf\n",out_length);
            fprintf(output_fd,"chslope:  %lf\n",node[seg_num].chslope);

            value = planes[i].leftnum;
            value = table[value];
            fprintf(output_fd,"left plane\n");

            if (value != 0)
            {
                if (watshed[value].count == 0)
                {
                    fprintf(stderr,"error -- count for renumbered division %d is zero\n", value);
                    exit(8);
                }
                if (slope_fd)
                {
                    avslope = watshed[value].slope / (double)watshed[value].count;
                    fprintf(output_fd,"olslope: %lf\n",avslope);
                }
                flowlength = ((double)watshed[value].count * window.ew_res
                  * window.ns_res) / node[i].chlength;

/* convert meters to feet */

                out_length = flowlength * 3.28084;
                fprintf(output_fd,"flow length: %lf\n",out_length);
                output(value);
            }
            else
                fprintf(output_fd,"no data\n");

            value = planes[i].rightnum;
            value = table[value];
            fprintf(output_fd,"right plane\n");

            if (value != 0)
            {
                if (watshed[value].count == 0)
                {
                    fprintf(stderr,"error -- count for renumbered division %d is zero\n", value);
                    exit(8);
                }
                if (slope_fd)
                {
                    avslope = watshed[value].slope / (double)watshed[value].count;
                    fprintf(output_fd,"olslope: %lf\n",avslope);
                }
                flowlength = ((double)watshed[value].count * window.ew_res
                  * window.ns_res) / node[i].chlength;

/* convert meters to feet */

                out_length = flowlength * 3.28084;
                fprintf(output_fd,"flow length: %lf\n",out_length);
                output(value);
            }
            else
                fprintf(output_fd,"no data\n");
        }
    }
    else
    {
        fprintf(output_fd,"division\n");

        fprintf(output_fd,"number of divisions:  %d\n",datacount);

        for (i=1; i<=catnum; i++)
        {
            if (table[i] > 0)
            {
                fprintf(output_fd,"segment:  %d\n",i);

                value = table[i];

                if (watshed[value].count == 0)
                {
                    fprintf(stderr,"error -- count for renumbered division %d is zero\n", value);
                    exit(8);
                }
            
                if (elev_flag)
                {
                    avslope = watshed[value].slope /
                      (double)watshed[value].count;
                    fprintf(output_fd,"olslope: %lf\n",avslope);
                }
                output(value);
            }
        }

        fclose(output_fd);
    }

/*    for (i=0; i<=datacount; i++)
    {
        fprintf(stdout,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %lf\n",
          watshed[i].count,watshed[i].soils[0],watshed[i].soils[1],watshed[i].soils[2],
          watshed[i].soils[3],watshed[i].soils[4],watshed[i].soils[5],
          watshed[i].soils[6],watshed[i].soils[7],watshed[i].soils[8],
          watshed[i].soils[9],watshed[i].soils[10],watshed[i].soils[11],
          watshed[i].cover[0],watshed[i].cover[1],watshed[i].cover[2],
          watshed[i].cover[3],watshed[i].slope);
    } */

    exit(0);

}

/*
**  Dave Gerdes   		rewritten J Moorman 
**  3 March 1990		18 June 1990
**
**  Building unlabelled contours
**  
**  This program reads in a cell file (raster) and creates a dig file
**  (vector) containing contours at intervals specified on the command
**  line.	
**
**  The command line takes 4 arguments:
**	mkcontour  cell_filename   dig_filename  contour_interval
**
**  where:
**	mkcontour is the program name
**	cell_filename is cell file that exist in current mapset
**	dig_filename is the name you provide for the programs output
**	contour_interval is an integer value indicating steps between
**			 contours to be recorded.
*/


		    

#include "contour.h"




main (argc, argv)
    char *argv[];
{
    char *cell_file, *dig_file, *att_file, *cell_mapset;
    int	 cell_fd;
    FILE *dig_fp, *att_fp;
    int	 interval; /* the interval between contours entered on command line*/
    struct Cell_head	window; /* info for active window */
	
    /* check the number of command line arguments */
    if (argc != 4)
    {
	fprintf (stderr, "USAGE: %s  cellfile  dig_outputfile ", argv[0]);
	fprintf (stderr, " contour_interval \n");
	exit(1);
    }
    G_gisinit (argv[0]);

    /* check to see if cell_file exists in current mapset */
    cell_file = argv[1];
    if ((cell_mapset = G_find_cell(cell_file,"")) == NULL)
    {
	fprintf (stderr, "%s:  %s: cellfile not found\n", argv[0], cell_file);
    }
    /* open cellfile */
    cell_fd = G_open_cell_old (cell_file, cell_mapset);
    if (cell_fd < 0)
    {
	fprintf(stderr,"ERROR: unable to open cell file %s\n",cell_file);
	exit(1);
    }

    dig_file = att_file = argv[2];
    /* command line name for dig file will overwrite existing file */
    /*check if output file name provided on command line is ok for grass files*/
    if (G_legal_filename(dig_file) != 1)
    {
	fprintf(stderr,"%s is not a legal filename for grass\n",dig_file);
	exit(1);
    }
    /* open digfile */
    dig_fp = G_fopen_vector_new(dig_file);
    if (dig_fp == NULL)
    {
	fprintf(stderr,"ERROR: unable to open new dig file %s\n",dig_file);
	exit(1);
    }

    /* command line output file name will overwrite existing attribute file */ 
    /* open attribute file*/
    if (G_legal_filename(att_file) != 1)
    {
	fprintf(stderr,"%s is not a legal filename for grass\n",att_file);
	exit(1);
    }
    /* open attribute file*/
    att_fp = G_fopen_new("dig_att",att_file);
    if (att_fp == NULL)
    {
	fprintf(stderr,"ERROR: unable to open new att file %s\n", 
		 att_file);
	exit(1);
    }

    interval = atoi(argv[3]);

    cntr_init_functns(dig_fp,cell_mapset,cell_file,interval,&window);
    /*&routines to build contours*/
    cntr_read_data (cell_fd,dig_fp,att_fp,interval,&window); 
     
    /* close files and terminate Level 1 access */
    G_close_cell(cell_fd);
    fclose(dig_fp);
    fclose(att_fp);

    exit(0);
}

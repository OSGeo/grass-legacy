/*	January, 1991	Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	r.weighted.cn()

	This program will output a map layer with weighted curve number
	assigned to all the cells. The input for the program is the
	distributed curve number map.
*/

#include <stdio.h>
#include <math.h>
#include "gis.h"
#include <string.h>
#include <stdio.h>


#define KILOMETERS(C,R) R*C/1000000.0
#define ACRES(C,R)      KILOMETERS(C,R) * 247.1000

main(argc,argv)
int	argc;
char	*argv[];
{
	double		square_meters, weighted_cn;
	double		acres, acres0, value, total_cn;
	char		*cn_mapset, *weighted_cn_mapset;
	char		*this_mapset, buf[100], title[100];
	char		weighted_cn_name[40], cn_name[40];
	CELL		*cn_cell, *weighted_cn_cell, c;
	int		ncols, nrows, row;
	struct Categories cats;
	struct Cell_head window;
	struct Cell_stats statf;
	int		cn_id, weighted_cn_id, i; 
	int		cn_flag, weighted_cn_flag;
	long		count;
	char		*G_get_cat();
	struct Option *parm1, *parm2;
	
        parm1 = G_define_option() ;
        parm1->key        = "input" ;
        parm1->type       = TYPE_STRING ;
        parm1->required   = YES;
        parm1->gisprompt  = "any,cell,raster" ;
        parm1->description= "curve_number_map" ;

        parm2 = G_define_option() ;
        parm2->key        = "output" ;
        parm2->type       = TYPE_STRING ;
        parm2->required   = YES;
        parm2->gisprompt  = "any,cell,raster" ;
        parm2->description= "weighted_cn_map" ;

/*  Initialize the GRASS environment variables */
	G_gisinit(argv[0]);
        if (G_parser(argc, argv))
                        exit(-1);

        strcpy (cn_name, parm1->answer);
        strcpy (weighted_cn_name, parm2->answer);
        
	cn_flag = weighted_cn_flag = 0;

	G_get_window (&window);
	square_meters = window.ns_res * window.ew_res ;

/*      get the current mapset */
	 this_mapset = G_mapset();

/*      find input map in the mapset and get its mapset location */
	cn_mapset = G_find_cell2 (cn_name,"");
	if(!cn_mapset)
	{
	    sprintf(buf, "curve number file [%s] not found\n", cn_name);
	    G_fatal_error (buf);
	    exit(1);
	}

/*      if the output map exists in the mapset then
	print error message and quit */
	weighted_cn_mapset = G_find_cell2(weighted_cn_name, this_mapset);
	if (weighted_cn_mapset)
	{
	    sprintf(buf, "weighted curve number file [%s] is existing\n", weighted_cn_name);
	    G_fatal_error (buf);
	    exit(1);
	}

/* get the category names and cell title */
    	if (G_read_cats (cn_name, cn_mapset, &cats) < 0)
	    exit(-1);

/* open the cell file */
    	cn_id = cell_open (cn_name, cn_mapset);

	nrows = G_window_rows();
	ncols = G_window_cols();
	cn_cell  	  = G_allocate_cell_buf ();
	weighted_cn_cell  = G_allocate_cell_buf ();

/* initialize the stats */
    	G_init_cell_stats (&statf);

    	acres = 0.0;
    	acres0 = 0.0;
    	total_cn = 0.0;

/* read the cell file and gather the stats */

    	for (row = 0; row < nrows; row++)
	{
	   if (G_get_map_row (cn_id, cn_cell, row) < 0)
	       exit(-3);

	   G_update_cell_stats (cn_cell, ncols, &statf);
	}
	G_close_cell (cn_id);

	G_rewind_cell_stats (&statf);
	while (G_next_cell_stat (&c, &count, &statf))
	{
	   if ( c != 0)
	   {
	      value = ACRES (count, square_meters);
	      acres += value;
	      total_cn = total_cn + (value * c);
	      if (count == 0) acres0 = value;
	   }
	}

	weighted_cn = total_cn / acres;

	fprintf (stderr,"	The weighted curve number of the %s map is %5.2f\n",cn_name,weighted_cn);

	weighted_cn_id = cell_open_new(weighted_cn_name);

    	cn_id = cell_open (cn_name, cn_mapset);

    	for (row = 0; row < nrows; row++)
	{
	   G_get_map_row (cn_id, cn_cell, row);
	   G_zero_cell_buf(weighted_cn_cell);

	   for(i=0;i < ncols;i++)
	   {
	      if(cn_cell[i] > 0)
		  weighted_cn_cell[i] = (int) weighted_cn;
	   }
	   G_put_map_row(weighted_cn_id,weighted_cn_cell);
	}
	G_close_cell(weighted_cn_id);

	strcpy(title,"The weighted curve number");

	G_put_cell_title(weighted_cn_name,title);


}

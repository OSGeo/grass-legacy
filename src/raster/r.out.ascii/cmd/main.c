#include <string.h>
#include "gis.h"
#include <math.h>
#include <stdio.h>

/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:     r.out.ascii
 * AUTHOR(S):  Michael Shapiro
 *             Markus Neteler: added SURFER support
 * PURPOSE:    r.out.ascii: writes ASCII GRID file
 * COPYRIGHT:  (C) 2000 by the GRASS Development Team
 *
 *             This program is free software under the GNU General Public
 *   	    	License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	for details.
 *
 *****************************************************************************/

int main(int argc, char *argv[])
{
    void *raster, *ptr;
   /*
    char  *null_row;
    */
    RASTER_MAP_TYPE out_type, map_type;
    char *name; 
    char *mapset;
    char *outfile;
    char *null_str;
    char surfer_null_str[12];
    char cell_buf[300];
    int fd;
    int row,col;
    int nrows, ncols, dp;
    int do_stdout;
    FILE *fp;
    struct GModule *module;
    struct
    {
	struct Option *map ;
	struct Option *output ;
	struct Option *dp ;
	struct Option *null ;
    } parm;
    struct
    {
        struct Flag *noheader;
        struct Flag *surfer;
        struct Flag *int_out;
    } flag;

    G_gisinit(argv[0]);

	module = G_define_module();
    module->description =
		"Converts a raster map layer into an ASCII text file.";

/* Define the different options */

    parm.map = G_define_option() ;
    parm.map->key        = "input";
    parm.map->type       = TYPE_STRING;
    parm.map->required   = YES;
    parm.map->gisprompt  = "old,cell,raster" ;
    parm.map->description= "Name of an existing raster map" ;

    parm.output = G_define_option() ;
    parm.output->key        = "output";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = YES;
    parm.output->description= "Name of an output ASCII grid map (use out=- for stdout)";

    parm.dp = G_define_option() ;
    parm.dp->key        = "dp";
    parm.dp->type       = TYPE_INTEGER;
    parm.dp->required   = NO;
    parm.dp->answer     = "6";
    parm.dp->description= "Number of decimal places" ;

    parm.null = G_define_option() ;
    parm.null->key        = "null";
    parm.null->type       = TYPE_STRING;
    parm.null->required   = NO;
    parm.null->answer     = "*";
    parm.null->description= "Char string to represent no data cell" ;

    flag.noheader = G_define_flag();
    flag.noheader->key = 'h';
    flag.noheader->description = "Suppress printing of header information";

    flag.surfer = G_define_flag();
    flag.surfer->key = 's';
    flag.surfer->description = "Write SURFER .grd ASCII GRID instead of GRASS ASCII GRID";

    flag.int_out = G_define_flag();
    flag.int_out->key = 'i';
    flag.int_out->description = "Output integer category values, not cell values";

    if (G_parser(argc, argv))
       	exit (-1);

    sscanf(parm.dp->answer, "%d", &dp);
    if(dp>20 || dp < 0)
       G_fatal_error("dp has to be from 0 to 20");
    null_str = parm.null->answer;
    outfile =  parm.output->answer;
    if((strcmp("-", outfile)) == 0)
      do_stdout = 1;
    else
      do_stdout = 0;

    if (flag.surfer->answer && flag.noheader->answer)
    	G_fatal_error("Both -s and -h doesn't make sense.");

    name = parm.map->answer;
    mapset = G_find_cell2 (name, "");

    map_type = G_raster_map_type(name, mapset);
    if(!flag.int_out->answer)
	 out_type = map_type;
    else out_type = CELL_TYPE;

    if (mapset == NULL)
    {
        char msg[100];	
		
	sprintf (msg, "%s: <%s> cellfile not found\n", G_program_name(), name);
		G_fatal_error (msg);
        exit(1);
    }

    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
    	exit(1);

/*
    null_row = G_allocate_null_buf();
    */
    raster =  G_allocate_raster_buf(out_type);

    nrows = G_window_rows();
    ncols = G_window_cols();

/* open ascii file for writing */
    if(do_stdout)
      fp = stdout;
    else
      if(NULL == (fp = fopen(outfile, "w")))
        G_fatal_error("Not able to open file for [%s]", outfile );

    if (!flag.noheader->answer)
    {
	struct Cell_head region;
	char buf[128], buf2[128], buf3[128];
	struct FPRange range;
	DCELL Z_MIN, Z_MAX;
	        
	if (flag.surfer->answer) /* write Surfer Golden Software header */
	{
		if(G_read_fp_range (name, mapset, &range) < 0)
			G_fatal_error ("%s: can't read fp range for [%s]",G_program_name(),name);
		G_get_fp_range_min_max(&range, &Z_MIN, &Z_MAX);
		fprintf (fp,"DSAA \n");
		G_get_window (&region);
		fprintf (fp,"%d %d\n", region.cols, region.rows);
		G_format_easting (region.west, buf2, region.proj);
		G_format_easting (region.east, buf3, region.proj);
		fprintf (fp,"%s %s\n", buf2, buf3);
		G_format_northing (region.south, buf2, region.proj);
		G_format_northing (region.north, buf3, region.proj);
		fprintf (fp,"%s %s\n", buf2, buf3);
		fprintf (fp,"%f %f\n", (double)Z_MIN, (double)Z_MAX);
		
		sprintf(surfer_null_str, "1.70141e+038"); /* define NULL (N/A) value */
	}
	else  /* write GRASS header */
	{
		G_get_window (&region);
		G_format_northing (region.north, buf, region.proj);
		fprintf (fp,"north: %s\n", buf);
		G_format_northing (region.south, buf, region.proj);
		fprintf (fp,"south: %s\n", buf);
		G_format_easting (region.east, buf, region.proj);
		fprintf (fp,"east: %s\n", buf);
		G_format_easting (region.west, buf, region.proj);
		fprintf (fp,"west: %s\n", buf);

		fprintf (fp,"rows: %d\n", region.rows);
		fprintf (fp,"cols: %d\n", region.cols);
	}
    }
    
    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
        exit(1);

  if (!flag.surfer->answer)
  {
    /* write GRASS ASCII GRID row order as usual */
    for (row = 0; row < nrows; row++)
    {
        G_percent(row, nrows, 2);
	if (G_get_raster_row(fd, raster, row, out_type) < 0)
             exit(1);
	/*
	if (G_get_null_value_row(fd, null_row, row) < 0)
	     exit(1);
	     */
        for (col = 0, ptr = raster; col < ncols; col++, 
		       ptr = G_incr_void_ptr(ptr, G_raster_size(out_type))) 
        {
           if(!G_is_null_value(ptr, out_type))
	   {
               if(out_type == CELL_TYPE)
	           fprintf (fp,"%d", *((CELL *) ptr));

               else if(out_type == FCELL_TYPE)
	       {
	           sprintf(cell_buf, "%.*f", dp, *((FCELL *) ptr));
	           G_trim_decimal (cell_buf);
	           fprintf (fp,"%s", cell_buf);
	       }
               else if(out_type == DCELL_TYPE)
	       {
	           sprintf(cell_buf, "%.*f", dp, *((DCELL *) ptr));
	           G_trim_decimal (cell_buf);
	           fprintf (fp,"%s", cell_buf);
	       }
            }
            else
                if (flag.surfer->answer)
                	fprintf (fp,"%s", surfer_null_str);
                else
                	fprintf (fp,"%s", null_str);
            fprintf (fp," ");
        }
	fprintf (fp,"\n");
	/*
        for (col = 0; col < ncols; col++)
            fprintf (fp,"%d ", null_row[col]);
	fprintf (fp,"\n");
	*/
    }
  } /* end GRASS row order */
  else
  {
    /* write Surfer Golden Software row order (reversed to GRASS) */
    for (row = nrows -1 ; row >= 0 ; row--)
    {
        G_percent((row-nrows)*(-1), nrows, 2);
	if (G_get_raster_row(fd, raster, row, out_type) < 0)
             exit(1);
	/*
	if (G_get_null_value_row(fd, null_row, row) < 0)
	     exit(1);
	     */
        for (col = 0, ptr = raster; col < ncols; col++, 
		       ptr = G_incr_void_ptr(ptr, G_raster_size(out_type))) 
        {
           if(!G_is_null_value(ptr, out_type))
	   {
               if(out_type == CELL_TYPE)
	           fprintf (fp,"%d", *((CELL *) ptr));

               else if(out_type == FCELL_TYPE)
	       {
	           sprintf(cell_buf, "%.*f", dp, *((FCELL *) ptr));
	           G_trim_decimal (cell_buf);
	           fprintf (fp,"%s", cell_buf);
	       }
               else if(out_type == DCELL_TYPE)
	       {
	           sprintf(cell_buf, "%.*f", dp, *((DCELL *) ptr));
	           G_trim_decimal (cell_buf);
	           fprintf (fp,"%s", cell_buf);
	       }
            }
            else
                if (flag.surfer->answer)
                	fprintf (fp,"%s", surfer_null_str);
                else
                	fprintf (fp,"%s", null_str);
            fprintf (fp," ");
        }
	fprintf (fp,"\n");
    }
  } /* Surfer row order (bottom up) */
  
  G_close_cell(fd);
  fclose(fp);
  exit(0);
}

int set_type( char *str, RASTER_MAP_TYPE *out_type)
{
   char msg[100];
   char *ch;

   ch = str;
   if(*ch != '%')
   {
        sprintf(msg, "wrong format: %s", str);
        G_fatal_error(msg);
   }
   while (*(++ch));
   ch--;
   if(*ch=='d' || *ch=='i' || *ch=='o' || *ch=='u' || *ch=='x' || *ch=='X')
       *out_type = CELL_TYPE;
   else if(*ch=='f' || *ch=='e' || *ch=='E' || *ch=='g' || *ch=='G')
       *out_type = DCELL_TYPE;
   /*
       *out_type = FCELL_TYPE;
   */

    return 0;
}       

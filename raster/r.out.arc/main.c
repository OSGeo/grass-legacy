#include <string.h>
#include "gis.h"
#include <math.h>
#include <stdio.h>

/* 11/22/99 fix again for row order M. Neteler */
/* fix for coordinates in header 11/17/99 M.N.*/

/* This is the GRASS 5 r.out.ascii code. Modified to
 * write ARC/Info-ascii-GRID format.
 * 11/1999 Markus Neteler, Univ. of Hannover 
 * neteler@geog.uni-hannover.de
 */

int main(int argc, char *argv[])
{
    void *raster, *ptr;
    /*
    char  *null_row;
    */
    RASTER_MAP_TYPE out_type, map_type;
    char *name; 
    char *mapset;
    char null_str[80];
    char cell_buf[300];
    int fd;
    int row,col;
    int nrows, ncols, i, dp;
    int number;
    double cellsize;
	struct GModule *module;
    struct
    {
	struct Option *map ;
	struct Option *dp ;
	struct Option *null ;
    } parm;
    struct
    {
        struct Flag *noheader;
        struct Flag *singleline;
    } flag;

    G_gisinit(argv[0]);

	module = G_define_module();
    module->description =
		"Converts a raster map layer into an ESRI ARCGRID file.";

/* Define the different options */

    parm.map = G_define_option() ;
    parm.map->key        = "map";
    parm.map->type       = TYPE_STRING;
    parm.map->required   = YES;
    parm.map->gisprompt  = "old,cell,raster" ;
    parm.map->description= "Name of an existing raster map layer";

    parm.dp = G_define_option() ;
    parm.dp->key        = "dp";
    parm.dp->type       = TYPE_INTEGER;
    parm.dp->required   = NO;
    parm.dp->answer     = "6";
    parm.dp->description= "Number of decimal places";

    flag.noheader = G_define_flag();
    flag.noheader->key = 'h';
    flag.noheader->description = "Suppress printing of header information";

  /* Added to optionaly produce a single line output.     -- emes -- 12.10.92 */
    flag.singleline = G_define_flag();
    flag.singleline->key = '1';
    flag.singleline->description = "List one entry per line instead of full row";

    if (G_parser(argc, argv))
       	exit (-1);

    sscanf(parm.dp->answer, "%d", &dp);
    if(dp>20 || dp < 0)
       G_fatal_error("dp has to be from 0 to 20");

    sprintf(null_str,"-9999");

    name = parm.map->answer;
    mapset = G_find_cell2 (name, "");

    map_type = G_raster_map_type(name, mapset);
    out_type = map_type;

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

    if (!flag.noheader->answer)
    {
	struct Cell_head region;
	char buf[128];

	G_get_window (&region);
	fprintf (stdout, "ncols %d\n", region.cols);
	fprintf (stdout, "nrows %d\n", region.rows);

	if(G_projection() != 3)  /* Is Projection != LL (3) */
	{
	  G_format_easting (region.west, buf, region.proj);
	  fprintf (stdout, "xllcorner %s\n", buf);
	  G_format_northing (region.south, buf, region.proj);
	  fprintf (stdout, "yllcorner %s\n", buf);
	}
	else /* yes, lat/long */
	{
	  fprintf (stdout, "xllcorner %f\n", region.west);
	  fprintf (stdout, "yllcorner %f\n", region.south);
	}

	cellsize= fabs(region.east - region.west)/region.cols;
	fprintf(stdout, "cellsize %f\n", cellsize);
        fprintf(stdout, "NODATA_value %s\n", null_str);
    }
    
    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
        exit(1);

    for (row = 0; row < nrows; row++)
    {
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
	           fprintf (stdout,"%d", *((CELL *) ptr));

               else if(out_type == FCELL_TYPE)
	       {
	           sprintf(cell_buf, "%.*f", dp, *((FCELL *) ptr));
	           G_trim_decimal (cell_buf);
	           fprintf (stdout,"%s", cell_buf);
	       }
               else if(out_type == DCELL_TYPE)
	       {
	           sprintf(cell_buf, "%.*f", dp, *((DCELL *) ptr));
	           G_trim_decimal (cell_buf);
	           fprintf (stdout,"%s", cell_buf);
	       }
            }
            else
                fprintf (stdout,"%s", null_str);

        if(!flag.singleline->answer)          
	  fprintf (stdout, " ");
        else
          fprintf (stdout,"\n");
        }

	if(!flag.singleline->answer)
          fprintf (stdout,"\n");

	/*
        for (col = 0; col < ncols; col++)
            fprintf (stdout,"%d ", null_row[col]);
	fprintf (stdout,"\n");
	*/
    }
    G_close_cell(fd);
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

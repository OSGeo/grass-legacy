#include <string.h>
#include "gis.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    void *raster, *ptr;
    /*
    char  *null_row;
    */
    RASTER_MAP_TYPE out_type, map_type;
    char *name; 
    char *mapset;
    char *null_str;
    char cell_buf[300];
    int fd;
    int row,col;
    int nrows, ncols, i, dp;
    int number;
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
        struct Flag *int_out;
    } flag;

    G_gisinit(argv[0]);

	module = G_define_module();
    module->description =
		"Converts a raster map layer into an ASCII text file.";

/* Define the different options */

    parm.map = G_define_option() ;
    parm.map->key        = "map";
    parm.map->type       = TYPE_STRING;
    parm.map->required   = YES;
    parm.map->gisprompt  = "old,cell,raster" ;
    parm.map->description= "Name of an existing raster map" ;

    parm.dp = G_define_option() ;
    parm.dp->key        = "dp";
    parm.dp->type       = TYPE_INTEGER;
    parm.dp->required   = NO;
    parm.dp->answer     = "6";
    parm.dp->description= "number of decimal places" ;

    parm.null = G_define_option() ;
    parm.null->key        = "null";
    parm.null->type       = TYPE_STRING;
    parm.null->required   = NO;
    parm.null->answer     = "*";
    parm.null->description= "Char string to represent no data cell" ;

    flag.noheader = G_define_flag();
    flag.noheader->key = 'h';
    flag.noheader->description = "Suppress printing of header information";

    flag.int_out = G_define_flag();
    flag.int_out->key = 'i';
    flag.int_out->description = "Output integer category values, not cell values";

    if (G_parser(argc, argv))
       	exit (-1);

    sscanf(parm.dp->answer, "%d", &dp);
    if(dp>20 || dp < 0)
       G_fatal_error("dp has to be from 0 to 20");
    null_str = parm.null->answer;

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

    if (!flag.noheader->answer)
    {
	struct Cell_head region;
	char buf[128];

	G_get_window (&region);
	G_format_northing (region.north, buf, region.proj);
	fprintf (stdout,"north: %s\n", buf);
	G_format_northing (region.south, buf, region.proj);
	fprintf (stdout,"south: %s\n", buf);
	G_format_easting (region.east, buf, region.proj);
	fprintf (stdout,"east: %s\n", buf);
	G_format_easting (region.west, buf, region.proj);
	fprintf (stdout,"west: %s\n", buf);

	fprintf (stdout,"rows: %d\n", region.rows);
	fprintf (stdout,"cols: %d\n", region.cols);

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
            fprintf (stdout," ");
        }
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

/*
 *   r.out.bin
 *
 *   Copyright (C) 2000 by the GRASS Development Team
 *   Author: Bob Covill <bcovill@tekmap.ns.ca>
 *
 *   This program is free software under the GPL (>=v2)
 *   Read the file COPYING coming with GRASS for details.
 *
 *   $Id$
 */
                                
#include <string.h>
#include "gis.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[])
{
    void *raster, *ptr;
    /*
    char  *null_row;
    */
    RASTER_MAP_TYPE out_type, map_type;
    char *name; 
    char *outfile; 
    char *mapset;
    char errbuf[100];
    int null_str = 0;
    char cell_buf[300];
    int fd;
    int row,col;
    int nrows, ncols, i;
    int number_i;
    int do_stdout = 0;
    FILE *fp;

    float number_f;
    double number_d;
    int null_val_i;
    float null_val_f;
    double null_val_d;
    struct Cell_head region;
    struct
    {
	struct Option *input ;
	struct Option *output ;
	struct Option *null ;
    } parm;
    struct
    {
        struct Flag *int_out;
    } flag;

    G_gisinit(argv[0]);

/* Define the different options */

    parm.input = G_define_option() ;
    parm.input->key        = "input";
    parm.input->type       = TYPE_STRING;
    parm.input->required   = YES;
    parm.input->gisprompt  = "old,cell,raster" ;
    parm.input->description= "Name of an existing raster map" ;

    parm.output = G_define_option() ;
    parm.output->key        = "output";
    parm.output->type       = TYPE_STRING;
    parm.output->required   = YES;
    parm.output->gisprompt  = "old,cell,raster" ;
    parm.output->description= "Name of an output binary map (use out=- for stdout)" ;

    parm.null = G_define_option() ;
    parm.null->key        = "null";
    parm.null->type       = TYPE_INTEGER;
    parm.null->required   = NO;
    parm.null->answer     = "0";
    parm.null->description= "Value to write out for null" ;

    flag.int_out = G_define_flag();
    flag.int_out->key = 'i';
    flag.int_out->description = "Output integer category values, not cell values";

    if (G_parser(argc, argv))
       	exit (1);

    sscanf(parm.null->answer, "%d", &null_str);    
    name = parm.input->answer;
    outfile =  parm.output->answer;

/* Does not seem to like this name?

   if ((strcmp("", outfile)) == 0) {
	outfile = name;
	strcat (outfile,".bin");
	} else {
*/
	if((strcmp("-", outfile)) == 0)
	do_stdout = 1; 

    G_get_window (&region);

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

/* open bin file for writing */
     if(do_stdout) fp = stdout;
     else if(NULL == (fp = fopen(outfile, "w"))) {
       sprintf(errbuf,"Not able to open file for [%s]", outfile);
       G_fatal_error(errbuf);
     }

    raster =  G_allocate_raster_buf(out_type);

    nrows = G_window_rows();
    ncols = G_window_cols();

/* Old Header section
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
*/
if(out_type == CELL_TYPE)
fprintf(stderr, "Exporting Raster as integer values (bytes=%d)\n", sizeof(int));
if(out_type == FCELL_TYPE)
fprintf(stderr, "Exporting Raster as floating values (bytes=%d)\n", sizeof(float));
if(out_type == DCELL_TYPE)
fprintf(stderr, "Exporting Raster as double values(bytes=%d)\n", sizeof(double));

fprintf(stderr, "Using the Current Region settings ...\n");
fprintf(stderr, "north=%f\n", region.north);
fprintf(stderr, "south=%f\n", region.south);
fprintf(stderr, "east=%f\n", region.east);
fprintf(stderr, "west=%f\n", region.west);
fprintf(stderr, "r=%d\n", region.rows);
fprintf(stderr, "c=%d\n\n", region.cols);

fprintf(stderr, "Percent complete: ");
    
    fd = G_open_cell_old (name, mapset);
    if (fd < 0)
        exit(1);

    for (row = 0; row < nrows; row++)
    {
	if (G_get_raster_row(fd, raster, row, out_type) < 0)
             exit(1);
		G_percent(row, nrows, 2);
        for (col = 0, ptr = raster; col < ncols; col++, 
		       ptr = G_incr_void_ptr(ptr, G_raster_size(out_type))) 
        {
           if(!G_is_null_value(ptr, out_type))
	   {
               if(out_type == CELL_TYPE) {
		number_i = *((CELL *) ptr);
		fwrite (&number_i, sizeof(int), 1, fp);
		}
               else if(out_type == FCELL_TYPE)
	       {
		number_f = *((FCELL *) ptr);
		fwrite (&number_f,sizeof(float), 1, fp);
	       }
               else if(out_type == DCELL_TYPE)
	       {
		number_d = *((DCELL *) ptr);
		fwrite (&number_d,sizeof(double), 1, fp);
	       }
            }
            else {
	if(out_type == CELL_TYPE) {
		null_val_i = (int)null_str;
		fwrite(&null_val_i, sizeof(int), 1, fp);
		}
	if( out_type == FCELL_TYPE ) {
		null_val_f = (float)null_str;
		fwrite(&null_val_f, sizeof(float), 1, fp);
		}
	if( out_type == DCELL_TYPE ) {
                null_val_d = (double)null_str;
                fwrite(&null_val_d, sizeof(double), 1, fp);
                }

		}
        }
    }
    G_close_cell(fd);
    fclose(fp);
	fprintf(stderr, "\nDone !\n");
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

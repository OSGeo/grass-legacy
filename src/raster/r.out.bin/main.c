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

#include "./gmt_grd.h"
#include "./swab.h"

int main(int argc, char *argv[])
{
    void *raster, *ptr;
    RASTER_MAP_TYPE out_type, map_type;
    char *name; 
    char *outfile; 
    char *mapset;
    char errbuf[100];
    int null_str = 0;
    char cell_buf[300], buf[128];
    int fd;
    int row,col;
    int nrows, ncols, i;
    short number_i;
    int do_stdout = 0;
    FILE *fp;
    struct GRD_HEADER header;
    struct FPRange range;
    DCELL Z_MIN, Z_MAX;

    float number_f;
    double number_d;
    short null_val_i;
    float null_val_f;
    double null_val_d;
    struct Cell_head region;
	struct GModule *module;
    struct
    {
	struct Option *input ;
	struct Option *output ;
	struct Option *null ;
    } parm;
    struct
    {
        struct Flag *int_out, *gmt_hd, *BIL_hd, *swap;
    } flag;
    union {
        int testWord;
        char testByte[4];
    } endianTest;
    int swapFlag;


    G_gisinit(argv[0]);

	module = G_define_module();
    module->description =
		"Exports a GRASS raster to a binary array.";

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
    flag.gmt_hd = G_define_flag();
    flag.gmt_hd->key = 'h';
    flag.gmt_hd->description = "Export Array with GMT compatible header";

    flag.BIL_hd = G_define_flag();
    flag.BIL_hd->key = 'b';
    flag.BIL_hd->description = "Generate BIL world and header files";

    flag.swap = G_define_flag();
    flag.swap->key = 's';
    flag.swap->description = "Byte Swap output";

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


/* Check Endian State of Host Computer*/
    endianTest.testWord = 1;
    if (endianTest.testByte[0] == 1) {
        swapFlag = 1; /*true: little endian */
	if (flag.swap->answer) swapFlag = 0; /* Swapping enabled */
    } else {
        swapFlag = 0;
	if (flag.swap->answer) swapFlag = 1; /* Swapping enabled */
    }


/* Set up Parameters for GMT header*/
if (flag.gmt_hd->answer) {
G_read_fp_range (name, mapset, &range);
G_get_fp_range_min_max(&range, &Z_MIN, &Z_MAX);

header.nx = region.cols;
header.ny = region.rows;
header.node_offset = 1; /* 1 is pixel registration */
header.x_min = (double)region.west ;
header.x_max = (double)region.east ;
header.y_min = (double)region.south ;
header.y_max = (double)region.north ;
header.z_min = (double)Z_MIN;
header.z_max = (double)Z_MAX;
header.x_inc = (double)region.ew_res;
header.y_inc = (double)region.ns_res;
header.z_scale_factor = (double)1.0;
header.z_add_offset = (double)0.0;

/* Swap Header if Required */
if (flag.swap->answer) {
fprintf(stderr, "Swapping Header Data\n");
TIFFSwabLong((uint32 *)&header.nx);
TIFFSwabLong((uint32 *)&header.ny);
TIFFSwabLong((uint32 *)&header.node_offset);

TIFFSwabDouble((double *)&header.x_min);
TIFFSwabDouble((double *)&header.x_max);
TIFFSwabDouble((double *)&header.y_min);
TIFFSwabDouble((double *)&header.y_max);
TIFFSwabDouble((double *)&header.z_min);
TIFFSwabDouble((double *)&header.z_max);
TIFFSwabDouble((double *)&header.x_inc);
TIFFSwabDouble((double *)&header.y_inc);
TIFFSwabDouble((double *)&header.z_scale_factor);
TIFFSwabDouble((double *)&header.z_add_offset);
} 

if(region.proj == PROJECTION_LL) {
strcpy(header.x_units, "degrees");
strcpy(header.y_units, "degrees");
} else {
strcpy(header.x_units, "Meters");
strcpy(header.y_units, "Meters");
}
strcpy(header.z_units, "elevation");
strcpy(header.title, name);
strcpy(header.command, "r.out.bin -h input=");
strcat(header.command, name);
strcat(header.command, " output=");
strcat(header.command, outfile);
if(flag.swap->answer)
TIFFSwabLong((uint32 *)&null_str);
sprintf(buf, "%d", null_str);
strcpy(header.remark, buf);
strcat(header.remark, " used for NULL");
}

/* Write out BIL support files compatible with 
* Arc-View 
*/
if(flag.BIL_hd->answer) {
char out_tmp1[100], out_tmp2[100];
FILE *fp_1, *fp_2;

strcpy(out_tmp1, outfile);
strcat(out_tmp1, ".hdr");
strcpy(out_tmp2, outfile);
strcat(out_tmp2, ".wld");

/* Open Header File */
if(NULL == (fp_1 = fopen(out_tmp1, "w"))) {
sprintf(errbuf,"Not able to open file for [%s]", out_tmp1);
G_fatal_error(errbuf);
}

/* Open World File */
if(NULL == (fp_2 = fopen(out_tmp2, "w"))) {
sprintf(errbuf,"Not able to open file for [%s]", out_tmp2);
G_fatal_error(errbuf);
}

fprintf(stderr, "Creating BIL support files ...\n");
fprintf(stderr, "Header File = %s\n", out_tmp1);
fprintf(stderr, "World File = %s\n", out_tmp2);

fprintf(fp_1, "nrows %d\n", region.rows);
fprintf(fp_1, "ncols %d\n", region.cols);
fprintf(fp_1, "nbands 1\n");
if(out_type == CELL_TYPE)
fprintf(fp_1, "nbits 16\n");
if(out_type == FCELL_TYPE)
fprintf(fp_1, "nbits 32\n");
if(out_type == DCELL_TYPE)
fprintf(fp_1, "nbits 64\n");
if (swapFlag == 1)
fprintf(fp_1, "byteorder I\n"); /* Intel - little endian*/
if (swapFlag == 0)
fprintf(fp_1, "byteorder M\n"); /* Motorola - big endian*/
fprintf(fp_1, "layout bil\n");
if(flag.gmt_hd->answer) {
if (swapFlag == 1)
fprintf(fp_1, "skipbytes 892\n"); /* Real size of struct - little endian*/
else
fprintf(fp_1, "skipbytes 896\n"); /* Pad size of struct - big endian*/
} else
fprintf(fp_1, "skipbytes 0\n");
fprintf(fp_1, "nodata %f\n", null_str);

fclose (fp_1);

fprintf(fp_2, "%f\n", region.ew_res);
fprintf(fp_2, "0.0\n");
fprintf(fp_2, "0.0\n");
fprintf(fp_2, "-%f\n", region.ns_res);
fprintf(fp_2, "%f\n", region.west + (region.ew_res/2) );
fprintf(fp_2, "%f\n", region.north - (region.ns_res/2) );

fclose(fp_2);
}

    raster =  G_allocate_raster_buf(out_type);

/* Write out GMT Header if required */
	if(flag.gmt_hd->answer) {
	/* Write Values 1 at a time if byteswapping */
	fwrite (&header.nx, sizeof(int), 1, fp);
	fwrite (&header.ny, sizeof(int), 1, fp);
	fwrite (&header.node_offset, sizeof(int), 1, fp);
	if(swapFlag== 0) /* Padding needed for big-endian */
	fwrite (&header.node_offset, sizeof(int), 1, fp);

	fwrite (&header.x_min, sizeof(double), 1, fp);
	fwrite (&header.x_max, sizeof(double), 1, fp);
	fwrite (&header.y_min, sizeof(double), 1, fp);
	fwrite (&header.y_max, sizeof(double), 1, fp);
	fwrite (&header.z_min, sizeof(double), 1, fp);
	fwrite (&header.z_max, sizeof(double), 1, fp);
	fwrite (&header.x_inc, sizeof(double), 1, fp);
	fwrite (&header.y_inc, sizeof(double), 1, fp);
	fwrite (&header.z_scale_factor, sizeof(double), 1, fp);
	fwrite (&header.z_add_offset, sizeof(double), 1, fp);

	fwrite (&header.x_units, sizeof(char[GRD_UNIT_LEN]), 1, fp);
	fwrite (&header.y_units, sizeof(char[GRD_UNIT_LEN]), 1, fp);
	fwrite (&header.z_units, sizeof(char[GRD_UNIT_LEN]), 1, fp);
	fwrite (&header.title, sizeof(char[GRD_TITLE_LEN]), 1, fp);
	fwrite (&header.command, sizeof(char[GRD_COMMAND_LEN]), 1, fp);
	fwrite (&header.remark, sizeof(char[GRD_REMARK_LEN]), 1, fp);
	}

    nrows = G_window_rows();
    ncols = G_window_cols();

if(out_type == CELL_TYPE) {
fprintf(stderr, "Exporting Raster as integer values (bytes=%d)\n", sizeof(short));
	if(flag.gmt_hd->answer)
	fprintf(stderr, "Writing GMT integer format ID=2\n");
}
if(out_type == FCELL_TYPE) {
fprintf(stderr, "Exporting Raster as floating values (bytes=%d)\n", sizeof(float));
	if(flag.gmt_hd->answer)
	fprintf(stderr, "Writing GMT float format ID=1\n");
}
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
		if(flag.swap->answer)
		TIFFSwabShort((uint16 *)&number_i);
		fwrite (&number_i, sizeof(short), 1, fp);
		}
               else if(out_type == FCELL_TYPE)
	       {
		number_f = *((FCELL *) ptr);
		if(flag.swap->answer)
		TIFFSwabLong((uint32 *)&number_f);
		fwrite (&number_f,sizeof(float), 1, fp);
	       }
               else if(out_type == DCELL_TYPE)
	       {
		number_d = *((DCELL *) ptr);
		if(flag.swap->answer)
		TIFFSwabDouble((double *)&number_d);
		fwrite (&number_d,sizeof(double), 1, fp);
	       }
            }
            else {
	if(out_type == CELL_TYPE) {
		null_val_i = (int)null_str;
		if(flag.swap->answer)
		TIFFSwabShort((uint16 *)&null_val_i);
		fwrite(&null_val_i, sizeof(int), 1, fp);
		}
	if( out_type == FCELL_TYPE ) {
		null_val_f = (float)null_str;
		if(flag.swap->answer)
		TIFFSwabLong((uint32 *)&null_val_f);
		fwrite(&null_val_f, sizeof(float), 1, fp);
		}
	if( out_type == DCELL_TYPE ) {
                null_val_d = (double)null_str;
		if(flag.swap->answer)
		TIFFSwabDouble((double *)&null_val_d);
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

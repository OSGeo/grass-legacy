/*
 * r.out.mat
 *
 * Output a GRASS raster file to a MAT-File (version 4).
 *
 *   Copyright (C) 2004 by the GRASS Development Team
 *   Author: Hamish Bowman, University of Otago, New Zealand
 *
 *   This program is free software under the GPL (>=v2)
 *   Read the COPYING file that comes with GRASS for details.
 *
 *   Code follows r.out.bin to a certain extent, which in turn
 *   follows r.out.ascii.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "glocale.h"

int main(int argc, char *argv[]) {

    int i,row,col; /* counters */
    unsigned long filesize;

    int endianness;  /* 0=little, 1=big */
    int data_format;    /* 0=double  1=float  2=32bit signed int  5=8bit unsigned int (ie text) */
    int data_type;      /* 0=numbers  1=text */
    long format_block;  /* combo of endianness, 0, data_format, and type */
    long realflag = 0;  /* 0=only real values used */
    /* should type be specifically uint32 ??? */

    char array_name[32];  /* variable names must start with a letter (case 
			     sensitive) followed by letters, numbers, or 
			     underscores. 31 chars max. */
    int name_len;
    long mrows, ncols;  /* text/data/map array dimensions*/

    int val_i;		/* for misc use */
    float val_f;	/* for misc use */
    double val_d;	/* for misc use */

    char *infile, *outfile, *mapset, *maptitle;
    struct Cell_head region;
    void *raster, *ptr;
    RASTER_MAP_TYPE map_type;

    struct Option *inputfile, *outputfile;
    struct Flag *verbose;
    struct GModule *module;

    int fd;
    FILE *fp1;


    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
         _("Exports a GRASS raster to a binary MAT-File.");

    /* Define the different options */

    inputfile = G_define_option() ;
    inputfile->key        = "input";
    inputfile->type       = TYPE_STRING;
    inputfile->required   = YES;
    inputfile->gisprompt  = "old,cell,raster" ;
    inputfile->description= _("Name of an existing raster map") ;

    outputfile = G_define_option() ;
    outputfile->key        = "output";
    outputfile->type       = TYPE_STRING;
    outputfile->required   = YES;
    outputfile->description= _("Name for the output binary MAT-File") ;

    verbose = G_define_flag();
    verbose->key = 'v';
    verbose->description = _("Verbose mode");


    if (G_parser(argc,argv))
	exit(1);


    infile = inputfile->answer;
    outfile = outputfile->answer;

    mapset = G_find_cell2 (infile, "");

    if (mapset == NULL) {
        G_fatal_error(_("raster <%s> not found"), infile);
    }

    map_type = G_raster_map_type(infile, mapset);

    fd = G_open_cell_old (infile, mapset);
    if (fd < 0)
        G_fatal_error(_("unable to open <%s>"), infile);

    /* open bin file for writing */
    fp1 = fopen(outfile, "wb");
    if(NULL == fp1)
       G_fatal_error("unable to open output file <%s>", outfile);


    /* Check Endian State of Host Computer*/
    if (G_is_little_endian())
        endianness = 0;   /* ie little endian */
    else
        endianness = 1;   /* ie big endian */
    G_debug(1, "Machine is %s endian.\n", endianness ? "big" : "little");

    G_get_window (&region);


    /********** Write map **********/

    /** write text element (map name) **/
    strncpy(array_name, "map_name", 31);
    mrows=1;
    ncols=strlen(infile);
    data_format = 5; /* 0=double  1=float  2=32bit signed int  5=8bit unsigned int(text) */
    data_type = 1;   /* 0=numbers  1=text */

    fprintf(stderr, "Exporting <%s>\n", infile);

    /* 4 byte data format */
    format_block = endianness*1000 + data_format*10 + data_type;
    fwrite(&format_block, sizeof(long), 1, fp1);
    /* fprintf(stderr, "name data format is [%04ld]\n", format_block); */

    /* 4 byte number of rows & columns */
    fwrite(&mrows, sizeof(long), 1, fp1);
    fwrite(&ncols, sizeof(long), 1, fp1);

    /* 4 byte real/imag flag   0=real vals only */
    fwrite(&realflag, sizeof(long), 1, fp1);

    /* length of array_name+1 */
    name_len = strlen(array_name) + 1;
    fwrite(&name_len, sizeof(long), 1, fp1);

    /* array name */
    fprintf(fp1, "%s%c", array_name, '\0');

    /* array data */
    fprintf(fp1, "%s", infile);


    /********** Write title (if there is one) **********/
    maptitle = G_get_cell_title (infile, mapset);
    if(strlen(maptitle) >= 1) {
	/** write text element (map title) **/
	strncpy(array_name, "map_title", 31);
	mrows=1;
	ncols=strlen(maptitle);
	data_format = 5; /* 0=double  1=float  2=32bit signed int  5=8bit unsigned int(text) */
	data_type = 1;   /* 0=numbers  1=text */

	/* 4 byte data format */
	format_block = endianness*1000 + data_format*10 + data_type;
	fwrite(&format_block, sizeof(long), 1, fp1);

	/* 4 byte number of rows & columns */
	fwrite(&mrows, sizeof(long), 1, fp1);
	fwrite(&ncols, sizeof(long), 1, fp1);

	/* 4 byte real/imag flag   0=real vals only */
	fwrite(&realflag, sizeof(long), 1, fp1);

	/* length of array_name+1 */
	name_len = strlen(array_name) + 1;
	fwrite(&name_len, sizeof(long), 1, fp1);

	/* array name */
	fprintf(fp1, "%s%c", array_name, '\0');

	/* array data */
	fprintf(fp1, "%s", maptitle);
    }


    /***** Write bounds *****/
    if(verbose->answer) {
	fprintf(stderr, "\nUsing the Current Region settings:\n");
	fprintf(stderr, "northern edge=%f\n", region.north);
	fprintf(stderr, "southern edge=%f\n", region.south);
	fprintf(stderr, "eastern edge=%f\n", region.east);
	fprintf(stderr, "western edge=%f\n", region.west);
	fprintf(stderr, "nsres=%f\n", region.ns_res);
	fprintf(stderr, "ewres=%f\n", region.ew_res);
	fprintf(stderr, "rows=%d\n", region.rows);
	fprintf(stderr, "cols=%d\n\n", region.cols);
    }

    for (i=0; i<4; i++) {
	switch (i) {
	    case 0:
		strncpy(array_name, "map_northern_edge", 31);
		val_d = region.north;
		break;
	    case 1:
		strncpy(array_name, "map_southern_edge", 31);
		val_d = region.south;
		break;
	    case 2:
		strncpy(array_name, "map_eastern_edge", 31);
		val_d = region.east;
		break;
	    case 3:
		strncpy(array_name, "map_western_edge", 31);
		val_d = region.west;
		break;
	    default:
	        fclose(fp1);
		G_fatal_error("please contact development team");
		break;
	}

	/** write data element **/
	data_format = 0; /* 0=double  1=float  2=32bit signed int  5=8bit unsigned int(text) */
	data_type = 0;   /* 0=numbers  1=text */
	mrows = 1;
	ncols = 1;

	/* 4 byte data format */
	format_block = endianness*1000 + data_format*10 + data_type;
	fwrite(&format_block, sizeof(long), 1, fp1);
	/* fprintf(stderr, "bounds data format is [%04ld]\n", format_block); */

	/* 4 byte number of rows , 4 byte number of colums */
	fwrite(&mrows, sizeof(long), 1, fp1);
	fwrite(&ncols, sizeof(long), 1, fp1);

	/* 4 byte real/imag flag   0=only real */
	fwrite(&realflag, sizeof(long), 1, fp1);

	/* length of array_name+1 */
	name_len = strlen(array_name) + 1;
	fwrite(&name_len, sizeof(long), 1, fp1);

	/* array name */
	fprintf(fp1, "%s%c", array_name, '\0');

	/* write array data, by increasing column */
	fwrite(&val_d, sizeof(double), 1, fp1);
	/** end of data element **/
    }



    /***** Write map data *****/
    strncpy(array_name, "map_data", 31);

    switch (map_type)
    {   /* data_format: 0=double  1=float  2=32bit signed int  5=8bit unsigned int (ie text) */

        case CELL_TYPE:
	    data_format = 2;
	    if(verbose->answer)
		fprintf(stderr, "Exporting raster as integer values\n\n");
	    break;

	case FCELL_TYPE:
	    data_format = 1;
	    if(verbose->answer)
		fprintf(stderr, "Exporting raster as floating point values\n\n");
	    break;

	case DCELL_TYPE:
	    data_format = 0;
	    if(verbose->answer)
		fprintf(stderr, "Exporting raster as double FP values\n\n");
	    break;

	default:
	    fclose(fp1);
	    G_fatal_error("Please contact development team");
	    break;
    }

    data_type = 0;   /* 0=numbers  1=text */

    mrows = region.rows;
    ncols = region.cols;

    /* 4 byte data format */
    format_block = (endianness*1000) + (data_format*10) + data_type;
    fwrite(&format_block, sizeof(long), 1, fp1);

    G_debug(3, "map data format is [%04ld]\n", format_block);

    /* 4 byte number of rows & columns*/
    fwrite(&mrows, sizeof(long), 1, fp1);
    fwrite(&ncols, sizeof(long), 1, fp1);

    /* 4 byte real/imag flag   0=only real */
    fwrite(&realflag, sizeof(long), 1, fp1);

    /* length of array_name+1 */
    name_len = strlen(array_name) + 1;
    fwrite(&name_len, sizeof(long), 1, fp1);

    /* array name */
    fprintf(fp1, "%s%c", array_name, '\0');

    /* data array, by increasing column */
    raster = G_calloc ((G_window_rows()+1)*(G_window_cols()+1), G_raster_size(map_type));

    G_debug(1, "mem alloc is %d bytes\n", /* I think _cols()+1 is unneeded? */
    	G_raster_size(map_type)*(G_window_rows()+1)*(G_window_cols()+1) );

    fprintf(stderr, "Reading in map .. ");
    fflush(stderr);


    /* load entire map into memory */
    for (row = 0, ptr = raster; row < mrows; row++, 
      ptr = G_incr_void_ptr(ptr, (G_window_cols()+1)*G_raster_size(map_type))) {
    	if (G_get_raster_row(fd, ptr, row, map_type) < 0)
    	    G_fatal_error("reading map");
	G_percent(row, mrows, 2);
    }
    G_percent(row, mrows, 2);  /* finish it off */

    
    fprintf(stderr, "Writing out map .. ");
    fflush(stderr);


    /* then write it to disk */
    /* NoGood: fwrite(raster, G_raster_size(map_type), mrows*ncols, fp1); */
    for(col=0; col<ncols; col++) {
    	for(row=0; row<mrows; row++) {

    	    ptr = raster;
    	    ptr = G_incr_void_ptr(ptr, (col+row*(ncols+1))*G_raster_size(map_type));

    	    if(!G_is_null_value(ptr, map_type)) {
    		if(map_type == CELL_TYPE) {
    		    val_i = *((CELL *) ptr);
    		    fwrite(&val_i, sizeof(int), 1, fp1);
    		}
    		else if(map_type == FCELL_TYPE) {
    		    val_f = *((FCELL *) ptr);
    		    fwrite(&val_f, sizeof(float), 1, fp1);
    		}
    		else if(map_type == DCELL_TYPE) {
    		    val_d = *((DCELL *) ptr);
    		    fwrite(&val_d, sizeof(double), 1, fp1);
    		}
    	    }
    	    else {  /* ie if NULL cell -> write IEEE NaN value */
    		if(map_type == CELL_TYPE) {
		    val_i = *((CELL *) ptr); /* int has no NaN value, so use whatever GRASS uses */
		    fwrite (&val_i, sizeof(int), 1, fp1);
		}
		else if(map_type == FCELL_TYPE) {
    		    if(endianness)  /* ie big */
    			fprintf(fp1, "%c%c%c%c", 0xff,0xf8,0,0);
    		    else  /* ie little */
    			fprintf(fp1, "%c%c%c%c", 0,0,0xf8,0xff);
		}
		else if(map_type == DCELL_TYPE) {
    		    if(endianness)
    			fprintf(fp1, "%c%c%c%c%c%c%c%c", 0xff,0xf8,0,0,0,0,0,0);
    		    else
    			fprintf(fp1, "%c%c%c%c%c%c%c%c", 0,0,0,0,0,0,0xf8,0xff);
		}
    	    }
	}
    	G_percent(col, ncols, 2);
    }
    G_percent(col, ncols, 2); /* finish it off */

    /*** end of data element ***/


    /* done! */
    filesize=ftell(fp1);
    fclose(fp1);

    if(verbose->answer)
	fprintf(stderr, "\n%ld bytes written to '%s'.\n", filesize, outfile);

    G_done_msg("");
    return 0;
}

/*
 *   r.in.bin
 *
 *   Copyright (C) 2000 by the GRASS Development Team
 *   Author: Bob Covill <bcovill@tekmap.ns.ca>
 *
 *   This program is free software under the GPL (>=v2)
 *   Read the file COPYING coming with GRASS for details.
 *
 *   $Id$
 */

#include <stdlib.h>
#include <unistd.h>
#include "gis.h"
#include <sys/stat.h>
#include "./swab.h"
#include "./gmt_grd.h"


FILE *Tmp_fd = NULL;
char *Tmp_file = NULL;
int file_cpy(FILE *, FILE *);

static double oldval ;

int main (int argc, char *argv[])
{
	char *input;
	char *output;
	char *title;
	FILE *fd;
	int cf;
	struct Cell_head cellhd;
	CELL *cell;
        FCELL *fcell;
        RASTER_MAP_TYPE map_type;
	int row, col;
	int nrows, ncols;
	int bytes, sflag, swap;
	short *x_s;
	int *x_i;
	float *x_f;
	struct stat fileinfo;
	int FILE_SIZE;
	char *err;
	char dummy[2];
	struct GRD_HEADER header;
	struct
	{
		struct Option *input, *output, *title, *bytes,
			      *north, *south, *east, *west,
			      *rows, *cols, *subst;
	} parm;
	struct
	{
	    struct Flag *s, *f, *b, *gmt_hd;
	} flag;
	union {
		int testWord;
		char testByte[4];
	} endianTest;
	int swapFlag;

	char *G_adjust_Cell_head();
	int G_scan_northing();
	int G_scan_easting();

	G_gisinit (argv[0]);

	parm.input = G_define_option();
	parm.input->key = "input";
	parm.input->type = TYPE_STRING;
	parm.input->required = YES;
	parm.input->description = "Bin raster file to be imported";

	parm.output = G_define_option();
	parm.output->key = "output";
	parm.output->type = TYPE_STRING;
	parm.output->required = YES;
	parm.output->description = "Name for resultant raster map";
	parm.output->gisprompt = "any,cell,raster";

	parm.title = G_define_option();
	parm.title->key = "title";
	parm.title->key_desc = "\"phrase\"";
	parm.title->type = TYPE_STRING;
	parm.title->required = NO;
	parm.title->description = "Title for resultant raster map";

	parm.bytes = G_define_option();
	parm.bytes->key = "bytes";
	parm.bytes->type = TYPE_INTEGER;
	parm.bytes->answer = "1";
	parm.bytes->required = NO;
	parm.bytes->description = "Number of bytes per cell (1, 2, 4)" ;

	parm.north = G_define_option();
	parm.north->key = "north";
	parm.north->type = TYPE_DOUBLE;
	parm.north->required = NO;
	parm.north->description = "Northern limit of geographic region" ;

	parm.south = G_define_option();
	parm.south->key = "south";
	parm.south->type = TYPE_DOUBLE;
	parm.south->required = NO;
	parm.south->description = "Southern limit of geographic region" ;

	parm.east = G_define_option();
	parm.east->key = "east";
	parm.east->type = TYPE_DOUBLE;
	parm.east->required = NO;
	parm.east->description = "Eastern limit of geographic region" ;

	parm.west = G_define_option();
	parm.west->key = "west";
	parm.west->type = TYPE_DOUBLE;
	parm.west->required = NO;
	parm.west->description = "Western limit of geographic region" ;

	parm.rows = G_define_option();
	parm.rows->key = "r";
	parm.rows->type = TYPE_DOUBLE;
	parm.rows->required = NO;
	parm.rows->description = "Number of rows";

	flag.s = G_define_flag();
	flag.s->key = 's';
	flag.s->description = "Signed data (high bit means negative value)";

	flag.f = G_define_flag();
        flag.f->key = 'f';
        flag.f->description = "Import Floating Point Data";

        flag.b = G_define_flag();
        flag.b->key = 'b';
        flag.b->description = "Byte Swap the Data During Import";

        flag.gmt_hd = G_define_flag();
        flag.gmt_hd->key = 'h';
        flag.gmt_hd->description = "Get region info from GMT style header";

	parm.cols = G_define_option();
	parm.cols->key = "c";
	parm.cols->type = TYPE_DOUBLE;
	parm.cols->required = NO;
	parm.cols->description = "Number of columns";

	parm.subst = G_define_option();
	parm.subst->key = "subst";
	parm.subst->type = TYPE_DOUBLE;
	parm.subst->required = NO;
	parm.subst->description = "Substitude value with NULL";

	if (G_parser(argc,argv))
		exit(1);
	input = parm.input->answer;
	output = parm.output->answer;
	if(title = parm.title->answer)
		G_strip (title);
	if (sscanf(parm.bytes->answer,"%d%1s",&bytes, dummy) != 1) return 1;
	if (bytes <= 0) return 1;
	sflag = flag.s->answer;

	swap = 0;
	if (flag.b->answer){
	 swap = 1;
	fprintf(stderr, "Byte Swapping Turned On!\n");
	}

/* Check Endian State of Host Computer*/
    endianTest.testWord = 1;
    if (endianTest.testByte[0] == 1) {
        swapFlag = 1; /*true: little endian */
        if (swap == 1) swapFlag = 0; /* Swapping enabled */
    } else {
        swapFlag = 0;
        if (swap == 1) swapFlag = 1; /* Swapping enabled */
    }


	cellhd.zone = G_zone();
	cellhd.proj = G_projection();

	if (!flag.gmt_hd->answer) {
	if (! G_scan_northing(parm.north->answer, &cellhd.north, cellhd.proj)) return 1;
	if (! G_scan_northing(parm.south->answer, &cellhd.south, cellhd.proj)) return 1;
	if (! G_scan_easting (parm.east->answer,  &cellhd.east,  cellhd.proj)) return 1;
	if (! G_scan_easting (parm.west->answer,  &cellhd.west,  cellhd.proj)) return 1;
	if (sscanf(parm.rows->answer,"%d%1s",&cellhd.rows, dummy) != 1
	|| cellhd.rows <= 0) return 1;
	if (sscanf(parm.cols->answer,"%d%1s",&cellhd.cols, dummy) != 1
	|| cellhd.cols <= 0) return 1;
	}

	if (parm.subst->answer !=NULL) 
	sscanf(parm.subst->answer, "%lf", &oldval);
	
	if (strcmp ("-", input) == 0)
	{
		Tmp_file = G_tempfile ();
		if (NULL == (Tmp_fd = fopen (Tmp_file, "w+")))
			perror (Tmp_file), exit (1);
		unlink (Tmp_file);
		if (0 > file_cpy (stdin, Tmp_fd))
			exit (1);
		fd = Tmp_fd;
	}
	else
		fd = fopen (input, "r");

	if (fd == NULL)
	{
		perror (input);
		G_usage();
		exit(-1) ;
	}

/* Read binary GMT style header */
	if (flag.gmt_hd->answer) {
fread(&header.nx, sizeof(int), 1, fd);
fread(&header.ny, sizeof(int), 1, fd);
fread(&header.node_offset, sizeof(int), 1, fd);
if(swapFlag == 0)
fread(&header.node_offset, sizeof(int), 1, fd);

fread(&header.x_min, sizeof(double), 1, fd);
fread(&header.x_max, sizeof(double), 1, fd);
fread(&header.y_min, sizeof(double), 1, fd);
fread(&header.y_max, sizeof(double), 1, fd);
fread(&header.z_min, sizeof(double), 1, fd);
fread(&header.z_max, sizeof(double), 1, fd);
fread(&header.x_inc, sizeof(double), 1, fd);
fread(&header.y_inc, sizeof(double), 1, fd);
fread(&header.z_scale_factor, sizeof(double), 1, fd);
fread(&header.z_add_offset, sizeof(double), 1, fd);

fread(&header.x_units ,sizeof(char[GRD_UNIT_LEN]), 1, fd);
fread(&header.y_units ,sizeof(char[GRD_UNIT_LEN]), 1, fd);
fread(&header.z_units ,sizeof(char[GRD_UNIT_LEN]), 1, fd);
fread(&header.title ,sizeof(char[GRD_TITLE_LEN]), 1, fd);
fread(&header.command ,sizeof(char[GRD_COMMAND_LEN]), 1, fd);
fread(&header.remark ,sizeof(char[GRD_REMARK_LEN]), 1, fd);

cellhd.cols = header.nx;
cellhd.rows = header.ny;
cellhd.west = header.x_min;
cellhd.east = header.x_max;
cellhd.south = header.y_min;
cellhd.north = header.y_max;
cellhd.ew_res = header.x_inc;
cellhd.ns_res = header.y_inc;

if (swap == 1) {
/* Swapping Header Values */
TIFFSwabLong((uint32 *)&cellhd.cols);
TIFFSwabLong((uint32 *)&cellhd.rows);
TIFFSwabDouble(&cellhd.west);
TIFFSwabDouble(&cellhd.east);
TIFFSwabDouble(&cellhd.south);
TIFFSwabDouble(&cellhd.north);
TIFFSwabDouble(&cellhd.ew_res);
TIFFSwabDouble(&cellhd.ns_res);
}
}

	/* Adjust Cell Header to New Values */
       if (err = G_adjust_Cell_head (&cellhd, 1, 1)) {
                fprintf(stderr, "%s\n", err);
                return 1;
        }

	nrows = cellhd.rows;
	ncols = cellhd.cols;
	if(G_set_window (&cellhd) < 0)
		exit(3);

	if (nrows != G_window_rows())
	{
		fprintf (stderr, "OOPS: rows changed from %d to %d\n", nrows, G_window_rows());
		exit(1);
	}
	if (ncols != G_window_cols())
	{
		fprintf (stderr, "OOPS: cols changed from %d to %d\n", ncols, G_window_cols());
		exit(1);
	}

/* Find File Size in Byte and Check against byte size */
stat(input, &fileinfo);
FILE_SIZE = fileinfo.st_size;

if (flag.gmt_hd->answer) {
if(swapFlag == 0) {
if (FILE_SIZE != (896 + (ncols*nrows*bytes))) {
fprintf(stderr, "Bytes do not match File size\n");
fprintf(stderr, "File Size %d ... Total Bytes %d\n", FILE_SIZE, 896 + (ncols*nrows*bytes) );
fprintf(stderr, "Try bytes=%d\n", (FILE_SIZE-896) / (ncols*nrows) );
exit(1);
} 
} else {
if (FILE_SIZE != (892 + (ncols*nrows*bytes))) {
fprintf(stderr, "Bytes do not match File size\n");
fprintf(stderr, "File Size %d ... Total Bytes %d\n", FILE_SIZE, 892 + (ncols*nrows*bytes) );
fprintf(stderr, "Try bytes=%d\n", (FILE_SIZE-892) / (ncols*nrows) );
exit(1);
}
}
} else {
/* No Header */
if (FILE_SIZE != (ncols*nrows*bytes)) {
fprintf(stderr, "Bytes do not match File size\n");
fprintf(stderr, "File Size %d ... Total Bytes %d\n", FILE_SIZE, ncols*nrows*bytes);
fprintf(stderr, "Try bytes=%d\n", FILE_SIZE/(ncols*nrows) );
exit(1);
}
}

	if (flag.f->answer) { 
	map_type = FCELL_TYPE;
	fcell = G_allocate_f_raster_buf();
	} else {
	cell = G_allocate_c_raster_buf();
	map_type = CELL_TYPE;
	}
	
	cf = G_open_raster_new(output, map_type);
	if (cf < 0)
	{
		char msg[100];
		sprintf (msg, "unable to create raster map %s", output);
		G_fatal_error (msg);
		exit(1);
	}
	if (bytes < 4) {
	/* malloc as short */
	x_s = malloc(ncols * bytes);
	if (x_s == NULL) {
                fprintf(stderr, "Impossible allocation\n");
                exit(1);
        }
	} else if (flag.f->answer) {
	/* malloc as float */
	x_f = malloc(ncols * bytes);
	if (x_f == NULL) {
		fprintf(stderr, "Impossible allocation\n");
		exit(1);
	}
	} else {
	/* malloc as int*/
	x_i = malloc(ncols * bytes);
        if (x_i == NULL) {
                fprintf(stderr, "Impossible allocation\n");
                exit(1);
        }
	}

fprintf(stderr, "Percent Complete: ");
	for (row = 1; row <= nrows; row++)
	{
		if(bytes < 4) {
		if (fread (x_s, bytes * ncols, 1, fd) != 1)
                {
                        char msg[100];
                        G_unopen_cell (cf);
                        sprintf (msg, "Conversion failed at row %d\n", row);
                        G_fatal_error (msg);
                        exit(1);
                }
		} else if(flag.f->answer){
		if (fread (x_f, bytes * ncols, 1, fd) != 1)
		{
			char msg[100];
			G_unopen_cell (cf);
			sprintf (msg, "Conversion failed at row %d\n", row);
			G_fatal_error (msg);
			exit(1);
		}
		} else {
                if (fread (x_i, bytes * ncols, 1, fd) != 1)
                {
                        char msg[100];
                        G_unopen_cell (cf);
                        sprintf (msg, "Conversion failed at row %d\n", row);
                        G_fatal_error (msg);
                        exit(1);
                }
		}
		for (col = 0 ; col < ncols; col++ ) {
	if(bytes < 4) {
	/* Import 2 byte Short */
	if (swap == 1 && bytes == 2)
	    TIFFSwabShort((uint16 *)&x_s[col]);
	cell[col] = (CELL) (bytes == 1) ? *(((uint8 *)x_s) + col): x_s[col] ;
	if (sflag && cell[col] > 127) cell[col] -= 256;
	if(oldval) {
	if (cell[col] == (int)oldval) G_set_c_null_value(&cell[col], 1) ;
	}
	} else if (flag.f->answer) {
	/* Import Float */
	if (swap == 1) TIFFSwabLong((uint32 *)&x_f[col]);
	fcell[col] = (FCELL)x_f[col] ;
	if(oldval) {
	if (fcell[col] == (float)oldval) G_set_f_null_value(&fcell[col], 1);
	}
	} else {
	/* Import 4 byte Int */
	if (swap == 1) TIFFSwabLong((uint32 *)&x_i[col]);
        cell[col] = (CELL)x_i[col] ;
        if (sflag && cell[col] > 127) cell[col] -= 256;
	if(oldval) {
        if (cell[col] == (int)oldval) G_set_c_null_value(&cell[col], 1) ;
	}	
	}
		}
		G_percent(row, nrows, 2);
	if (flag.f->answer) {
	G_put_f_raster_row(cf, fcell);
	} else {
	G_put_c_raster_row(cf, cell);
	}
	}
	fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", output);
	G_close_cell (cf);
	if (title)
		G_put_cell_title (output, title);

	exit (0);
}

int 
file_cpy (FILE *from, FILE *to)
{
	char buf[BUFSIZ];
	long size;
	int  written = 0;

	while (1)
	{
		size = fread (buf, 1, BUFSIZ, from);
		if (!size)
		{
			if (written)
			{
				fflush (to);
				fseek (to, 0l, 0);
			}
			return (0);
		}
		if (!fwrite (buf, 1, size, to))
		{
			perror ("file copy");
			return (-1);
		}
		written = 1;
	}
	/* NOTREACHED */
}

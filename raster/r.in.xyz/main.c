/*
 * r.in.xyz
 *
 *  Calculates univariate statistics from the non-null cells of a GRASS raster map
 *
 *   Copyright 2006 by M. Hamish Bowman, and The GRASS Development Team
 *   Author: M. Hamish Bowman, University of Otago, Dunedin, New Zealand
 *
 *   This program is free software licensed under the GPL (>=v2).
 *   Read the COPYING file that comes with GRASS for details.
 *
 *   This program is intended as a replacement for the GRASS 5 s.cellstats module.
 */

#include <grass/config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "local_proto.h"


int main(int argc, char *argv[])
{

    FILE   *in_fp;
    int    out_fd;
    char   *infile, *outmap;
    int    xcol, ycol, zcol, max_col, percent;
    int    do_zfilter;
    int    method = -1;
    int    bin_n, bin_min, bin_max, bin_sum, bin_sumsq;
    double zrange_min, zrange_max, d_tmp;
    char   *fs; /* field delim */
    off_t  filesize;
    int    linesize, estimated_lines;
    int    from_stdin = FALSE;

    RASTER_MAP_TYPE rtype;
    struct History history;
    char   title[64];
    void   *n_array, *min_array, *max_array, *sum_array, *sumsq_array;
    void   *raster_row, *ptr;
    struct Cell_head region;
    int    rows, cols; /* scan box size */
    int    row, col; /* counters */

    int    pass, npasses, line;
    char   buff[BUFFSIZE];
    double x,y,z;
    char   **tokens;
    int    ntokens;  /* number of tokens */
    double pass_north, pass_south;
    int    arr_row, arr_col, count, count_total;

    double min = 0.0/0.0; /* init as nan */
    double max = 0.0/0.0; /* init as nan */
    size_t offset, n_offset;
    int    n = 0;
    double sum = 0.;
    double sumsq = 0.;
    double variance;

    struct GModule *module;
    struct Option *input_opt, *output_opt, *delim_opt, *percent_opt, *type_opt;
    struct Option *method_opt, *xcol_opt, *ycol_opt, *zcol_opt, *zrange_opt;
    struct Flag *scan_flag, *shell_style;


    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("raster");
    module->description =
      _("Create a raster map from an assemblage of many coordinates using univariate statistics.");

    input_opt = G_define_standard_option(G_OPT_F_INPUT);
    input_opt->description = _("ASCII file containing input data (or \"-\" to read from stdin)");

    output_opt = G_define_standard_option(G_OPT_R_OUTPUT);

    method_opt = G_define_option();
    method_opt->key = "method";
    method_opt->type = TYPE_STRING;
    method_opt->required = NO;
    method_opt->description = _("Statistic to use for raster values");
    method_opt->options = "n,min,max,range,sum,mean,stddev,variance,coeff_var";
    method_opt->answer = "mean";

    type_opt = G_define_option();
    type_opt->key = "type";
    type_opt->type = TYPE_STRING;
    type_opt->required = NO;
    type_opt->options = "CELL,FCELL,DCELL";
    type_opt->answer = "FCELL";
    type_opt->description = _("Storage type for resultant raster map");

    delim_opt = G_define_standard_option(G_OPT_F_SEP);

    xcol_opt = G_define_option();
    xcol_opt->key = "x";
    xcol_opt->type = TYPE_INTEGER;
    xcol_opt->required = NO;
    xcol_opt->answer = "1";
    xcol_opt->description =
	_("Column number of x coordinates in input file (first column is 1)");

    ycol_opt = G_define_option();
    ycol_opt->key = "y";
    ycol_opt->type = TYPE_INTEGER;
    ycol_opt->required = NO;
    ycol_opt->answer = "2";
    ycol_opt->description =
	_("Column number of y coordinates in input file");

    zcol_opt = G_define_option();
    zcol_opt->key = "z";
    zcol_opt->type = TYPE_INTEGER;
    zcol_opt->required = NO;
    zcol_opt->answer = "3";
    zcol_opt->description =
	_("Column number of data values in input file");

    zrange_opt = G_define_option();
    zrange_opt->key = "zrange";
    zrange_opt->type = TYPE_DOUBLE;
    zrange_opt->required = NO;
    zrange_opt->key_desc   = "min,max";
    zrange_opt->description = _("Filter range for z data (min,max)");

    percent_opt = G_define_option();
    percent_opt->key = "percent";
    percent_opt->type = TYPE_INTEGER;
    percent_opt->required = NO;
    percent_opt->answer = "100";
    percent_opt->options = "1-100";
    percent_opt->description = _("Percent of map to keep in memory");

    scan_flag = G_define_flag();
    scan_flag->key = 's';
    scan_flag->description = _("Scan data file for extent then exit");

    shell_style = G_define_flag();
    shell_style->key = 'g';
    shell_style->description = _("In scan mode, print using shell script style");


    if (G_parser(argc,argv))
	exit(EXIT_FAILURE);


    /* parse input values */
    infile = input_opt->answer;
    outmap = output_opt->answer;
 
    if (shell_style->answer && !scan_flag->answer) {
        scan_flag->answer = 1;
    }

    fs = delim_opt->answer;
    if ( strcmp(fs,"\\t") == 0 ) fs = "\t";
    if ( strcmp(fs,"tab") == 0 ) fs = "\t";
    if ( strcmp(fs,"space") == 0 ) fs = " ";

    xcol = atoi(xcol_opt->answer);
    ycol = atoi(ycol_opt->answer);
    zcol = atoi(zcol_opt->answer);
    if( (xcol < 0) || (ycol < 0) || (zcol < 0))
	G_fatal_error(_("Please specify a reasonable column number."));
    max_col = (xcol > ycol) ? xcol : ycol;
    max_col = (zcol > max_col) ? zcol : max_col;

    percent = atoi(percent_opt->answer);

    /* parse zrange */
    do_zfilter = FALSE;
    if (zrange_opt->answer != NULL) {     /* should this be answerS ? */ 
	sscanf(zrange_opt->answers[0], "%lf", &zrange_min);
	sscanf(zrange_opt->answers[1], "%lf", &zrange_max);
	do_zfilter = TRUE;

	if (zrange_min > zrange_max) {
	    d_tmp = zrange_max;
	    zrange_max = zrange_min;
	    zrange_min = d_tmp;
	}
    }

   /* figure out what maps we need in memory */
    /*  n         n
        min       min
        max       max
        range     min max        max - min
        sum       sum
        mean      sum n          sum/n
        stddev    sum sumsq n    sqrt((sumsq - sum*sum/n)/n)
        variance  sum sumsq n    (sumsq - sum*sum/n)/n
        coeff_var sum sumsq n    sqrt((sumsq - sum*sum/n)/n) / (sum/n)
    */
    bin_n   = FALSE;
    bin_min = FALSE;
    bin_max = FALSE;
    bin_sum = FALSE;
    bin_sumsq = FALSE;

    if( strcmp(method_opt->answer, "n") == 0 ) {
	method = METHOD_N;
	bin_n  = TRUE;
    }
    if( strcmp(method_opt->answer, "min") == 0 ) {
	method = METHOD_MIN;
	bin_min = TRUE;
    }
    if( strcmp(method_opt->answer, "max") == 0 ) {
	method = METHOD_MAX;
	bin_max = TRUE;
    }
    if( strcmp(method_opt->answer, "range") == 0 ) {
	method = METHOD_RANGE;
	bin_min = TRUE;
	bin_max = TRUE;
    }
    if( strcmp(method_opt->answer, "sum") == 0 ) {
	method = METHOD_SUM;
	bin_sum = TRUE;
    }
    if( strcmp(method_opt->answer, "mean") == 0 ) {
	method = METHOD_MEAN;
	bin_sum = TRUE;
	bin_n = TRUE;
    }
    if( strcmp(method_opt->answer, "stddev") == 0 ) {
	method = METHOD_STDDEV;
	bin_sum = TRUE;
	bin_sumsq = TRUE;
	bin_n = TRUE;
    }
    if( strcmp(method_opt->answer, "variance") == 0 ) {
	method = METHOD_VARIANCE;
	bin_sum = TRUE;
	bin_sumsq = TRUE;
	bin_n = TRUE;
    }
    if( strcmp(method_opt->answer, "coeff_var") == 0 ) {
	method = METHOD_COEFF_VAR;
	bin_sum = TRUE;
	bin_sumsq = TRUE;
	bin_n = TRUE;
    }

    if(strcmp("CELL", type_opt->answer) == 0)
	rtype = CELL_TYPE;
    else if(strcmp("DCELL", type_opt->answer) == 0)
	rtype = DCELL_TYPE;
    else 
	rtype = FCELL_TYPE;

    if(method == METHOD_N)
	rtype = CELL_TYPE;


    G_get_window (&region);
    rows = (int)(region.rows * (percent/100.0));
    cols = region.cols;

    G_debug(2, "region.n=%f  region.s=%f  region.ns_res=%f", region.north,
	region.south, region.ns_res);
    G_debug(2, "region.rows=%d  [box_rows=%d]  region.cols=%d", region.rows,
	rows, region.cols);

    npasses = (int)ceil( 1.0 * region.rows / rows);

    if( ! scan_flag->answer) {
	/* allocate memory (test for enough before we start) */
	if(bin_n)
	    n_array = G_calloc(rows*(cols+1), G_raster_size(CELL_TYPE));
	if(bin_min)
	    min_array = G_calloc(rows*(cols+1), G_raster_size(rtype));
	if(bin_max)
	    max_array = G_calloc(rows*(cols+1), G_raster_size(rtype));
	if(bin_sum)
	    sum_array = G_calloc(rows*(cols+1), G_raster_size(rtype));
	if(bin_sumsq)
	    sumsq_array = G_calloc(rows*(cols+1), G_raster_size(rtype));

	/* and then free it again */
	if(bin_n) G_free(n_array);
	if(bin_min) G_free(min_array);
	if(bin_max) G_free(max_array);
	if(bin_sum) G_free(sum_array);
	if(bin_sumsq) G_free(sumsq_array);
	/** end memory test **/
    }


    /* open input file */
    if (strcmp ("-", infile) == 0) {
	from_stdin = TRUE;
	in_fp = stdin;
	strcpy(infile, "stdin"); /* filename for history metadata */  /* need to realloc()?? */
	/* can't rewind() stdin; dumping to a tmp file first is slow and prone to LFS problems */
	if(npasses != 1) {
	    G_warning(_("Can only perform a single pass if input is from stdin."));
	    npasses = 1;
	}
    }
    else {
	if((in_fp = fopen(infile, "r" )) == NULL )
	    G_fatal_error(_("Could not open input file <%s>."), infile);
    }

    if(scan_flag->answer) {
	if( zrange_opt->answer )
	    G_warning(_("zrange will not be taken into account during scan"));
	scan_bounds(in_fp, xcol, ycol, zcol, fs, shell_style->answer);
	fclose(in_fp);
	exit(EXIT_SUCCESS);
    }


    /* open output map */
    out_fd = G_open_raster_new(outmap, rtype);
    if (out_fd < 0)
	G_fatal_error(_("Unable to create raster map <%s>"), outmap);

    if(!from_stdin) {
	/* guess at number of lines in the file without actually reading it all in */
	for(line=0; line<10; line++) {  /* arbitrarily use 10th line for guess */
	    if( 0 == G_getl2(buff, BUFFSIZE-1, in_fp) ) break;
	    linesize = strlen(buff) + 1;
	}
	fseek(in_fp, 0L, SEEK_END);
	filesize = ftell(in_fp);
	rewind(in_fp);
	if(linesize < 6)  /* min possible: "0,0,0\n" */
	    linesize = 6;
	estimated_lines = filesize/linesize;
	G_debug(2, "estimated number of lines in file: %d", estimated_lines);
    }
    else
	estimated_lines = -1;

    /* allocate memory for a single row of output data */
    raster_row = G_allocate_raster_buf(rtype);

    G_message(_("Scanning data ..."));

    count_total = 0;

    /* main binning loop(s) */
    for(pass=1; pass <= npasses; pass++ ) {
	if(npasses > 1)
	    G_message(_("Pass #%d (of %d) ..."), pass, npasses);

	if(!from_stdin)
	    rewind(in_fp);

	/* figure out segmentation */
	pass_north = region.north - (pass-1)*rows*region.ns_res;
	if( pass == npasses )
	    rows = region.rows - (pass-1)*rows;
	pass_south = pass_north - rows*region.ns_res;

	G_debug(2, "pass=%d/%d  pass_n=%f  pass_s=%f  rows=%d",
	  pass, npasses, pass_north, pass_south, rows);


	if(bin_n) {
	    G_debug(2, "allocating n_array");
	    n_array = G_calloc(rows*(cols+1), G_raster_size(CELL_TYPE));
	    blank_array(n_array, rows, cols, CELL_TYPE, 0);
	}
	if(bin_min) {
	    G_debug(2, "allocating min_array");
	    min_array = G_calloc(rows*(cols+1), G_raster_size(rtype));
	    blank_array(min_array, rows, cols, rtype, -1); /* fill with NULLs */
	}
	if(bin_max) {
	    G_debug(2, "allocating max_array");
	    max_array = G_calloc(rows*(cols+1), G_raster_size(rtype));
	    blank_array(max_array, rows, cols, rtype, -1); /* fill with NULLs */
	}
	if(bin_sum) {
	    G_debug(2, "allocating sum_array");
	    sum_array = G_calloc(rows*(cols+1), G_raster_size(rtype));
	    blank_array(sum_array, rows, cols, rtype, 0);
	}
	if(bin_sumsq) {
	    G_debug(2, "allocating sumsq_array");
	    sumsq_array = G_calloc(rows*(cols+1), G_raster_size(rtype));
	    blank_array(sumsq_array, rows, cols, rtype, 0);
	}


	line = 0;
	count = 0;
	G_percent_reset();

	while( 0 != G_getl2(buff, BUFFSIZE-1, in_fp) ) {
	    line++;

	    if(line%10000 == 0) { /* mod for speed */
		if(from_stdin)
		    G_clicker();
		else if(line < estimated_lines)
		    G_percent(line, estimated_lines, 3);
	    }

	    if((buff[0] == '#') || (buff[0] == '\0')) {
		continue; /* line is a comment or blank */
	    }

	    G_chop(buff); /* remove leading and trailing whitespace from the string.  unneded?? */
	    tokens = G_tokenize (buff, fs);
	    ntokens = G_number_of_tokens ( tokens );

	    if((ntokens < 3) || (max_col > ntokens) )
		G_fatal_error(_("Not enough data columns. "
		   "Incorrect delimiter or column number? "
		   "Found the following character(s) in row %d:\n[%s]"), line, buff);

/* too slow?
	    if ( G_projection() == PROJECTION_LL ) {
		G_scan_easting( tokens[xcol-1], &x, region.proj);
		G_scan_northing( tokens[ycol-1], &y, region.proj);
	    }
	    else {
*/
	    if( 1 != sscanf(tokens[ycol-1], "%lf", &y) )
		G_fatal_error(_("Bad y-coordinate line %d column %d. <%s>"), line, ycol, tokens[ycol-1]);
	    if (y <= pass_south || y > pass_north) {
		G_free_tokens(tokens);
		continue;
	    }
	    if( 1 != sscanf(tokens[xcol-1], "%lf", &x) )
		G_fatal_error(_("Bad x-coordinate line %d column %d. <%s>"), line, xcol, tokens[xcol-1]);
	    if (x < region.west || x > region.east) {
		G_free_tokens(tokens);
		continue;
	    }
	    if( 1 != sscanf(tokens[zcol-1], "%lf", &z) )
		G_fatal_error(_("Bad z-coordinate line %d column %d. <%s>"), line, zcol, tokens[zcol-1]);
	    if(zrange_opt->answer) {
		if (z < zrange_min || z > zrange_max) {
		    G_free_tokens(tokens);
		    continue;
		}
	    }

	    count++;
/*	    G_debug(5, "x: %f, y: %f, z: %f", x, y, z); */
	    G_free_tokens(tokens);

	    /* find the bin in the current array box */
	    arr_row = (int)((pass_north - y) / region.ns_res);
	    arr_col = (int)((x - region.west) / region.ew_res);

/*	    G_debug(5, "arr_row: %d   arr_col: %d", arr_row, arr_col); */

	   /* The range should be [0,cols-1]. We use (int) to round down,
		but if the point exactly on eastern edge arr_col will be /just/
		on the max edge .0000000 and end up on the next row.
		We could make above bounds check "if(x>=region.east) continue;"
		But instead we go to all sorts of trouble so that not one single
		data point is lost. GE is too small to catch them all.
		We don't try to make y happy as percent segmenting will make some
		points happen twice that way; so instead we use the y<= test above.
		*/
	    if (arr_col >= cols) {
		if( ((x - region.west) / region.ew_res) - cols < 10*GRASS_EPSILON)
		    arr_col--;
		else { /* oh well, we tried. */
		    G_debug(3, "skipping extraneous data point [%.3f], column %d of %d",
			x, arr_col, cols);
		    continue;
		}
	    }

	    if(bin_n)
		update_n(n_array, cols, arr_row, arr_col);
	    if(bin_min)
		update_min(min_array, cols, arr_row, arr_col, rtype, z);
	    if(bin_max)
		update_max(max_array, cols, arr_row, arr_col, rtype, z);
	    if(bin_sum)
		update_sum(sum_array, cols, arr_row, arr_col, rtype, z);
	    if(bin_sumsq)
		update_sumsq(sumsq_array, cols, arr_row, arr_col, rtype, z);

	} /* while !EOF */
	G_debug(2, "pass %d finished, %d coordinates in box", pass, count);
	count_total += count;

	/* calc stats and output */
	G_message(_("Writing to map ..."));
	for(row = 0; row<rows; row++) {

	    switch(method)
	    {
	    case METHOD_N:  /* n is a straight copy */
		G_raster_cpy(raster_row, n_array+(row*cols*G_raster_size(CELL_TYPE)),
		   cols, CELL_TYPE);
		break;

	    case METHOD_MIN:
		G_raster_cpy(raster_row, min_array+(row*cols*G_raster_size(rtype)),
		   cols, rtype);
		break;

	    case METHOD_MAX:
		G_raster_cpy(raster_row, max_array+(row*cols*G_raster_size(rtype)),
		   cols, rtype);
		break;

	    case METHOD_SUM:
		G_raster_cpy(raster_row, sum_array+(row*cols*G_raster_size(rtype)),
		   cols, rtype);
		break;

	    case METHOD_RANGE:   /* (max-min)*/
		ptr = raster_row;
		for (col=0; col<cols; col++) {
		    offset = (row*cols + col) * G_raster_size(rtype);
		    min = G_get_raster_value_d(min_array + offset, rtype);
		    max = G_get_raster_value_d(max_array + offset, rtype);
		    G_set_raster_value_d(ptr, max - min, rtype);
		    ptr = G_incr_void_ptr(ptr, G_raster_size(rtype));
		}
		break;

	    case METHOD_MEAN:   /* (sum / n) */
		ptr = raster_row;
		for (col=0; col<cols; col++) {
		    offset = (row*cols + col) * G_raster_size(rtype);
		    n_offset = (row*cols + col) * G_raster_size(CELL_TYPE);
		    n   = G_get_raster_value_c(n_array + n_offset, CELL_TYPE);
		    sum = G_get_raster_value_d(sum_array + offset, rtype);

		    if(n == 0)
			G_set_null_value(ptr, 1, rtype);
		    else
			G_set_raster_value_d(ptr, (sum / n), rtype);

		    ptr = G_incr_void_ptr(ptr, G_raster_size(rtype));
		}
		break;

	    case METHOD_STDDEV:     /*  sqrt(variance)        */
	    case METHOD_VARIANCE:   /*  (sumsq - sum*sum/n)/n */
	    case METHOD_COEFF_VAR:  /*  100 * stdev / mean    */
		ptr = raster_row;
		for (col=0; col<cols; col++) {
		    offset = (row*cols + col) * G_raster_size(rtype);
		    n_offset = (row*cols + col) * G_raster_size(CELL_TYPE);
		    n     = G_get_raster_value_c(n_array + n_offset, CELL_TYPE);
		    sum   = G_get_raster_value_d(sum_array + offset, rtype);
		    sumsq = G_get_raster_value_d(sumsq_array + offset, rtype);

		    if(n == 0)
			G_set_null_value(ptr, 1, rtype);
		    else {
			variance = (sumsq - sum*sum/n)/n;
			if( variance < GRASS_EPSILON )
			    variance = 0.0;

			if(method == METHOD_STDDEV)
			    G_set_raster_value_d(ptr, sqrt(variance), rtype);

			else if(method == METHOD_VARIANCE)
			    G_set_raster_value_d(ptr, variance, rtype);

			else if(method == METHOD_COEFF_VAR)
			    G_set_raster_value_d(ptr, 100*sqrt(variance)/(sum/n), rtype);

		    }
		    ptr = G_incr_void_ptr(ptr, G_raster_size(rtype));
		}
		break;

	    default:
		G_fatal_error("?");
	    }

	    /* write out line of raster data */
	    if( 1 != G_put_raster_row(out_fd, raster_row, rtype) ) {
		G_close_cell(out_fd);
		G_fatal_error(_("Writing map, row %d"), ((pass-1)*rows)+row);
	    }
	}

	/* free memory */
	if(bin_n) G_free(n_array);
	if(bin_min) G_free(min_array);
	if(bin_max) G_free(max_array);
	if(bin_sum) G_free(sum_array);
	if(bin_sumsq) G_free(sumsq_array);

    } /* passes loop */

    G_percent(1,1,1); /* flush */
    G_free(raster_row);

    /* close input file */
    if(!from_stdin)
	fclose(in_fp);

    /* close raster file & write history */
    G_close_cell(out_fd);

    sprintf(title, "Raw x,y,z data binned into a raster grid by cell %s", method_opt->answer);
    G_put_cell_title(outmap, title);

    G_short_history(outmap, "raster", &history);
    G_command_history(&history);
    strncpy(history.datsrc_1, infile, RECORD_LEN);
    history.datsrc_1[RECORD_LEN-1] = '\0'; /* strncpy() doesn't null terminate if maxfill */
    G_write_history(outmap, &history);


    sprintf(buff, _("%d points found in region."), count_total);
    G_done_msg(buff);
    G_debug(1, "Processed %d lines.", line);

    exit(EXIT_SUCCESS);

}



int scan_bounds(FILE* fp, int xcol, int ycol, int zcol, char *fs, int shell_style)
{
    int    line, first, max_col;
    char   buff[BUFFSIZE];
    double min_x, max_x, min_y, max_y, min_z, max_z;
    char   **tokens;
    int    ntokens;   /* number of tokens */
    double x,y,z;

    max_col = (xcol > ycol) ? xcol : ycol;
    max_col = (zcol > max_col) ? zcol : max_col;

    line = 0;
    first = TRUE;

    while( 0 != G_getl2(buff, BUFFSIZE-1, fp) ) {
	line++;

	if((buff[0] == '#') || (buff[0] == '\0')) {
	    continue; /* line is a comment or blank */
	}

	G_chop(buff); /* remove leading and trailing whitespace. unneded?? */
	tokens = G_tokenize (buff, fs);
	ntokens = G_number_of_tokens ( tokens );

	if((ntokens < 3) || (max_col > ntokens) )
	    G_fatal_error(_("Not enough data columns. "
	       "Incorrect delimiter or column number?\n[%s]"), buff);

/* too slow?
	    if ( G_projection() == PROJECTION_LL ) {
		G_scan_easting( tokens[xcol-1], &x, region.proj);
		G_scan_northing( tokens[ycol-1], &y, region.proj);
	    }
	    else {
*/
	if( 1 != sscanf(tokens[xcol-1], "%lf", &x) )
	    G_fatal_error(_("Bad x-coordinate line %d column %d. <%s>"), line, xcol, tokens[xcol-1]);

	if(first) {
	    min_x = x;
	    max_x = x;
	}
	else {
	    if ( x < min_x ) min_x = x;
	    if ( x > max_x ) max_x = x;
	}

	if( 1 != sscanf(tokens[ycol-1], "%lf", &y) )
	    G_fatal_error(_("Bad y-coordinate line %d column %d. <%s>"), line, ycol, tokens[ycol-1]);

	if(first) {
	    min_y = y;
	    max_y = y;
	}
	else {
	    if ( y < min_y ) min_y = y;
	    if ( y > max_y ) max_y = y;
	}

	if( 1 != sscanf(tokens[zcol-1], "%lf", &z) )
	    G_fatal_error(_("Bad z-coordinate line %d column %d. <%s>"), line, zcol, tokens[zcol-1]);

	if(first) {
	    min_z = z;
	    max_z = z;
	    first = FALSE;
	}
	else {
	    if ( z < min_z ) min_z = z;
	    if ( z > max_z ) max_z = z;
	}


	G_free_tokens(tokens);
    }

    if (!shell_style) {
	fprintf(stderr,_("Range:     min         max\n"));
	fprintf(stdout,"x: %11f %11f\n", min_x, max_x);
	fprintf(stdout,"y: %11f %11f\n", min_y, max_y);
	fprintf(stdout,"z: %11f %11f\n", min_z, max_z);
   } else
        fprintf(stdout,"n=%f s=%f e=%f w=%f b=%f t=%f\n", 
	  max_y, min_y, max_x, min_x, min_z, max_z);

    G_debug(1, "Processed %d lines.", line);
    G_debug(1, "region template: g.region n=%f s=%f e=%f w=%f",
      max_y, min_y, max_x, min_x);

    return 0;
}

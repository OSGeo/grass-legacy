/***************************************************************
 *
 * MODULE:       v.univar
 * 
 * AUTHOR(S):    Radim Blazek
 *               Hamish Bowman, University of Otago, New Zealand (r.univar2)
 *               
 * PURPOSE:      Univariate Statistics for attribute
 *               
 * COPYRIGHT:    (C) 2004 by the GRASS Development Team
 *
 *               This program is free software under the 
 *               GNU General Public License (>=v2). 
 *               Read the file COPYING that comes with GRASS
 *               for details.
 *
 **************************************************************/
#include <stdlib.h> 
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "dbmi.h"

int 
main (int argc, char *argv[])
{
    struct GModule *module;
    struct Option  *map_opt, *type_opt, *field_opt, *col_opt;
    struct Flag *shell_flag;
    char   *mapset;
    struct Map_info  Map;
    struct field_info *Fi;
    dbDriver *Driver;
    dbCatValArray Cvarr;
    struct line_pnts *Points;
    struct line_cats *Cats;
    int    otype, ofield;
    int    compatible = 1; /* types are compatible: point+centroid or line+boundary or area */
    int    nrec, ctype, nlines, line, nareas, area;
    int    nmissing = 0; /* number of missing atttributes */
    int    nnull = 0; /* number of null values */
    int    first = 1;

    /* Statistics */
    int count = 0; /* number of features with non-null attribute */
    double sum = 0.0;
    double sumsq = 0.0;
    double min = 0.0/0.0; /* init as nan */
    double max = 0.0/0.0;
    double mean, pop_variance, sample_variance, pop_stdev, sample_stdev;
    double total_size = 0.0;     /* total size: length/area */
    
    module = G_define_module();
    module->description = "Calculates univariate statistics for attribute. Variance and standard "
	                  "deviation is calculated only for points.";

    map_opt = G_define_standard_option(G_OPT_V_INPUT);
    map_opt->key = "map";

    type_opt = G_define_standard_option(G_OPT_V_TYPE) ;
    type_opt->options = "point,line,boundary,centroid,area";
    
    col_opt = G_define_option();
    col_opt->key            = "column";
    col_opt->type           = TYPE_STRING;
    col_opt->required       = YES;
    col_opt->multiple       = NO;
    col_opt->description    = "Column name";

    field_opt = G_define_standard_option(G_OPT_V_FIELD);
    field_opt->answer = "1";

    shell_flag = G_define_flag();
    shell_flag->key = 'g';
    shell_flag->description = "Print the stats in shell script style";

    G_gisinit(argv[0]);
    if (G_parser (argc, argv))
	exit(-1); 
    
    otype = Vect_option_to_types ( type_opt );
    ofield = atoi ( field_opt->answer ); 
    
    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();
    
    /* open input vector */
    if ((mapset = G_find_vector2 (map_opt->answer, "")) == NULL) 
	G_fatal_error ( "Vector '%s' not found\n", map_opt->answer);
    
    Vect_set_open_level (2); 
    Vect_open_old (&Map, map_opt->answer, mapset); 

    /* Check if types are compatible */
    if ( (otype & GV_POINTS) && ( (otype & GV_LINES) || (otype & GV_AREA) ) ) compatible = 0;
    if ( (otype & GV_LINES) && ( otype & GV_AREA ) ) compatible = 0;
    
    if ( !compatible ) {
	G_warning ("Incompatible vector type(s), only number of features, minimum, maximum and range "
		   "can be calculated");
    }

    /* Read attributes */
    db_CatValArray_init ( &Cvarr );
    Fi = Vect_get_field( &Map, ofield);
    if ( Fi == NULL ) {
	G_fatal_error ("Cannot read field info");
    }

    Driver = db_start_driver_open_database ( Fi->driver, Fi->database );
    if (Driver == NULL)
	G_fatal_error("Cannot open database %s by driver %s", Fi->database, Fi->driver);
    
    /* Note do not check if the column exists in the table because it may be an expression */

    nrec = db_select_CatValArray ( Driver, Fi->table, Fi->key, col_opt->answer, NULL, &Cvarr );
    G_debug (2, "nrec = %d", nrec );

    ctype = Cvarr.ctype;
    if ( ctype != DB_C_TYPE_INT && ctype != DB_C_TYPE_DOUBLE )
	G_fatal_error ( "Column type not supported" );

    if ( nrec < 0 ) G_fatal_error ("Cannot select data from table");

    db_close_database_shutdown_driver(Driver);

    /* Lines */
    nlines = Vect_get_num_lines ( &Map );

    for ( line = 1 ; line <= nlines; line++ ) {
	int i, type;
		    
	G_debug ( 3, "line = %d", line );
	
	type = Vect_read_line ( &Map, Points, Cats, line );
	if ( !(type & otype ) ) continue;

	for ( i = 0; i < Cats->n_cats; i++ ) {
	    if ( Cats->field[i] == ofield ) {
		double val;
		dbCatVal *catval;
	
		G_debug ( 3, "cat = %d", Cats->cat[i] );
		
		if ( db_CatValArray_get_value ( &Cvarr, Cats->cat[i], &catval ) != DB_OK ) {
		    G_debug ( 3, "No record for cat = %d", Cats->cat[i] );
		    nmissing++;
		    continue ;
	    	}

		if ( catval->isNull ) {
		    G_debug ( 3, "NULL value for cat = %d", Cats->cat[i] );
		    nnull++;
		    continue ;
	    	}

		if ( ctype == DB_C_TYPE_INT ) {
		    val = catval->val.i;
		} else if ( ctype == DB_C_TYPE_DOUBLE ) {
		    val = catval->val.d;
		}

		count++;

		if ( first ) {
		    max = val;
		    min = val;
		    first = 0;
		} else {
		    if ( val > max ) max = val;
		    if ( val < min ) min = val;
		}
		
		if ( compatible ) {
		    if ( type & GV_POINTS ) {
			sum += val;
			sumsq += val*val;
		    } else { /* GV_LINES */
			double l;

			l = Vect_line_length ( Points );
			sum += l*val;
			sumsq += l*val*val;
			total_size += l;
		    }
		}
		G_debug ( 3, "sum = %f total_size = %f", sum, total_size );
	    }
	}
    }

    if ( otype & GV_AREA ) {
	nareas = Vect_get_num_areas ( &Map );
	for ( area = 1; area <= nareas; area++ ) {
	    int i, centr;
		
	    G_debug ( 3, "area = %d", area );
	    
	    centr = Vect_get_area_centroid ( &Map, area );
	    if ( centr < 1 ) continue;
	    
	    G_debug ( 3, "centr = %d", centr );
	    Vect_read_line ( &Map, NULL, Cats, centr );
	    
	    for ( i = 0; i < Cats->n_cats; i++ ) {
		if ( Cats->field[i] == ofield ) {
		    double val;
		    dbCatVal *catval;
	    
		    G_debug ( 3, "cat = %d", Cats->cat[i] );
		    
		    if ( db_CatValArray_get_value ( &Cvarr, Cats->cat[i], &catval ) != DB_OK ) {
			G_debug ( 3, "No record for cat = %d", Cats->cat[i] );
			nmissing++;
			continue ;
		    }

		    if ( catval->isNull ) {
			G_debug ( 3, "NULL value for cat = %d", Cats->cat[i] );
			nnull++;
			continue ;
		    }

		    if ( ctype == DB_C_TYPE_INT ) {
			val = catval->val.i;
		    } else if ( ctype == DB_C_TYPE_DOUBLE ) {
			val = catval->val.d;
		    }

		    count++;

		    if ( first ) {
			max = val;
			min = val;
			first = 0;
		    } else {
			if ( val > max ) max = val;
			if ( val < min ) min = val;
		    }
		    
		    if ( compatible ) {
			double a;

			a = Vect_get_area_area ( &Map, area );
			sum += a*val;
			sumsq += a*val*val;
			total_size += a;
		    }
		    G_debug ( 4, "sum = %f total_size = %f", sum, total_size );
		}
	    }
	}
    }
    
    G_debug ( 2, "sum = %f total_size = %f", sum, total_size );

    if ( compatible ) {
	if ( (otype & GV_LINES) || (otype & GV_AREA) ) {
	    mean = sum / total_size;
	    /* Roger Bivand says it is wrong see GRASS devel list 7/2004 */
	    /*
	    pop_variance = (sumsq - sum*sum/total_size)/total_size;
	    pop_stdev = sqrt(pop_variance);
	    */
	} else {
	    mean = sum / count;
	    pop_variance = (sumsq - sum*sum/count)/count;
	    pop_stdev = sqrt(pop_variance);
	    sample_variance = (sumsq - sum*sum/count)/(count-1);
	    sample_stdev = sqrt(sample_variance);
	}
    }
		    
    if ( shell_flag->answer ) {
    	fprintf(stdout, "n=%d\n", count);
    	fprintf(stdout, "nmissing=%d\n", nmissing);
    	fprintf(stdout, "nnull=%d\n", nnull);
	fprintf(stdout, "min=%g\n", min);
	fprintf(stdout, "max=%g\n", max);
	fprintf(stdout, "range=%g\n", max - min);
	if ( compatible && (otype & GV_POINTS) ) {
	    fprintf(stdout, "mean=%g\n", mean);
	    fprintf(stdout, "population_stddev=%g\n", pop_stdev);
	    fprintf(stdout, "population_variance=%g\n", pop_variance);
	    if ( otype & GV_POINTS ) {
		fprintf(stdout, "sample_stddev=%g\n", sample_stdev);
		fprintf(stdout, "sample_variance=%g\n", sample_variance);
	    }
	}
    } else {
	fprintf(stdout, "number of features with non NULL attribute: %d\n", count);
    	fprintf(stdout, "number of missing attributes: %d\n", nmissing);
    	fprintf(stdout, "number of NULL attributes: %d\n", nnull);
	fprintf(stdout, "minimum: %g\n", min);
	fprintf(stdout, "maximum: %g\n", max);
	fprintf(stdout, "range: %g\n", max - min);
	if ( compatible && (otype & GV_POINTS) ) {
	    fprintf(stdout, "mean: %g\n", mean);
	    fprintf(stdout, "population standard deviation: %g\n", pop_stdev);
	    fprintf(stdout, "population variance: %g\n", pop_variance);
	    if ( otype & GV_POINTS ) {
		fprintf(stdout, "sample standard deviation: %g\n", sample_stdev);
		fprintf(stdout, "sample variance: %g\n", sample_variance);
	    }
	}
    }

    Vect_close ( &Map );

    exit(0) ;
}



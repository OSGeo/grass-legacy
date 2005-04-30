/* Written by Bill Brown, USA-CERL, NCSA, UI GMSL.
 */

#include <stdlib.h>
#include <strings.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "global.h"

int extract_points( int z_flag )
{
    CELL	*cellbuf; 
    FCELL	*fcellbuf; 
    DCELL	*dcellbuf; 
    int		row, col; 
    double 	x, y;
    int         count;

    switch (data_type){
	case  CELL_TYPE:
	    cellbuf = G_allocate_c_raster_buf();
	    break;
	case  FCELL_TYPE:
	    fcellbuf = G_allocate_f_raster_buf();
	    break;
	case  DCELL_TYPE:
	    dcellbuf = G_allocate_d_raster_buf();
	    break;
    }
    
    count = 1;
    for (row = 0; row < cell_head.rows; row++) {
	int cat, val;
	double dval;
	char *lab;

	G_percent(row, cell_head.rows - 1, 2);

	y = G_row_to_northing((double)(row +.5), &cell_head); 

	switch (data_type){
	    case  CELL_TYPE:
		 G_get_c_raster_row(input_fd,cellbuf,row);
		break;
	    case  FCELL_TYPE:
		 G_get_f_raster_row(input_fd,fcellbuf,row);
		break;
	    case  DCELL_TYPE:
		 G_get_d_raster_row(input_fd,dcellbuf,row);
		break;
	}

	for(col=0; col < cell_head.cols; col++){
	    x = G_col_to_easting((double)(col +.5), &cell_head); 

	    switch (data_type){
		case  CELL_TYPE:
		    if (G_is_c_null_value (cellbuf+col)) continue;
		    val = cellbuf[col];
		    dval = val;
		    break;
		case  FCELL_TYPE:
		    if (G_is_f_null_value (fcellbuf+col)) continue;
		    dval = fcellbuf[col];
		    break;
		case  DCELL_TYPE:
		    if (G_is_d_null_value (dcellbuf+col)) continue;
		    dval = dcellbuf[col];
		    break;
	    }

	    if(value_flag) { /* raster value */
		cat = val;  /* value_flag is used only for CELL type */
	    } else {         /* sequence */
		cat = count;
	    }
	    
	    Vect_reset_line(Points);

	    Vect_append_point(Points, x, y, dval);

	    Vect_reset_cats(Cats);
	    Vect_cat_set ( Cats, 1, cat);
	    
	    Vect_write_line(&Map, GV_POINT, Points, Cats);

	    if ( driver != NULL && !value_flag ) {
		char buf[1000];

		sprintf ( buf, "insert into %s values (%d", Fi->table, cat);
		db_set_string ( &sql, buf );

		if ( data_type == CELL_TYPE )
		    sprintf ( buf, ", %d", val );
		else
		    sprintf ( buf, ", %f", dval );
		
		db_append_string ( &sql, buf );


		if ( has_cats ) {
		    lab = G_get_cat(val, &RastCats); /*cats are loaded only for CELL type */

		    db_set_string (&label, lab);
		    db_double_quote_string ( &label );
		    sprintf ( buf, ", '%s'", db_get_string(&label) );
		    db_append_string ( &sql, buf );
		}
		
		db_append_string ( &sql, ")" );

		G_debug ( 3, db_get_string ( &sql ) );

		if (db_execute_immediate (driver, &sql) != DB_OK )
		    G_fatal_error ( "Cannot insert new row: %s", db_get_string ( &sql ) );

	    }
	    
	    count++;
	}
	
    }

    return(1);
}


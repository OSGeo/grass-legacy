#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <tcl.h>
#include <tk.h>
#include "gis.h"
#include "Vect.h"
#include "dbmi.h"
#include "raster.h"
#include "global.h"
#include "proto.h"

/* Interface functions between GUI and C:
*  c_*() functions in C, called from GUI, in this case from Tk
*/

/* Request to cancel running tool */
int
c_cancel ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    G_debug (3, "c_cancel()");
    R_set_cancel ( 1 );
    Tool_next = TOOL_NOTHING;
    return TCL_OK;
}

/* set the next tool to start */
int
c_next_tool ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    char *tl;

    G_debug (3, "c_next_tool()");
    if ( argc < 2 ) {
	G_warning ( "c_next_tool(): inicorrect number of parameters" );
	return TCL_ERROR;
    }
    tl = argv[1];

    if ( strcmp ( tl, "new_point" ) == 0 )
	Tool_next = TOOL_NEW_POINT;
    else if ( strcmp ( tl, "new_line" ) == 0 )
	Tool_next = TOOL_NEW_LINE;
    else if ( strcmp ( tl, "new_boundary" ) == 0 )
	Tool_next = TOOL_NEW_BOUNDARY;
    else if ( strcmp ( tl, "new_centroid" ) == 0 )
	Tool_next = TOOL_NEW_CENTROID;
    else if ( strcmp ( tl, "move_vertex" ) == 0 )
	Tool_next = TOOL_MOVE_VERTEX;
    else if ( strcmp ( tl, "move_line" ) == 0 )
	Tool_next = TOOL_MOVE_LINE;
    else if ( strcmp ( tl, "delete_line" ) == 0 )
	Tool_next = TOOL_DELETE_LINE;
    else if ( strcmp ( tl, "exit" ) == 0 )
	Tool_next = TOOL_EXIT;
    else if ( strcmp ( tl, "zoom_window" ) == 0 )
	Tool_next = TOOL_ZOOM_WINDOW;
    else if ( strcmp ( tl, "zoom_out_centre" ) == 0 )
	Tool_next = TOOL_ZOOM_OUT_CENTRE;
    else if ( strcmp ( tl, "redraw" ) == 0 )
	Tool_next = TOOL_REDRAW;
    else {
	G_warning ( "c_next_tool(): Unknown tool: %s", tl );
	return TCL_ERROR;
    }
	
    G_debug (2, "  Tool_next = %d", Tool_next);
    
    /* Stop running if any */
    R_set_cancel ( 1 );
    
    return TCL_OK;
}

/* set color */
int
c_set_color ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    int code;

    G_debug (2, "c_set_color()");
    if ( argc < 5 ) {
	G_warning ( "c_set_color(): inicorrect number of parameters" );
	return TCL_ERROR;
    }

    code = get_symb_code ( argv[1] );

    if ( code < 0 ) {
	G_warning ( "c_set_color(): Unknown symb name: %s", argv[1] );
	return TCL_ERROR;
    }
	
    G_debug (2, "symb = %d", code);
    G_debug (2, " %s %s %s", argv[2], argv[3], argv[4]);
    
    Symb[code].r = atoi (argv[2]);
    Symb[code].g = atoi (argv[3]);
    Symb[code].b = atoi (argv[4]);
    
    return TCL_OK;
}

/* set layer on/off */
int
c_set_on ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    int code;

    G_debug (2, "c_set_on()");
    if ( argc < 3 ) {
	G_warning ( "c_set_on(): inicorrect number of parameters" );
	return TCL_ERROR;
    }

    code = get_symb_code ( argv[1] );

    if ( code < 0 ) {
	G_warning ( "c_set_on(): Unknown symb name: %s", argv[1] );
	return TCL_ERROR;
    }
	
    G_debug (2, "symb = %d on = %d", code, atoi (argv[2]) );
    
    Symb[code].on = atoi (argv[2]);
    
    return TCL_OK;
}

int
c_tool_centre ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    tool_centre();
    return TCL_OK;
}

/* create table definition in GUI */
int
c_table_definition ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    int col, ncols, sqltype;
    char buf[1000];
    struct field_info *Fi;
    dbString tabname;
    dbDriver *driver;
    dbHandle handle;
    dbTable *table;
    dbColumn *column;
    
    G_debug (2, "c_table_definition()");

    db_init_string (&tabname);

    if ( Vect_get_num_dblinks ( &Map ) > 0 ) {
        Fi = Vect_get_dblink ( &Map, 0 );

	driver = db_start_driver( Fi->driver );
	if (driver == NULL) {
	    G_warning ( "Cannot open driver %s", Fi->driver );
	    return TCL_OK;
	}
	db_init_handle (&handle);
	db_set_handle (&handle, Vect_subst_var(Fi->database,Map.name,G_mapset()), NULL);
	if (db_open_database(driver, &handle) != DB_OK) {
	    G_warning ( "Cannot open database %s", Fi->database );
	    db_shutdown_driver(driver);
	    return TCL_OK;
	}
        db_init_string(&tabname);
        db_set_string(&tabname, Fi->table);
	if(db_describe_table (driver, &tabname, &table) != DB_OK)
	    return TCL_OK;

	ncols = db_get_table_number_of_columns(table);
        for (col = 0; col < ncols; col++) {
            column = db_get_table_column (table, col);
	    sqltype = db_get_column_sqltype(column);
	    sprintf (buf, "add_tab_col \"%s\" \"%s\" %d 0 0 0", db_get_column_name (column),
		              db_sqltype_name(sqltype), db_get_column_length(column) );
            Tcl_Eval ( Toolbox, buf );
	    
	}
    } else {
        Tcl_Eval ( Toolbox, "add_tab_col cat integer 0 1 0 0" );
        Tcl_Eval ( Toolbox, "table_buttons" );
    }

    return TCL_OK;
}

/* create new table */
int
c_create_table ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    int field, ret;
    struct field_info *Fi;
    dbString sql, err;
    dbDriver *driver;
    dbHandle handle;
    
    G_debug (2, "c_create_table() field = %s key = %s cols = %s", argv[1], argv[3], argv[4] );
    
    Tcl_SetVar(Toolbox, "create_table_err", "1", TCL_GLOBAL_ONLY);
    db_init_string (&sql);
    db_init_string (&err);
    field = atoi ( argv[1] );

    Fi = Vect_default_field_info ( Map.name, field, NULL, GV_1TABLE );
    G_debug (2, "driver = %s, database = %s",  Fi->driver, Fi->database );

    driver = db_start_driver( Fi->driver );
    if (driver == NULL) {
	G_warning ( "Cannot open driver %s", Fi->driver );
	db_set_string ( &err, "Cannot open driver " );
	db_append_string ( &err, Fi->driver );
	Tcl_SetVar(Toolbox, "create_table_msg", db_get_string ( &err), TCL_GLOBAL_ONLY);
	return TCL_OK;
    }
    db_init_handle (&handle);
    db_set_handle (&handle, Vect_subst_var(Fi->database,Map.name,G_mapset()), NULL);
    if (db_open_database(driver, &handle) != DB_OK) {
	G_warning ( "Cannot open database %s", Fi->database );
	db_set_string ( &err, "Cannot open database " );
	db_append_string ( &err, Fi->database );
	db_append_string ( &err, " by driver " );
	db_append_string ( &err, Fi->driver );
	db_append_string ( &err, db_get_error_msg() );
        db_shutdown_driver(driver);
	Tcl_SetVar(Toolbox, "create_table_msg", db_get_string ( &err), TCL_GLOBAL_ONLY);
	return TCL_OK;
    }

    db_set_string (&sql, "create table " );
    db_append_string ( &sql, Fi->table);
    db_append_string ( &sql, " ( ");
    db_append_string ( &sql,  argv[4] );
    db_append_string ( &sql, " ) ");
    G_debug ( 2, db_get_string ( &sql ) );
    
    if (db_execute_immediate (driver, &sql) != DB_OK ) {
	G_warning ( "Cannot create table: %s", db_get_string ( &sql )  );
	db_set_string ( &err, "Cannot create table: " );
	db_append_string ( &err, db_get_string ( &sql ) );
	db_append_string ( &err, "\n" );
	db_append_string ( &err, db_get_error_msg() );
        db_close_database(driver);
	db_shutdown_driver(driver);
	Tcl_SetVar(Toolbox, "create_table_msg", db_get_string ( &err), TCL_GLOBAL_ONLY);
	return TCL_OK;
    }
  
    db_close_database(driver);
    db_shutdown_driver(driver);	

    ret = Vect_map_add_dblink ( &Map, field, NULL, Fi->table, argv[3], Fi->database, Fi->driver);
    if ( ret == -1 ) {
	db_set_string ( &err, "Cannot add database link to vector, link for given field probably "
		              "already exists." );
	Tcl_SetVar(Toolbox, "create_table_msg", db_get_string ( &err), TCL_GLOBAL_ONLY);
	return TCL_OK;
    }

    Tcl_SetVar(Toolbox, "create_table_err", "0", TCL_GLOBAL_ONLY);
    
    return TCL_OK;
}


/* set variable */
int
c_var_set ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    int type, code;
    
    G_debug (5, "c_var_set()");

    if ( argc != 3 ) {
	G_warning ( "c_var_set(): inicorrect number of parameters" );
	return TCL_ERROR;
    }

    type = var_get_type_by_name ( argv[1] );
    if ( type == -1 ) return TCL_ERROR;
    code = var_get_code_by_name ( argv[1] );
    
    switch ( type ) {
        case VART_INT:
	    var_seti ( code, atoi(argv[2]) );
            break;
        case VART_DOUBLE:
	    var_setd ( code, atof(argv[2]) );
            break;
        case VART_CHAR:
	    var_setc ( code, argv[2] );
            break;
    }

    return TCL_OK;
}


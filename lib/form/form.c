#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h> 
#include <tcl.h>
#include <tk.h>
#include <locale.h>
#include "gis.h"
#include "dbmi.h"
#include "form.h"

/* Structure to store column names and values */
typedef struct {
    char *name;
    int  ctype;
    char *value;
} COLUMN;

char  *Drvname, *Dbname, *Tblname, *Key;

COLUMN *Cols = NULL; 
int aCols = 0;    /* allocated space */
int nCols = 0;  

int form_open = 0;

/* Close form */
int
close_form ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[] ) 
{
    G_debug ( 3, "close_form()" );
    form_open = 0;
    return TCL_OK;
}

/* Start new sql update */
int
reset_values ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[] ) 
{
    nCols = 0;
    Drvname = NULL;
    Dbname = NULL;
    Tblname = NULL;
    Key = NULL;

    return TCL_OK;
}

int
set_value ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    G_debug ( 2, "set_value(): %s %s", argv[1], argv[2] );
 
    if ( strcmp(argv[1], F_DRIVER_FNAME ) == 0 ) {
	Drvname = G_store ( argv[2] );
    } else if ( strcmp(argv[1], F_DATABASE_FNAME ) == 0 ) {
	Dbname = G_store ( argv[2] );
    } else if ( strcmp(argv[1], F_TABLE_FNAME ) == 0 ) {
	Tblname = G_store ( argv[2] );
    } else if ( strcmp(argv[1], F_KEY_FNAME ) == 0 ) {
	Key = G_store ( argv[2] );
    } else { 
        if ( nCols == aCols ) {
	    aCols += 100;
	    Cols = (COLUMN*) G_realloc ( Cols, (aCols) * sizeof(COLUMN) );
        }
	Cols[nCols].name = G_store ( argv[1] );
	Cols[nCols].value = G_store ( argv[2] );
	nCols++;
    }

    return TCL_OK;
}

/* Update table, use the data previously stored by set_value() */
int
submit ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[])
{
    int i, first, ncols, found, col, sqltype, keyval = 0, ret;
    char buf[2001];
    dbString sql, table_name, strval;
    dbDriver *driver;
    dbHandle handle;
    dbTable  *table;
    dbColumn *column;

    G_debug ( 2, "submit()");
    
    db_init_string (&sql);
    db_init_string(&table_name);
    db_init_string(&strval);
    
    /* Check if all internal values are set */
    if ( Drvname == NULL || Dbname == NULL || Tblname == NULL || Key == NULL ) {
	G_warning ("db connection was not set by form\n");
        sprintf ( buf, "set submit_msg \"db connection was not set by form.\"" );
        Tcl_Eval(interp, buf);
        Tcl_Eval(interp, "set submit_result 0" );
	return TCL_OK; 
    }
    
    /* Get column types */
    G_debug ( 2, "Open driver" );
    driver = db_start_driver(Drvname);
    if (driver == NULL) { 
	G_warning ("Cannot open driver\n"); 
        sprintf ( buf, "set submit_msg \"Cannot open driver '%s'\"", Drvname );
        Tcl_Eval(interp, buf);
        Tcl_Eval(interp, "set submit_result 0" );
	return TCL_OK; 
    }
    G_debug ( 2, "Driver opened" );

    db_init_handle (&handle);
    db_set_handle (&handle, Dbname, NULL);
    G_debug ( 2, "Open database" );
    if (db_open_database(driver, &handle) != DB_OK){
	G_warning ("Cannot open database\n"); 
	db_shutdown_driver(driver); 
	sprintf ( buf, "set submit_msg \"Cannot open database '%s' by driver '%s'\"", Dbname, Drvname);
        Tcl_Eval(interp, buf);
        Tcl_Eval(interp, "set submit_result 0" );
	return TCL_OK;
    }
    G_debug ( 2, "Database opened" );
    
    db_set_string(&table_name, Tblname);
    if(db_describe_table (driver, &table_name, &table) != DB_OK) {
	G_warning ("Cannot describe table\n"); 
	db_shutdown_driver(driver); 
        db_close_database(driver);
	sprintf ( buf, "set submit_msg \"Cannot describe table '%s'\"", Tblname);
        Tcl_Eval(interp, buf);
        Tcl_Eval(interp, "set submit_result 0" );
	return TCL_OK;
    }
    ncols = db_get_table_number_of_columns(table);

    /* For each column get ctype */
    for ( i = 0; i < nCols; i++) {
	found = 0;
	for ( col = 0; col < ncols; col++) {
	    /* get keyval */
	    if ( G_strcasecmp( Cols[i].name, Key ) == 0 ) {
                keyval = atoi ( Cols[i].value );  
	    }
	    column = db_get_table_column (table, col);
	    if ( G_strcasecmp( db_get_column_name(column), Cols[i].name ) == 0 ) { 
		sqltype = db_get_column_sqltype (column);
		Cols[i].ctype = db_sqltype_to_Ctype(sqltype);
		found = 1;
		break;
	    }
	}
	if ( !found && (G_strcasecmp(Cols[i].name, F_ENCODING) != 0)) { 
	    G_warning ( "Cannot find column type" );
	    db_close_database(driver);
	    db_shutdown_driver(driver); 
	    sprintf ( buf, "set submit_msg \"Cannot find column type\"" );
	    Tcl_Eval(interp, buf);
	    Tcl_Eval(interp, "set submit_result 0" );
	    return TCL_OK;
	}
    }
    
    /* Construct update statement */
    sprintf (buf, "update %s set ", Tblname );
    db_set_string (&sql, buf);

    first = 1;
    for ( i = 0; i < nCols; i++ ) {
	if ( G_strcasecmp( Cols[i].name, Key ) == 0  )  continue;
		
	if (G_strcasecmp(Cols[i].name, F_ENCODING) == 0) {

	    G_debug(3, "env is %s, val is %s", G__getenv("GRASS_DB_ENCODING"),
		    Cols[i].value);

	    if (G_strcasecmp(Cols[i].value, G__getenv("GRASS_DB_ENCODING")) ==
		0)
		continue;
	    else {
		G_setenv("GRASS_DB_ENCODING", Cols[i].value);
		if ( Tcl_SetSystemEncoding(interp, Cols[i].value) == TCL_ERROR ) {
			fprintf(stderr, 
				"Could not set Tcl system encoding to %s\n", Cols[i].value);
		}
		db_close_database(driver);
		db_shutdown_driver(driver);
		sprintf(buf,
			"set submit_msg \"View data encoding now is %s\"",
			Cols[i].value);
		Tcl_Eval(interp, buf);
		Tcl_Eval(interp, "set submit_result 0");
		return TCL_OK;
	    }
	}

	if ( !first ) { db_append_string (&sql, ", "); }
	if ( Cols[i].ctype == DB_C_TYPE_INT || Cols[i].ctype == DB_C_TYPE_DOUBLE ) {
            sprintf (buf, "%s = %s", Cols[i].name, Cols[i].value );
	} else {
	    memset(buf, '\0', strlen(buf));
	    ret = Tcl_UtfToExternal(interp,
	    		      Tcl_GetEncoding(interp, G__getenv("GRASS_DB_ENCODING")),
	                      Cols[i].value, strlen(Cols[i].value), 0, NULL,
	                      buf, 2000, NULL, NULL, NULL);
	    
	    if ( ret != TCL_OK ) {
		G_warning ("Could not convert UTF to external.");
		db_set_string ( &strval, Cols[i].value );
	    } else {
	        db_set_string ( &strval, buf );
	    }

	    db_double_quote_string (&strval);
            sprintf (buf, "%s = '%s'", Cols[i].name, db_get_string(&strval) );
	}
        db_append_string (&sql, buf);
	first = 0;
    }

    sprintf (buf, " where %s = %d", Key, keyval );
    db_append_string (&sql, buf);
    
    G_debug ( 2, "SQL: %s", db_get_string(&sql) );

    /* Update table */
    ret = db_execute_immediate (driver, &sql);
	
    db_close_database(driver);
    db_shutdown_driver(driver); 
    
    if ( ret != DB_OK) {
	G_warning ("Cannot update table");
        Tcl_VarEval(interp, "set submit_msg \"Cannot update table:\n", db_get_error_msg(), "\"", NULL );
        Tcl_Eval(interp, "set submit_result 0" );
    } else { 
        Tcl_Eval(interp, "set submit_msg \"Record successfully updated\"" );
        Tcl_Eval(interp, "set submit_result 1" );
    }

    return TCL_OK;
}

/* 
*  Form 
*/
int 
main ( int argc, char *argv[] ) 
{
    int        length;
    int         ret;
    char        buf[5000];
    char        *child_html, *child_title;
    static FILE *child_send, *child_recv;
    static      Tcl_Interp *interp;
    static int  frmid = 0;
    char * encoding_val;

    G_debug ( 2, "Form: main()" );
    
    setlocale(LC_CTYPE, "");
        
    child_recv = stdin;
    child_send = stdout;

    while ( 1 ) {
	ret = read ( fileno(stdin) , &(buf[0]), 1);
	fcntl ( fileno(child_recv), F_SETFL, O_NONBLOCK); /* Don't wait if pipe is empty */
	if ( ret == 0 ) break; /* Pipe was closed by parent -> quit */
	if ( ret == 1 ) {
	    G_debug ( 3, "Form: received = '%c'", buf[0] );
	    if ( buf[0] == 'O' ) { 
		if ( !form_open ) {
	            G_debug ( 3, "Form is not opened" ); 
		    /* Open the window and display the form */
		    interp = Tcl_CreateInterp();
		    if (Tcl_Init(interp) == TCL_ERROR) G_fatal_error ( "Tcl_Init failed: %s\n", 
			                                      interp->result) ;
		    if (Tk_Init(interp) == TCL_ERROR) G_fatal_error ("Tk_Init failed: %s\n", 
			                                      interp->result);

		    Tcl_CreateCommand(interp, "submit", (Tcl_CmdProc*) submit, (ClientData) NULL, 
							(Tcl_CmdDeleteProc*) NULL);
		    Tcl_CreateCommand(interp, "set_value", (Tcl_CmdProc*) set_value, (ClientData) NULL, 
							(Tcl_CmdDeleteProc*) NULL);
		    Tcl_CreateCommand(interp, "reset_values", (Tcl_CmdProc*) reset_values, 
			                             (ClientData) NULL, (Tcl_CmdDeleteProc*) NULL);
		    Tcl_CreateCommand(interp, "close_form", (Tcl_CmdProc*) close_form, 
			                             (ClientData) NULL,  (Tcl_CmdDeleteProc*) NULL);
		    
		    sprintf(buf,"%s/etc/form/form.tcl", G_gisbase());
		    ret = Tcl_EvalFile(interp, buf);
		    if ( ret == TCL_ERROR) {
			if (interp->result != NULL) G_fatal_error ( "Cannot open form: %s\n", interp->result);
			else G_fatal_error ( "Cannot open form\n");
		    }


                    form_open = 1;
		}
		G_debug ( 2, "Open form %d", frmid );
		/* Read title */
		fgets ( buf, 1000, child_recv ); 
		length = atoi ( buf ); /* length of the string */
		G_debug ( 2, "length = %d", length );
		child_title = (char *) G_malloc ( length + 1 ); 
		fread ( child_title, length, 1, child_recv);
		child_title[length] = '\0';

		/* Read html */
		fgets ( buf, 1000, child_recv ); 
		length = atoi ( buf ); /* length of the string */
		G_debug ( 2, "length = %d", length );
		child_html = (char *) G_malloc ( length + 1 ); 
		fread ( child_html, length, 1, child_recv);
		child_html[length] = '\0';
		
		memset(buf, '\0', strlen(buf));
		
		encoding_val =  G__getenv("GRASS_DB_ENCODING");
		Tcl_ExternalToUtf(interp,
		Tcl_GetEncoding(interp, encoding_val),
		child_html, strlen(child_html), 0, NULL,
		buf, strlen(child_html) * 2, NULL, NULL,
			NULL);

		G_debug(3,"Current GRASS_DB_ENCODING: %s", encoding_val);	
	        if ( Tcl_SetSystemEncoding(interp, encoding_val) == TCL_ERROR ) {
	 		fprintf(stderr, 
			"Could not set Tcl system encoding to %s\n", encoding_val);
    		}

		G_debug ( 2, "Form: html = %s", buf );

		/* Insert new page */
		Tcl_SetVar ( interp, "html", buf, 0);
		sprintf (buf, "add_form %d \"%s\"", frmid, child_title );
		Tcl_Eval( interp, buf );
		
		fprintf ( child_send, "O" ); /* OK */
		fflush ( child_send );
		frmid++;
		G_debug ( 2, "Form displayed\n" );
	    } else if ( buf[0] == 'C' ) { /* clear old forms */
		Tcl_Eval(interp, "clear_nb");          
		fprintf ( child_send, "O" ); /* OK */
		fflush ( child_send );
	    } else if ( buf[0] == 'D' ) { /* done! */
		Tcl_Eval(interp, "clear_nb");          
		fprintf ( child_send, "O" ); /* OK */
		fflush ( child_send );
		break;
	    }
	}
	
	Tcl_Eval(interp, "update");
    }

    Tcl_Eval(interp, "destroy .");
    G_debug(3, "Form: end\n");
    exit (0);

    return 0;
}


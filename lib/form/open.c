#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h> 
#include <tcl.h>
#include <tk.h>
#include "gis.h"
#include "dbmi.h"
#include "form.h"

int first_parent = 1;
/* the pipe to send data to GUI */
FILE *parent_send, *parent_recv;

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
    char buf[2000];
    dbString sql, table_name;
    dbDriver *driver;
    dbHandle handle;
    dbTable  *table;
    dbColumn *column;

    G_debug ( 2, "submit()");
    
    db_init_string (&sql);
    db_init_string(&table_name);
    
    /* Check if all internal values are set */
    if ( Drvname == NULL || Dbname == NULL || Tblname == NULL || Key == NULL ) {
	G_warning ("Internal field (db connection) was not set by form\n");
        sprintf ( buf, "set submit_msg \"Internal field (db connection) was not set by form.\"" );
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
	if ( !found ) { 
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
	if ( !first ) { db_append_string (&sql, ", "); }
	if ( Cols[i].ctype == DB_C_TYPE_INT || Cols[i].ctype == DB_C_TYPE_DOUBLE ) {
            sprintf (buf, "%s = %s", Cols[i].name, Cols[i].value );
	} else {
            sprintf (buf, "%s = '%s'", Cols[i].name, Cols[i].value );
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

/* Open new form
*
*  returns: 0 success
*/
int 
F_open ( char *title,  char *html ) 
{
    /* parent */
    int c;
    /* common */
    static int first = 1;
    static int pid;
    static int p1[2], p2[2];
    int        length;
    /* child */
    int         ret;
    static int  first_child = 1;
    char        buf[5000];
    char        *child_html, *child_title;
    static FILE *child_send, *child_recv;
    static      Tcl_Interp *interp;
    static int  frmid = 0; 
    
    G_debug ( 2, "F_open(): title = %s", title);
    
    if ( first ) {
	if ( pipe(p1) < 0 || pipe(p2) < 0 ) G_fatal_error ("Cannot open pipe"); 
        if ((pid = fork()) < 0) G_fatal_error ("Cannot create fork"); 
	first = 0;
    }

    if ( pid == 0 ) { /* Child */
        G_debug ( 2, "CHILD" );
	if ( first_child ) {
	    /* convert pipes to FILE* */
	    child_recv = fdopen (p1[0], "r");
	    close(p1[1]);
	    child_send = fdopen (p2[1], "w");
	    close(p2[0]);
	    
	    /* Open the window and display the form */
	    interp = Tcl_CreateInterp();
	    if (Tcl_Init(interp) == TCL_ERROR) G_fatal_error ( "Tcl_Init failed: %s\n", interp->result) ;
	    if (Tk_Init(interp) == TCL_ERROR) G_fatal_error ("Tk_Init failed: %s\n", interp->result);

	    Tcl_CreateCommand(interp, "submit", (Tcl_CmdProc*) submit, (ClientData) NULL, 
		                                (Tcl_CmdDeleteProc*) NULL);
	    Tcl_CreateCommand(interp, "set_value", (Tcl_CmdProc*) set_value, (ClientData) NULL, 
		                                (Tcl_CmdDeleteProc*) NULL);
	    Tcl_CreateCommand(interp, "reset_values", (Tcl_CmdProc*) reset_values, (ClientData) NULL, 
		                                (Tcl_CmdDeleteProc*) NULL);
	    
	    sprintf(buf,"%s/etc/form/form.tcl", G_gisbase());
	    ret = Tcl_EvalFile(interp, buf);
	    if ( ret == TCL_ERROR) {
		if (interp->result != NULL) G_fatal_error ( "Cannot open form: %s\n", interp->result);
		else G_fatal_error ( "Cannot open form\n");
	    }
	    first_child = 0;
	}
	
	fcntl ( p1[0], F_SETFL, O_NONBLOCK); /* Don't wait if pipe is empty */
	while ( 1 ) {
	    ret = read ( p1[0] , &(buf[0]), 1);
	    if ( ret == 0 ) break; /* Pipe was closed by parent -> quit */
            if ( ret == 1 ) {
	        G_debug ( 3, "CHILD: recieved = '%c'", buf[0] );
                if ( buf[0] == 'O' ) { 
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

                    /* Insert new page */
		    sprintf (buf, "add_form %d \"", frmid );
	            Tcl_VarEval( interp, buf,  child_title, "\" \"",  child_html, "\"", NULL);
		    
		    fprintf ( child_send, "O" ); /* OK */
		    fflush ( child_send );
		    frmid++;
		    G_debug ( 2, "Form displayed\n" );
		} else if ( buf[0] == 'C' ) { /* clear old forms */
                    Tcl_Eval(interp, "clear_nb");          
		    fprintf ( child_send, "O" ); /* OK */
		    fflush ( child_send );
		}
	    }
	    
	    Tcl_Eval(interp, "update");
	}

	Tcl_Eval(interp, "destroy .");
	G_debug(2, "CHILD END\n");
	exit (0);

    } else { /* Parent */
        G_debug ( 2, "PARENT" );

	if ( first_parent ) {
            parent_send = fdopen (p1[1], "w");
	    close(p1[0]);
            parent_recv = fdopen (p2[0], "r");
	    close(p2[1]);
	    first_parent = 0;
	}
		  
	G_debug ( 2, "PARENT HTML:\n%s\n", html );

	fprintf ( parent_send, "O" );
	length = strlen ( title );
	fprintf ( parent_send, "%d\n", length );
	fprintf ( parent_send, "%s", title );
	length = strlen ( html );
	fprintf ( parent_send, "%d\n", length );
	fprintf ( parent_send, "%s", html );
	fflush ( parent_send );
	G_debug ( 2, "PARENT: Request sent\n" );
        
	/* Wait for response */
	c = fgetc ( parent_recv );
	G_debug ( 2, "PARENT: recieved %c\n", c );


    }
    
    return 0;
}

/* Clear old forms from window
*
*/
void
F_clear ( void ) 
{
    char c;

    G_debug ( 2, "F_clear()" );

    if ( first_parent ) return;

    fprintf ( parent_send, "C" );
    fflush ( parent_send );
    c = fgetc ( parent_recv );
    G_debug ( 2, "PARENT: recieved %c\n", c );
}


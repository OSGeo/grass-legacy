#include <config.h>

#ifdef HAVE_TCLTK

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h> 
#include "gis.h"
#include <tcl.h>
#include <tk.h>

int code = -1;
char user[100], password[100];

int set_value  ( ClientData cdata, Tcl_Interp *interp, int argc, char *argv[]) 
{
    
    if ( strcmp(argv[1], "code" ) == 0 )
	code = atoi ( argv[2] ) ;
    else if ( strcmp(argv[1], "user" ) == 0 )
	strcpy ( user, argv[2] ) ;
    else if ( strcmp(argv[1], "password" ) == 0 )
	strcpy ( password, argv[2] ) ;
    
    return TCL_OK;
}

/* Open dialog for user/password for driver/database
*
*  returns:  1 OK
*            0 cancel 
*           -1 error
*/
int 
dbd_user ( char *driver,  char *database, char **usr, char **pwd ) 
{
    Tcl_Interp *interp;
    char buf[1000];
    
    G_debug ( 3, "dbd_user()" );
	    
    user[0] = 0; password[0] = 0;
    
    /* Open the window and display the form */
    interp = Tcl_CreateInterp();
    if (Tcl_Init(interp) == TCL_ERROR) {
       	G_warning ( "Tcl_Init failed: %s\n", interp->result) ;
	return -1;
    }
    if (Tk_Init(interp) == TCL_ERROR) {
	G_warning ("Tk_Init failed: %s\n", interp->result);
	return -1;
    }

    Tcl_CreateCommand(interp, "set_value", (Tcl_CmdProc*) set_value, (ClientData) NULL, 
	                  (Tcl_CmdDeleteProc*) NULL);


    Tcl_Eval(interp, "label .l1 -text \"Enter user name and password for:\"" );          
    sprintf ( buf, "label .l2 -text \"driver: %s\"", driver );
    Tcl_Eval(interp, buf );
    sprintf ( buf, "label .l3 -text \"database: %s\"", database );
    Tcl_Eval(interp, buf );
    Tcl_Eval(interp, "pack .l1 .l2 .l3" );

    Tcl_Eval(interp, "label .luser -text \"user:\"" );
    sprintf ( buf, "set user \"%s\"", *usr );
    Tcl_Eval(interp, buf );
    Tcl_Eval(interp, "entry .euser -textvariable user" );
    Tcl_Eval(interp, "label .lpassword -text \"password:\"" );
    Tcl_Eval(interp, "entry .epassword -textvariable password -show \"*\"" );
    Tcl_Eval(interp, "pack .luser .euser .lpassword .epassword" );

    Tcl_Eval(interp, "button .ok -text \"OK\" -command { set_value user $user;"
	             "set_value password $password; set_value code 1 }" );          
    Tcl_Eval(interp, "button .cancel -text \"Cancel\" -command { set_value code 0 }" );          
    Tcl_Eval(interp, "pack .ok .cancel" );          
    
    while ( code < 0 ) { 
        Tcl_Eval(interp, "update" );          
    }
	    
    Tcl_Eval(interp, "destroy .");
 
    if ( code == 1 ) {
        *usr = G_store ( user );
        *pwd = G_store ( password );
    }
    
    G_debug ( 3, "user =  %s", user );

    return code;
}
#else

/* Open dialog for user/password for driver/database
*
*  returns:  1 OK
*            0 cancel 
*           -1 error
*/
int 
dbd_user ( char *driver,  char *database, char **usr, char **pwd ) 
{
    G_warning ( "Cannot open user/password dialog (compiled without Tcl/Tk)" );
    return -1;
}

#endif

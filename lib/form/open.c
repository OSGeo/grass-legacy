#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h> 
#include <tcl.h>
#include <tk.h>
#include "gis.h"
#include "dbmi.h"
#include "form.h"

int first = 1;
/* the pipe to send data to GUI */
FILE *parent_send, *parent_recv;

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
    static int pid;
    static int p1[2], p2[2];
    int        length;
    /* child */
    char        buf[2000];
    
    G_debug ( 2, "F_open(): title = %s", title);
    
    if ( first ) {
	if ( pipe(p1) < 0 || pipe(p2) < 0 ) G_fatal_error ("Cannot open pipe"); 
        if ((pid = fork()) < 0) G_fatal_error ("Cannot create fork"); 
    }

    if ( pid == 0 ) { /* Child */
        G_debug ( 2, "CHILD" );

        /* Note: If you are forking in a Tk based apllication  you
	 *       must  execl  before  doing any window operations in the
	 *	 child or you will receive an error from the  X server */

	close(p1[1]);
	close(p2[0]);

	close (0);
        close (1);

        if (dup(p1[0]) != 0) G_fatal_error ("Form: cannot dup() input");
        if (dup(p2[1]) != 1) G_fatal_error ("Form: cannot dup() output");
	
	sprintf(buf,"%s/etc/form/form", G_gisbase());

	execl ("/bin/sh", "sh", "-c", buf, 0);
	
	G_debug(2, "CHILD END\n");
	exit (0);

    } else { /* Parent */
        G_debug ( 2, "PARENT" );

	if ( first ) {
            parent_send = fdopen (p1[1], "w");
	    close(p1[0]);
            parent_recv = fdopen (p2[0], "r");
	    close(p2[1]);
	    first = 0;
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

    if ( first ) return;

    fprintf ( parent_send, "C" );
    fflush ( parent_send );
    c = fgetc ( parent_recv );
    G_debug ( 2, "PARENT: recieved %c\n", c );
}


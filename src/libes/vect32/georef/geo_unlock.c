#include <stdlib.h>
#include <stdio.h>
#include "gis.h"
#include "lock.h"
#include "georef.h"

int 
main (int argc, char *argv[])
{

    int   ret ;
    int   pid ;
    char  tty[80] ;
    char  driver_name[80] ;
    char  lock_name[80] ;

    FILE   *fp;

    G_gisinit (argv[0]);

    if (argc != 2)
    {
	fprintf (stderr, "Usage: %s control_file\n", argv[0]);
	exit(-1) ;
    }


/*  control_file contains info about the digitizer, tty, and the control
    points.
*/

    if( (fp = fopen (argv[1], "r"))  == NULL)
    {
	perror (argv[1]);
	exit(-2);
    }


    ret = geo_read_control( fp, driver_name,  tty, lock_name, &pid ) ;
    if ( ret < 0 )
	exit(-2);


    fclose (fp) ;

    ret = unlock_file( lock_name) ;

    exit(0) ;

}


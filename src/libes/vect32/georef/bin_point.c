
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "gis.h"
#include "lock.h"
#include "bin_digit.h"
#include "georef.h"

#define		PROGRAM_NAME	"geo.point"

int 
main (int argc, char *argv[])
{

    int   ret ;
    int   pid ;
    char  tty[80] ;
    char  driver_name[80] ;
    char  lock_name[80] ;
    char  command[500] ;
    void   (*sigint)();
#ifdef SIGQUIT
    void   (*sigquit)();
#endif    

    FILE   *fp, *fopen();

    if(argc != 3)
    {
	fprintf( stderr, "Usage: %s  control_file_name point_file\n", argv[0]) ;
	exit( -1) ;
    }

    G_gisinit(argv[0]) ;

/*  control_file contains info about the digitizer, tty, and the control
    points.
*/

    if( (fp = fopen (argv[1], "r"))  == NULL)
    {
	fprintf( stderr, "Error: geo_point_main cannot open file '%s'\n",
		argv[1]) ;
	sleep(3) ;
	exit(-1);
    }

    ret = geo_read_control( fp, driver_name,  tty, lock_name, &pid) ;
    fclose (fp) ;

/* need at least four points to setup formulas */
    if ( ret < 0  ||  ret < 4)
	exit(-1);

/*  check digitizer  */
    ret = lock_file( lock_name, pid) ;
    if ( ! ret)
    {
	fprintf( stderr, "Digitizer is already being used.\n") ;
	exit(-1) ;
    }

/********  everything is okay, block signals */

	sigint = signal(SIGINT, SIG_IGN) ;
#ifdef SIGQUIT    
	sigquit = signal(SIGQUIT, SIG_IGN) ;
#endif    

/*  NOW execute the geo.point program in etc  */

	sprintf( command, "%s/etc/%s/%s/%s  %s  %s ", G_gisbase(),
		DRIVER_DIR, driver_name, PROGRAM_NAME,
		argv[1], argv[2]) ;


	ret = system(command) ;

    	exit(ret!=0) ;

}


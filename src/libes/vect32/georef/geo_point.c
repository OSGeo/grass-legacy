#include <stdio.h>
#include <unistd.h>
#include "display.h"
#include "lock.h"
#include "gis.h"
#include "Vect.h"
#include "georef.h"

int geo_point_main(char  *control_file,char *point_file )
{

    int   ret ;
    int   pid ;
    char  tty[80] ;
    char  driver_name[80] ;
    char  lock_name[80] ;
    double  x,  y ;

    FILE   *fp;

    G_gisinit("geo_point") ;

/*  control_file contains info about the digitizer, tty, and the control
    points.
*/

    if( (fp = fopen (control_file, "r"))  == NULL)
    {
	fprintf( stderr, "Error: geo_point_main cannot open file '%s'\n",
		control_file) ;
	sleep(3) ;
	close_down(-2);
    }

    ret = geo_read_control( fp, driver_name,  tty, lock_name, &pid) ;
    fclose (fp) ;

/* need at least four points to setup formulas */
    if ( ret < 0  ||  ret < 4)
	close_down(-2);

/*  check digitizer  */
    ret = lock_file( lock_name, pid) ;
    if ( ! ret)
    {
	fprintf( stderr, "Digitizer is already being used.\n") ;
	close_down(-2) ;
    }

/*  opens digitizer
*/
    if ( D_setup_driver(tty) <0)
	close_down(-1);

    Init_curses() ;
    Clear_base() ;
    Clear_info() ;

/*  HELP-  how to handle resetting the origin of the geographics digitizer,
*   the arm has to be in exactly the same position as it was for geo_reg()
*   so that i can use the digitizer control points..
*    D_setup_origin() ;
*/


    if( (fp = fopen (point_file, "w"))  == NULL)
    {
	fprintf( stderr, "Error: geo_point_main cannot open file '%s'\n",
		point_file) ;
	sleep(3) ;
	close_down(-2);
    }


/*  sets up transformation formulas
*/
    if ( geo_reset_transform() <0)
	close_down(-1);

    if ( geo_get_point( &x, &y) < 0 )
	close_down(-1);
    Close_curses() ;

/*  save the point  */
    if ( fprintf( fp, " %f  %f\n", x, y) == EOF)
	exit (-1);

    fclose (fp) ;

    exit(0) ;

}

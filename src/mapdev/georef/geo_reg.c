


#include <stdio.h>
#include <signal.h>
#include "gis.h"
#include "dig_head.h"
#include "digit.h"

geo_reg_main( driver_name, tty, control_file, lock_name, pid_string)
	char  *driver_name,  *tty,  *control_file, *lock_name, *pid_string ;
{
    int    pid ;
    FILE   *fp, *fopen();
    int    close_down();


/* set up permissions and for higher nice value */
    init_priority ();

/*  in case of floating point error  */
    signal(SIGFPE, close_down) ;

/* Couldn't call G_gisinit () because of UID stuff */
    G_no_gisinit ("GEO_REG");

    Init_curses() ;

/*  opens digitizer, gets control coordinates, registers control coordinates
*/

    if (D_setup_driver(tty) <0)
	close_down(-1) ;

    D_setup_origin() ;
    if ( reset_map() <0)
	    close_down(-1) ;

    Close_curses() ;

/*  coor_file contains about the digitizer, tty, and the control points  */

    if( (fp = fopen (control_file, "w"))  == NULL)
    {
	fprintf( stderr, "Error: geo_reg_main cannot open file '%s'\n",
		control_file) ;
	sleep(3) ;
	close_down(-2);
    }

    pid = atoi(pid_string) ;

    if ( geo_save_control( fp, driver_name,  tty, lock_name, pid ) < 0)
	close_down(-2);

    fclose (fp) ;

    exit(0) ;

}


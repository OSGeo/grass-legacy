#include <stdlib.h>
#include <unistd.h>
#include "globals.h"

static int setup();
static int oops();

int setup_digitizer()
{
    int setup();
    char command[1024];


    use_digitizer = 0;

/* Disable use of the digitizer until geo.quest, geo.reg, and geo.point 
 * make use of the 4.1 digitier interface 
 */
   return (0);



    /*
     * test to see if we have a digitizer (geo.quest)
     * make sure this program has execute permission first.
     * then run the program and check its exit status
     *  0 means can use digitizer, other means can't
     */
    sprintf (command, "%s/etc/geo.quest", G_gisbase());
    if (access (command, 1) != 0)
      return 0;
    if (system(command))
      return 0;


    /*
     * ask the user if he/she wishes to use it
     */
    if (!G_yes ("Do you want to use a digitizer to mark points?", -1)) {
      use_digitizer = 0;
      return 0;
    }
    else {
      use_digitizer = 1;
      setup ();
    }
}


static int setup()
{
    char command[1024];

    /*
     * setup the digitizer. system() call must exit with 0 to indicate
     * everything went fine
     */
    sprintf (command, "%s/etc/geo.reg %s %d",
	     G_gisbase(), digit_points, getpid());
    
    if (system (command))
      {
	use_digitizer = 0;
	sleep(3);
    }

    return 0;
}

int digitizer_point (double *east,double *north)
{
    char command[1024];
    FILE *fd;
    int stat;

    /* make sure digitzer is to be used */
    if (!use_digitizer)
      return 0;
    
    sprintf (command, "%s/etc/geo.point %s %s",
	     G_gisbase(), digit_points, digit_results);
    
    
    if(system (command))
      {
	sleep(3);
	oops();
	return 0;
    }

    fd = fopen (digit_results, "r");
    if (fd == NULL)
    {
	oops();
	return 0;
    }
    stat = (fscanf (fd, "%lf %lf", east, north) == 2);
    fclose (fd);

    if (stat == 0)
	oops();
    return stat;
}

static int oops()
{
  
    /** Curses_clear_window (MENU_WINDOW); **/
    /** Curses_write_window (MENU_WINDOW, 3,2,"Can't get data from digitizer"); **/

    return 0;
}


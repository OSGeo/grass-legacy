#include "globals.h"
setup_digitizer()
{
    static int use = 1;
    int setup(), no(), yes();
    static Objects objects[]=
    {
	INFO("Do you wish to use the digitizer?  ", &use),
	MENU("YES", yes, &use),
	MENU("NO", no, &use),
	{0}
    };
    char command[1024];

    use_digitizer = 0;
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
    Start_mouse_in_menu();
    Input_pointer (objects);
    if (use_digitizer)
	Input_other (setup, "Keyboard");
}

static
setup()
{
    char command[1024];
/*
 * setup the digitizer. system() call must exit with 0 to indicate
 * everything went fine
 */
    sprintf (command, "%s/etc/geo.reg %s %d",
	G_gisbase(), digit_points, getpid());
    Suspend_curses();
    if (system (command))
    {
	use_digitizer = 0;
	sleep(3);
    }
    Resume_curses();
}

digitizer_point (east, north)
    double *east, *north;
{
    char command[1024];
    FILE *fd;
    int stat;

/* make sure digitzer is to be used */
    if (!use_digitizer)
	return 0;

    sprintf (command, "%s/etc/geo.point %s %s",
	G_gisbase(), digit_points, tempfile1);

    Suspend_curses();
    if(system (command))
    {
	sleep(3);
	Resume_curses();
	oops();
	return 0;
    }
    Resume_curses();
    fd = fopen (tempfile1, "r");
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

static
oops()
{
    Curses_clear_window (MENU_WINDOW);
    Curses_write_window (MENU_WINDOW, 3,2,"Can't get data from digitizer");
}

static
no()
{
    use_digitizer = 0;
    return 1;
}

static
yes()
{
    use_digitizer = 1;
    return 1;
}

#include "globals.h"


zoom()
{
    static int use = 1;
    int cancel();
    int zoom_box();
    int zoom_point();


    static Objects objects[]=
    {
	MENU("CANCEL",cancel,&use),
	MENU("BOX", zoom_box, &use),
	MENU("POINT", zoom_point, &use),
	INFO("Select type of zoom",&use),
	{0}
    };

    Input_pointer (objects);
    return 0;	/* return, but don't QUIT */
}

static
cancel()
{
    return -1;
}

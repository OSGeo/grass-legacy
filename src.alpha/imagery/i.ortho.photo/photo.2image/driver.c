#include "globals.h"

static int use = 1;

driver()
{
    int really_quit();
    int zoom();
    int mark();
    int analyze();
    int plotcell();
    int drawcam();

    static Objects objects[] =
    {
	MENU("QUIT",really_quit,&use),
	MENU("ZOOM",zoom,&use),
	MENU("PLOT CELL",plotcell,&use),
        MENU("CAMERA",drawcam, &use),
	MENU("ANALYZE",analyze,&use),
	INFO("  Input method -> ", &use),
	OPTION("KEYBOARD",2,&from_keyboard),
	OPTION("CAMERA FILE",2,&from_screen),
	OTHER(mark, &use),
	{0}
    };

    Input_pointer (objects);
    Menu_msg ("");
}


static
really_quit()
{
    int stop(), dont_stop();
    static Objects objects[] =
    {
	INFO("really quit? ",&use),
	MENU("NO",dont_stop,&use),
	MENU("YES",stop,&use),
	{0}
    };
    if (Input_pointer (objects) < 0)
	return -1;
    return 0; /* don't quit */
}
static dont_stop()
{
    return 1;
}
static stop()
{
    return -1;
}








#include "globals.h"

static int use = 1;

driver()

{
  int really_quit();
  int analyze_sig();
  int define_region();
  int redisplay();
  int zoom_box();

  static Objects objects[] =
    {
      INFO("Command Menu:",&use),
      MENU(" Zoom ",zoom_box,&use),
      MENU(" Define region ",define_region,&use),
      MENU(" Redisplay map ", redisplay, &use),
      MENU(" Analyze region ", analyze_sig, &use),
      MENU(" Quit ",really_quit,&use),
      {0}
    };

  Input_pointer (objects);
  Menu_msg("");
}


static
really_quit()
{
    int stop(), dont_stop();
    static Objects objects[] =
    {
        INFO("really quit? ",&use),
        MENU(" No ",dont_stop,&use),
        MENU(" Yes ",stop,&use),
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



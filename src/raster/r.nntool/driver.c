#include "globals.h"

static int use = 1;

driver()
{
  int really_quit(), define_region(), redisplay(), zoom_box();
  int lump(), sample(), class(), configure(), linear();

  static Objects objects[] =
    {
      INFO("Menu:",&use),
      MENU(" Lump ", lump,&use),
      MENU(" Zoom ", zoom_box,&use),
      MENU(" Define reg ", define_region,&use),
      MENU(" Redisplay ", redisplay, &use),
      MENU(" Sample ", sample, &use),
      MENU(" Classes ", class, &use),
      MENU(" Configure ", configure,&use),
      MENU(" Linear ", linear,&use),    /* see nntool.c for code */
      MENU(" Quit ", really_quit,&use),
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
/* extern char group[10];
   
    if(group[0] == 'Y' || group[0] == 'y')
       write_signatures();

    End_curses();
    R_close_driver(); */
    return -1;
}

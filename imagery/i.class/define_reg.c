#include "globals.h"
#include "local_proto.h"

static int use = 1;
static int done(void);

int 
define_region (void)
{
  extern int really_quit();
  extern int draw_region();
  extern int complete_region();
  extern int restore_region();
  extern int erase_region();

  static Objects objects[] =
    {
      INFO("Region Menu:",&use),
      MENU(" Erase region ",erase_region,&use),
      MENU(" Draw region ",draw_region,&use),
      MENU(" Restore last region ",restore_region,&use),
      MENU(" Complete region ",complete_region,&use),
      MENU(" Done ",done,&use),
      {0}
    };
  
  Input_pointer (objects);
  return(0);
}

static int 
done (void)
{
  return(-1);
}

#include "global.h"
static int which_env = -1; /* 0 = cur, 1 = target */

select_current_env()
{
    if (which_env < 0)
    {
	G__create_alt_env();
	which_env = 0;
    }
    if (which_env != 0)
    {
	G__switch_env();
	which_env = 0;
    }
}

select_target_env()
{
    if (which_env < 0)
    {
	G__create_alt_env();
	which_env = 1;
    }
    if (which_env != 1)
    {
	G__switch_env();
	which_env = 1;
    }
}

show_env()
{
printf ("env(%d) switch to LOCATION %s, MAPSET %s\n", which_env,
    G__getenv("LOCATION_NAME")==NULL ? "?" : G__getenv("LOCATION_NAME"),
    G__getenv("MAPSET")==NULL ? "?" : G__getenv("MAPSET"));
sleep(2);
}



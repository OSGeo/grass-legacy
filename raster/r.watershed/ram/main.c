#define MAIN
#include <stdlib.h>
#include <unistd.h>
#include "Gwater.h"
#include <grass/gis.h>
#include <grass/glocale.h>
#undef MAIN


int main (int argc, char *argv[])
{
    init_vars (argc,argv);
    do_astar ();
    do_cum ();
    if (sg_flag || ls_flag)
    {
	sg_factor();
    }
    if (bas_thres <= 0)
    {
    	G_message(_("\n\nSECTION %d: Closing Maps."), tot_parts);
        close_maps ();
    }
    else    
    {
        if (arm_flag)
	{
	    fp = fopen (arm_name, "w");
	}
	bas = (CELL *)G_calloc(sizeof(CELL), size_array(&bas_seg,nrows,ncols));
	if (!bas) {
		G_fatal_error (_("not enough memory to run program (at bas)"));
	}
	haf = (CELL *)G_calloc(sizeof(CELL), size_array(&haf_seg,nrows,ncols));
	if (!haf) {
		G_fatal_error (_("not enough memory to run program (at haf)"));
	}

	G_message(_("\nSECTION %d: Watershed determination."), tot_parts - 1);
	find_pourpts ();
    	G_message(_("\nSECTION %d: Closing Maps."), tot_parts);
	close_array_seg ();
    }

    exit (EXIT_SUCCESS);
}

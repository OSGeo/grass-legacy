#define MAIN
#include <stdlib.h>
#include <unistd.h>
#include "Gwater.h"
#include <grass/gis.h>
#include <grass/glocale.h>
#undef MAIN


int 
main (int argc, char *argv[])
{
    extern FILE *fopen();

    one = 1;
    zero = 0;
    dzero = 0.0;
    init_vars (argc,argv);
    do_astar ();
    do_cum ();
    if (sg_flag || ls_flag)
    {
	sg_factor();
    }

    if (bas_thres <= 0)
    {
    	G_message(_("\nSECTION %d beginning: Closing Maps."), tot_parts);
        close_maps ();
    }
    else    
    {
        if (arm_flag)
	{
	    fp = fopen (arm_name, "w");
	}
	cseg_open (&bas, SROW, SCOL, 4);
	cseg_open (&haf, SROW, SCOL, 4);
    	G_message(_("\nSECTION %d beginning: Watershed determination."), 
			tot_parts - 1);
	find_pourpts ();
    	G_message(_("\nSECTION %d beginning: Closing Maps."), tot_parts);
	close_array_seg ();
    }

    exit (EXIT_SUCCESS);
}

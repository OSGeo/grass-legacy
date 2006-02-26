#define MAIN
#include <stdlib.h>
#include <unistd.h>
#include "Gwater.h"
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
    	fprintf (stdout,"\n\nSECTION %d: Closing Maps.\n", tot_parts);
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
		G_fatal_error ("not enough memory to run program (at bas)");
		exit(1);
	}
	haf = (CELL *)G_calloc(sizeof(CELL), size_array(&haf_seg,nrows,ncols));
	if (!haf) {
		G_fatal_error ("not enough memory to run program (at haf)");
		exit(1);
	}
	fprintf (stdout,"\nSECTION %d: Watershed determination.\n", tot_parts - 1);
	find_pourpts ();
    	fprintf (stdout,"\nSECTION %d: Closing Maps.\n", tot_parts);
	close_array_seg ();
    }
    exit (0);
}

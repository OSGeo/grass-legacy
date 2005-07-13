#define MAIN
#include <stdlib.h>
#include <unistd.h>
#include "Gwater.h"
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
    	fprintf (stdout,"\nSECTION %d beginning: Closing Maps.\n", tot_parts);
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
    	fprintf (stdout,"\nSECTION %d beginning: Watershed determination.\n", 
			tot_parts - 1);
	find_pourpts ();
    	fprintf (stdout,"\nSECTION %d beginning: Closing Maps.\n", tot_parts);
	close_array_seg ();
    }
    exit (0);
}

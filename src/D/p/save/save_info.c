/* @(#)save_info.c	2.1   6/26/87 */

#include <stdio.h>

save_info(outfile)
	FILE *outfile ;
{
	char **list;
	int count;
	int i;
	int stat ;
	char window_name[64] ;

	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

/* Get cell name */
	stat = R_pad_get_item ("cell", &list, &count);
	if (! stat)
	{
		fprintf(outfile,"Dcell %s\n", list[0]) ;
		R_pad_freelist (list,count);
	}

/* Get rest of list */
	stat = R_pad_get_item ("list", &list, &count);
	if (! stat)
	{
		for(i=0; i<count; i++)
			fprintf(outfile,"%s\n", list[i]) ;
		R_pad_freelist (list,count);
	}

	R_close_driver();
}

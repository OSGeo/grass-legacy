
#include "dlg.h"
#include "dlg_conv.h"
#include <stdio.h>

#define SOLID				0
#define DOTTED				1

dlg_plot_all_lines(fd, dlg)
	FILE *fd ;
	struct dlg *dlg ;
{
	int i ;
	int stat ;
	double D_get_u_to_d_xconv() ;
	double D_get_u_to_d_yconv() ;
	double D_get_u_west() ;
	double D_get_u_east() ;
	double D_get_u_north() ;
	double D_get_u_south() ;
	double D_get_d_west() ;
	double D_get_d_east() ;
	double D_get_d_north() ;
	double D_get_d_south() ;

	dlg_D_west  = D_get_d_west() ;
	dlg_D_east  = D_get_d_east() ;
	dlg_D_north = D_get_d_north() ;
	dlg_D_south = D_get_d_south() ;
	dlg_U_west  = D_get_u_west() ;
	dlg_U_east  = D_get_u_east() ;
	dlg_U_north = D_get_u_north() ;
	dlg_U_south = D_get_u_south() ;
	dlg_U_to_D_xconv = D_get_u_to_d_xconv() ;
	dlg_U_to_D_yconv = D_get_u_to_d_yconv() ;

	for (i=1; i<=dlg->max_lines; i++)
	{
		if ((stat = dlg_read_line(fd, dlg, i)) < 0)
		{
			fprintf (stderr, "ERROR: Cannot read dlg line: %d\n", i) ;
			exit(-1) ;
		}

		if (stat == 1)
			continue ;

		if (dlg->line.N < dlg_U_south)
			continue ;
		if (dlg->line.S > dlg_U_north)
			continue ;
		if (dlg->line.E < dlg_U_west)
			continue ;
		if (dlg->line.W > dlg_U_east)
			continue ;
		dlg_plot_all_coors(dlg->line.n_coors, dlg->line.coors) ;
	}
}

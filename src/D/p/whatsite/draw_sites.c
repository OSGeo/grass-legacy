#include <stdio.h>
#define MARK_SIZE	3     /* pixles */
#define XFUDGE		1.

#define XADJ(x)    (int)((x - dlg_U_west ) * dlg_U_to_D_xconv + dlg_D_west + XFUDGE)
#define YADJ(y)		(int)((dlg_U_south - y) * dlg_U_to_D_yconv + dlg_D_south)

#include "sites.h"

static double current_x_utm ;
static double current_y_utm ;
static on_a_roll ;
static double dlg_D_west ;
static double dlg_D_east ;
static double dlg_D_north ;
static double dlg_D_south ;
static double dlg_U_west ;
static double dlg_U_east ;
static double dlg_U_north ;
static double dlg_U_south ;
static double dlg_U_to_D_xconv ;
static double dlg_U_to_D_yconv ;

draw_all_sites(mapset, mapname)
	char *mapset, *mapname ;
{
	char db[128] ;
	int table ;
	char *record[128] ;
	double utm_n, utm_e ;
	long get_table_entry() ;
	long offset ;
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

/* Set conversion factors */
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
	mark_size = (double)MARK_SIZE / dlg_U_to_D_xconv ;

/* Open data base */
	sprintf(db, "%s/%s/%s/%s", 
		G_getenv("GISDBASE"),
		G_getenv("LOCATION_NAME"),
		mapset,
		"site") ;

	if ((table = open_table(db, mapname, 0)) < 0)
	{
		printf("db: %s  table: %s\n", db, mapname) ;
		sprintf(db,"%d error in opening table\n", table) ;
		G_fatal_error(db) ;
	}

/* Read through data base saving row/col info and plotting points */
	nsites = 0 ;
	while(-1 != (offset = get_table_entry(table, record, 0L)))
	{
		sscanf(record[1],"%lf",&sites[nsites].e) ;
		sscanf(record[2],"%lf",&sites[nsites].n) ;
		sites[nsites].offset = offset ;
		mark_site(sites[nsites].n, sites[nsites].e) ;
		nsites++ ;
	}

	close_table(table) ;
}

mark_site(utm_n, utm_e)
	double utm_n, utm_e ;
{
	dlg_First(utm_e, utm_n-mark_size) ;
	dlg_Next (utm_e, utm_n+mark_size) ;
	dlg_First(utm_e-mark_size, utm_n) ;
	dlg_Next (utm_e+mark_size, utm_n) ;
}

dlg_First(x, y)
	double x, y ;
{
	current_x_utm = x ;
	current_y_utm = y ;
	on_a_roll = 0 ;
}

dlg_Next(x, y)
	double x, y ;
{
	int off_a_roll ;
	int xpos, ypos ;
	double new_x, new_y ;

/* check to see if entire line segment is outside window */

	if ((x < dlg_U_west) && (current_x_utm < dlg_U_west))
		off_a_roll = 1 ;
	else if ((x > dlg_U_east) && (current_x_utm > dlg_U_east))
		off_a_roll = 1 ;
	else if ((y < dlg_U_south) && (current_y_utm < dlg_U_south))
		off_a_roll = 1 ;
	else if ((y > dlg_U_north) && (current_y_utm > dlg_U_north))
		off_a_roll = 1 ;
	else
		off_a_roll = 0 ;


	if (off_a_roll)
	{
		on_a_roll = 0 ;
	}
	else
	{
		new_x = x ;
		new_y = y ;
		D_clip( dlg_U_south, dlg_U_north, dlg_U_west, dlg_U_east, 
			&new_x, &new_y, &current_x_utm, &current_y_utm) ;
			
		if (on_a_roll)
		{
			xpos = XADJ(new_x) ;
			ypos = YADJ(new_y) ;
			R_cont_abs(xpos, ypos) ;
		}
		else
		{
			xpos = XADJ(current_x_utm) ;
			ypos = YADJ(current_y_utm) ;
			R_move_abs(xpos, ypos) ;
			xpos = XADJ(new_x) ;
			ypos = YADJ(new_y) ;
			R_cont_abs(xpos, ypos) ;
			on_a_roll = 1 ;
		}
	}
	current_x_utm = x ;
	current_y_utm = y ;

	return ;
}

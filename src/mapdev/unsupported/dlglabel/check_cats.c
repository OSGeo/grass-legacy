/*  @(#)check_cats.c	2.1  6/26/87  */
#include <stdio.h>
#include "dlg.h"
#include "dlghead.h"

check_cats(num)
	int num ;
{
	int n_nodes ;
	int n_areas ;
	int n_lines ;
	int i ;

	n_nodes = 0 ;
	n_areas = 0 ;
	n_lines = 0 ;

/* Count nodes */
	for(i=1; i<=tot_nodes; i++)
	{
		if ( (node[i].x == 0.0) || (node[i].y == 0.0) ) 
			continue ;
		n_nodes++ ;
	}
	
/* Count areas */
	for(i=1; i<=tot_areas; i++)
	{
		if ( (area[i].x == 0.0) || (area[i].y == 0.0) ) 
			continue ;
		n_areas++ ;
	}
	
/* Count lines */
	for(i=1; i<=tot_lines; i++)
	{
		if ( (line[i].start_node == 0) || (line[i].end_node == 0) ) 
			continue ;
		n_lines++ ;
	}
	
	dlg_head.num_cats = 1 ;
	dlg_cats[0].form_code = 0 ;
	dlg_cats[0].num_nodes = n_nodes ;
	dlg_cats[0].act_nodes = n_nodes ;
	dlg_cats[0].nta_link = 0 ;
	dlg_cats[0].ntl_link = 1 ;
	dlg_cats[0].num_areas = n_areas ;
	dlg_cats[0].act_areas = n_areas ;
	dlg_cats[0].atn_link = 0 ;
	dlg_cats[0].atl_link = 1 ;
	dlg_cats[0].area_list = 0 ;
	dlg_cats[0].num_lines = n_lines ;
	dlg_cats[0].act_lines = n_lines ;
	dlg_cats[0].line_list = 1 ;
}

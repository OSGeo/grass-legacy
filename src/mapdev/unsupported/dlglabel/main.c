/*  @(#)dlgedit.c	1.1  5/4/87  */
#include "dlg.h"
#include "dlghead.h"
#include "convert.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	FILE *fopen() ;
	char buff[128] ;
	int status ;
	int i ;
	char *name            ;
	char buffer[256]      ;

	setbuf(stdout, 0) ;

/* Show advertising */
	G_gisinit("dlglabel");
	clear_screen() ;
	printf("\n\n   DLG LABEL FACILITY\n") ;

/* Check for prober number of arguments */
	if (argc != 4)
	{
		printf("Usage: dlgedit dlg_file new_dlg_file tmp_file\n") ;
		exit(-1) ;
	}

/* Set up graphics */
	R_open_driver() ;

/* Check for and read dlg header info */

	if (! (dlg = fopen(argv[1], "r")) )
	{
		printf("  PROBLEM: Can't open dlg file\n") ;
		exit(-1) ;
	}
	if (! (dlg_new = fopen(argv[2], "w")) )
	{
		printf("  PROBLEM: Can't open new dlg file: %s\n", argv[2]) ;
		exit(-1) ;
	}
	if (! (dlg_tmp = fopen(argv[3], "w")) )
	{
		printf("  PROBLEM: Can't open tmp file: %s\n", argv[3]) ;
		exit(-1) ;
	}
	status = read_dlg_header() ;
	if (status == -1)
	{
		printf("Problem in initial read of dlg file\n") ;
		exit(-1) ;
	}
	printf("\nSelected information from dlg header\n") ;
	printf(" Banner:      %s\n", dlg_head.banner) ;
	printf(" Cart. Unit:  %s\n", dlg_head.cart_unit) ;
	printf(" Source Date: %s\n", dlg_head.source_date) ;
	printf(" Orig. Scale: %s\n", dlg_head.orig_scale) ;

/* Key all coordinate G_limits off dlg header coordinates */
	U_west  = dlg_coors.utm_e[0] ;
	U_east  = dlg_coors.utm_e[0] ;
	U_south = dlg_coors.utm_n[0] ;
	U_north = dlg_coors.utm_n[0] ;
	for(i=1; i<4; i++)
	{
		if (U_west  > dlg_coors.utm_e[i]) U_west  = dlg_coors.utm_e[i] ;
		if (U_east  < dlg_coors.utm_e[i]) U_east  = dlg_coors.utm_e[i] ;
		if (U_south > dlg_coors.utm_n[i]) U_south = dlg_coors.utm_n[i] ;
		if (U_north < dlg_coors.utm_n[i]) U_north = dlg_coors.utm_n[i] ;
	}

/* Initialize conversions variables  */
	init_conversions() ;

/* Establish conversion coefficients for the graphics */
	do_conversions() ;

/* Prepare the screen now to give user something entertaining */
	outline_map() ;

/* Do initial read of dlg file */
	printf("\nMaking initial read of dlg file... Please wait\n") ;
	status = read_dlg() ;
	printf("This file currently contains:\n") ;
	printf("  %d nodes, %d areas, and %d lines\n", 
		orig_nodes, orig_areas, orig_lines) ;
	if (status == -1)
	{
		printf("Problem reading body of dlg file\n") ;
		exit(-1) ;
	}
	tot_nodes = orig_nodes ;
	tot_areas = orig_areas ;
	tot_lines = orig_lines ;


/*  determine if there is a universe defined in this map. */
	find_univ_info() ;

/* Establish bounding boxes for Areas */
	b_box_areas() ;

	printf("\nHit <RETURN> to continue\n") ;
	gets(buffer) ;

	main_menu() ;

	close(dlg_new) ;
	close(dlg_tmp) ;
	close(dlg) ;
	R_close_driver() ;
}

/*  @(#)main.c	2.3  10/21/87  */
#include "structures.h"
#include "dlg.h"
#include <stdio.h>

main(argc, argv)
	int argc ;
	char **argv ;
{
	FILE *fopen(), *f_digit, *f_dlg ;
	int i ;
	int step ;
	double thresh ;
	double atof() ;
	char message[128] ;

	setbuf(stderr, 0) ;
	step = 0 ;

	if (argc != 3)
	{
		printf("Usage: digtodlg digit_file dlg_file\n") ;
		exit(-1) ;
	}

	G_gisinit ("digtodlg");
	Init_curses() ;

/* open necessary files */
	sprintf(message, "STEP %d: Open all necessary files", ++step) ;
	Write_info(1, message) ;

	if (! (f_digit = fopen(argv[1], "r+")) )
	{
		sprintf(message, "  PROBLEM: Can't open digit file: %s", argv[1]) ;
		Write_info(3, message) ;
		sleep(2) ;
		goto shutdown ;
	}
	if (! (f_dlg = fopen(argv[2], "w")) )
	{
		sprintf(message, "  PROBLEM: Can't open dlg for writing: %s", argv[2]) ;
		Write_info(3, message) ;
		sleep(2) ;
		goto shutdown ;
	}

/* Read digit header information */
	sleep(1) ;
	Clear_info() ;
	sprintf(message,"STEP %d: Read digit header", ++step) ;
	Write_info(1, message) ;

	if (! read_digit_head(f_digit, &thresh))
	{
		Close_curses() ;
		exit(0) ;
	}

	if (! quit())
	{
		Close_curses() ;
		exit(0) ;
	}

	init_graphics() ;

	Clear_base() ;

	if (do_graphics())
	{
		save_first_window(
			dlg_coors[NE].utm_n,
			dlg_coors[SE].utm_n,
			dlg_coors[SE].utm_e,
			dlg_coors[SW].utm_e) ;
		window_conversions(
			dlg_coors[NE].utm_n,
			dlg_coors[SE].utm_n,
			dlg_coors[SE].utm_e,
			dlg_coors[SW].utm_e) ;

		R_standard_color( D_translate_color("black") ) ;
		D_erase_window() ;
		outline_window() ;
	}


/* Read in data, accumulating endpoints */
	Clear_info() ;
	sprintf(message, "STEP %d: Read digit file, storing offset and endpoint info", ++step) ;
	Clear_info() ;
	Write_info(1, message) ;

	read_digit(f_digit, thresh) ;

	sprintf(message,"Digit file contents: %d lines", n_lines) ;
	Write_info(3, message) ;
	sleep(2) ;

/* Identify nodes in the data */
	Clear_info() ;
	sprintf(message, "STEP %d: Identify nodes in endpoints", ++step) ;
	Write_info(1, message) ;
	if ( ! find_nodes(thresh))
		goto shutdown ;

	sprintf(message," %d endpoints identified as nodes", n_nodes) ;
	Write_info(3, message) ;
	sleep(2) ;
	Write_info(3, "") ;

/*	printf("Information about Areas") ;
	for(i=1; i<=n_lines; i++)
	{
		printf("Area %d: b_ep:%d e_ep:%d %.2lf,%.2lf  %.2lf,%.2lf",
			i, 
			lines[i].endpoint_beg,
			lines[i].endpoint_end,
			endpoints[lines[i].endpoint_beg].x,
			endpoints[lines[i].endpoint_beg].y,
			endpoints[lines[i].endpoint_end].x,
			endpoints[lines[i].endpoint_end].y ) ;
	}
*/

	Clear_info() ;
	sprintf(message,"STEP %d: Group lines by node.", ++step) ;
	Write_info(1, message) ;
	if (group_nodes() < 0)
		goto shutdown ;

	Clear_info() ;
	sprintf(message,"STEP %d: Sort the lines around nodes by angle.", ++step) ;
	Write_info(1, message) ;
	sort_lines_on_nodes() ;

	Clear_info() ;
	sprintf(message,"STEP %d: Process area info.", ++step) ;
	Write_info(1, message) ;
	if ( do_areas(f_digit, thresh))
	{
		Close_curses() ;
		printf("\n\n Now quitting %s\n", argv[0]) ;
		unlink(argv[2]) ;
		exit(0) ;
	}

	Clear_info() ;
	sprintf(message,"STEP %d: Attach islands.", ++step) ;
	Write_info(1, message) ;
	do_islands(f_digit) ;

	Clear_info() ;
	sprintf(message,"STEP %d: Define universe.", ++step) ;
	Write_info(1, message) ;
	define_universe (f_digit) ;

	/* Set remaining category parameters and write out */
	Clear_info() ;
	sprintf(message,"STEP %d: Write header out in DLG format.", ++step) ;
	Write_info(1, message) ;
	dlg_head.num_cats = 1 ;
	dlg_cats.form_code = 0 ;
	dlg_cats.num_nodes = n_nodes ;
	dlg_cats.act_nodes = n_nodes ;
	dlg_cats.nta_link = 0 ;
	dlg_cats.ntl_link = 1 ;
	dlg_cats.num_areas = n_areas ;
	dlg_cats.act_areas = n_areas ;
	dlg_cats.atn_link = 0 ;
	dlg_cats.atl_link = 1 ;
	dlg_cats.area_list = 0 ;
	dlg_cats.num_lines = n_lines ;
	dlg_cats.act_lines = dlg_cats.num_lines ;
	dlg_cats.line_list = 1 ;

	write_bdlg_head(f_dlg) ;

	Clear_info() ;
	sprintf(message,"STEP %d: Write nodes out in DLG format", ++step) ;
	Write_info(1, message) ;
	write_nodes(f_dlg) ;

	sprintf(message,"STEP %d: Write univ node out in DLG format", ++step) ;
	Write_info(1, message) ;
	write_univ_node (f_dlg) ;

	Clear_info() ;
	sprintf(message,"STEP %d: Write areas out in DLG format", ++step) ;
	Write_info(1, message) ;
	write_univ_areas (f_dlg) ;
	write_areas(f_dlg) ;

	Clear_info() ;
	sprintf(message,"STEP %d: Write lines out in DLG format.", ++step) ;
	Write_info(1, message) ;
	write_lines(f_digit, f_dlg, thresh) ;
	write_univ_lines (f_dlg) ;

	fflush(f_dlg) ;
	fclose(f_dlg) ;
	fclose(f_digit) ;

	if ( do_graphics() )
		R_close_driver() ;

shutdown:
	Close_curses() ;
}

#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "his.h"

int 
main (int argc, char **argv)
{
	CELL *hue_array ;
	CELL *int_array = NULL;
	CELL *out_array;
	CELL *sat_array = NULL;
	char *mapset ;
	char name1[100] ;
	char name2[100];
	char name3[100];
	char name4[100];
	char buff[128] ;
	int intensity ;
	int saturation ;
	int atrow, atcol ;
	int next_row;
	int hue_file ;
	int int_file = 0;
	int int_used ;
	int sat_file = 0;
	int sat_used ;
	int out_file = 0;
	int out_used ;
	struct Cell_head window ;
	struct Colors hue_colors ;
	struct Colors int_colors ;
	struct Colors out_colors ;
	struct Colors sat_colors ;
	struct Option *opt1, *opt2, *opt3, *opt4 ;
	struct Flag *flg1 ;
	char mg[100];

	opt1 = G_define_option() ;
	opt1->key        = "h_map" ;
	opt1->type       = TYPE_STRING ;
	opt1->required   = YES ;
	opt1->gisprompt  = "old,cell,raster" ;
	opt1->description= "Name of layer to be used for HUE" ;

	opt2 = G_define_option() ;
	opt2->key        = "i_map" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = NO ;
	opt2->gisprompt  = "old,cell,raster" ;
	opt2->description= "Name of layer to be used for INTENSITY" ;

	opt3 = G_define_option() ;
	opt3->key        = "s_map" ;
	opt3->type       = TYPE_STRING ;
	opt3->required   = NO ;
	opt3->gisprompt  = "old,cell,raster" ;
	opt3->description= "Name of layer to be used for SATURATION" ;

	opt4 = G_define_option() ;
	opt4->key        = "output" ;
	opt4->type       = TYPE_STRING ;
	opt4->required   = NO ;
	opt4->gisprompt  = "new,cell,raster" ;
	opt4->description= "Name of raster map to contain results" ;

	flg1 = G_define_flag() ;
	flg1->key	 = 'o' ;
	flg1->description= "Overwrite output map" ;

	G_gisinit(argv[0]) ;

	if (G_parser(argc, argv))
		exit(-1);

	/* read in current window */
	G_get_window(&window) ;

	/* Do screen initializing stuff */
	if (init_his())
	{
		fprintf(stderr,"Error: problem initializing graphics\n") ;
		exit(1) ;
	}

	/* Get name of layer to be used for hue */
	strcpy (name1, opt1->answer);

	mapset = G_find_cell2(name1, "");
	if (mapset == NULL)
	{
		sprintf(mg, "%s: <%s> cell file not found\n", G_program_name(),
		    opt1->answer);
		G_fatal_error(mg);
		exit(0) ;
	}

	/* Make sure map is available */
	if ((hue_file = G_open_cell_old(name1, mapset)) == -1)
	{
		sprintf(buff,"Not able to open cellfile for [%s]", name1) ;
		G_fatal_error(buff) ;
		exit(1);
	}

	hue_array = G_allocate_cell_buf () ;

	/* Reading color lookup table */
	if (G_read_colors(name1, mapset, &hue_colors) == -1)
	{
		sprintf(buff,"Color file for [%s] not available", name1) ;
		G_fatal_error(buff) ;
		exit(-1);
	}

	/* Get name of layer to be used for intensity */

	int_used = 0 ;

	if (opt2->answer != NULL)
	{
		strcpy (name2, opt2->answer);
		mapset = G_find_cell2(name2, "");
		if (mapset == NULL)
		{
			int_used = 0 ;
		}
		else
		{
			int_used = 1 ;
			/* Make sure map is available */
			if ((int_file = G_open_cell_old(name2, mapset)) == -1)
			{
				sprintf(buff,"Not able to open cellfile for [%s]", name2) ;
				G_fatal_error(buff) ;
				exit(-1);
			}

			int_array = G_allocate_cell_buf () ;

			/* Reading color lookup table */
			if (G_read_colors(name2, mapset, &int_colors) == -1)
			{
				sprintf(buff,"Color file for [%s] not available", name2) ;
				G_fatal_error(buff) ;
			}
		}
	}

	/* Get name of layer to be used for saturation */

	sat_used = 0 ;

	if (opt3->answer != NULL)
	{
		strcpy (name3, opt3->answer);
		mapset = G_find_cell2 (name3, "");
		if (mapset == NULL)
		{
			sat_used = 0 ;
		}
		else
		{
			sat_used = 1 ;

			/* Make sure map is available */
			if ((sat_file = G_open_cell_old(name3, mapset)) == -1)
			{
				sprintf(buff,"Not able to open cellfile for [%s]", name3) ;
				G_fatal_error(buff) ;
			}

			sat_array = G_allocate_cell_buf () ;

			/* Reading color lookup table */
			if (G_read_colors(name3, mapset, &sat_colors) == -1)
			{
				sprintf(buff,"Color file for [%s] not available", name3) ;
				G_fatal_error(buff) ;
			}
		}
	}

	out_used = 0;

	if (opt4->answer != NULL)
	{
		strcpy (name4, opt4->answer);

		mapset = G_find_cell2( name4, "");
		if (mapset == NULL)
		{
			out_used = 0;
		}
		else
		{
			if (flg1->answer)
			    G_remove("cell", name4);
			else
			{
			    sprintf (mg, "%s: <%s> cell file exists already\n", 
			        G_program_name(), opt4->answer);
			    G_fatal_error(mg);
			    exit(-1);
			}
		}
		if ((out_file = G_open_cell_new (name4)) < 0)
		{
			out_used = 0;
		}
		else
		{
			out_used = 1;
		}
	}

	out_array = G_allocate_cell_buf () ;

	/* Make color table */
	G_make_HIS_color(&out_colors) ;
	D_set_colors (&out_colors);

	/* Now do the work */
	intensity =  127 ;  /* default is to not change intensity */
	saturation = 255 ;  /* default is to not change saturation */

	alloc_pass_buff(window.cols) ;

	next_row = 0;
	for (atrow=0; atrow<window.rows; )
	{
		G_percent (atrow, window.rows, 2);
		if(G_get_c_raster_row(hue_file, hue_array, atrow) < 0)
			exit(1);
		if (int_used && (G_get_c_raster_row(int_file, int_array, atrow) < 0))
			exit(1);
		if (sat_used && (G_get_c_raster_row(sat_file, sat_array, atrow) < 0))
			exit(1);

		for (atcol=0; atcol<window.cols; atcol++)
		{
			int r, g, b ;

			if (int_used)
				G_get_color(int_array[atcol], &intensity, &g, &b, &int_colors) ;
			if (sat_used)
				G_get_color(sat_array[atcol], &saturation,&g, &b, &sat_colors) ;

			G_get_color(hue_array[atcol], &r, &g, &b, &hue_colors) ;

			if(r==255 && g==255 && b==255 && intensity==255 && saturation==255)
				G_set_c_null_value(&out_array[atcol], 1);
			else
				out_array[atcol] = (CELL) G_HIS(atcol, r, g, b, intensity, saturation ) ;
		}

		if (atrow == next_row)
			next_row = D_draw_cell(next_row, out_array, &out_colors);
		if (out_used)
		{
			if(G_put_c_raster_row (out_file, out_array) < 0)
				out_used = 0;
		}
		if (out_used)
			atrow++;
		else if (next_row > 0)
			atrow = next_row;
		else
			break;
	}
	G_percent (window.rows, window.rows, 5);

	/* Close down connection to window driver */
	D_add_to_list(G_recreate_command()) ;
	R_close_driver() ;

	/* Close the cell files */
	G_close_cell(hue_file) ;
	if (int_used)
		G_close_cell(int_file) ;
	if (sat_used)
		G_close_cell(sat_file) ;
	if (out_used)
	{
		fprintf (stdout,"CREATING SUPPORT FILES FOR %s\n", name4);
		G_close_cell(out_file);
		G_write_colors (name4, G_mapset(), &out_colors);
	}
	exit(0);
}

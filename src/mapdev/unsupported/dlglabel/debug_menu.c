/*  @(#)debug_menu.c	2.1  6/26/87  */
#include "dlg.h"
#include <stdio.h>
#include "convert.h"


debug_menu()
{
	char cur_char ;
	char buffer[128] ;
	char answer[64] ;
	long atoi() ;
	int choice ;

	while(1) 
	{

		Clear_menu() ;
		Write_menu_line ( 4, " f - print internal array");
		Write_menu_line ( 5, "       to file.");
		Write_menu_line ( 6, " s - show a b-box");
		Write_menu_line ( 7, " a - show all b-box");
		Write_menu_line ( 8, " A - show areas");
		Write_menu_line ( 9, " q - quit");

		R_flush() ;
		cur_char = getchar() & 0177 ;
		sprintf(buffer,"  %c",cur_char) ;
		Write_message(2, buffer) ;
		switch(cur_char)
		{
			case 'f':
				debug_to_file() ;
				break;

			case 's':
				Clear_menu() ;
				debug_plot_box() ;
				break;

			case 'a':
				debug_all_boxs() ;
				break;

			case 'w':
				Clear_menu() ;
				where_am_i() ;
				Clear_message() ;
				break ;

			case 'A':
				Clear_menu() ;
				debug_show_area() ;
				break;

			case 'q':
				return(0) ;
			default:
				sprintf(buffer,"  %c - Unknown Command",cur_char) ;
				Write_message(2, buffer) ;
				break ;
		}
	}
}


#include	"dlghead.h"

debug_to_file()
{
	int i ;
	FILE *fp, *fopen() ;

	if ( ( fp = fopen ("debug_dlg", "w")) == NULL)
		return ;

	fprintf ( fp, "\n%s\n", dlg_head.banner) ;
	fprintf ( fp, "   nodes = %d,   areas = %d,   lines = %d\n\n",
	tot_nodes, tot_areas, tot_lines) ;	

	for(i=1; i<=tot_nodes; i++)
		fprintf(fp, "%1c%5d%12.2lf%12.2lf%6d%6d%6d%6d%6d%6d\n",
			'N', i, node[i].x, node[i].y, 0, node[i].n_lines, 0, node[i].n_atts,
			0, 0) ;


	for(i=1; i<=tot_areas; i++)
	{
		fprintf(fp, "%1c%5d%12.2lf%12.2lf%6d%6d%6d%6d%6d%6d\n",
			'A', i, area[i].x, area[i].y, 0, area[i].n_lines, 0, area[i].n_atts,
			0, area[i].n_isles) ;

		/*  bounding box  */
		fprintf(fp, "   N:%12.2lf,    S:%12.2lf,   E:%12.2lf,    W:%12.2lf\n",
			area[i].N,	area[i].S,	area[i].E,	area[i].W ) ;
		fprintf(fp, "   height: %12.2lf,    width:%12.2lf\n\n",
			(area[i].N - area[i].S),  (area[i].E -area[i].W) ) ;
	}


	for(i=1; i<=tot_lines; i++)
	{
		fprintf(fp, "%1c%5d%6d%6d%6d%6d            %6d%6d\n",
			'L', i, line[i].start_node, line[i].end_node,
			line[i].left_area, line[i].right_area, line[i].n_coors,
			line[i].n_atts ) ;

		/*  bounding box  */
		fprintf(fp, "   N:%12.2lf,    S:%12.2lf,   E:%12.2lf,    W:%12.2lf\n",
			line[i].N,	line[i].S,	line[i].E,	line[i].W ) ;
		fprintf(fp, "   height: %12.2lf,    width:%12.2lf\n\n",
			(line[i].N - line[i].S),  (line[i].E -line[i].W) ) ;
	}

	

	fprintf ( fp, "\n\n") ;	

	fclose (fp) ;


}


debug_plot_box ()
{
	int screen_x, screen_y ;
	int button ;
	int nogo ;
	int i ;

	double ux, uy ;
	double	box_coors[10] ;
	
	char buff[80] ;

	Clear_menu() ;
	Clear_message() ;

	for(;;)
	{
		Clear_message() ;

		Write_menu_line(6, "Identify LINE to be boxed") ;
		Write_menu_line(8, "Buttons:") ;
		Write_menu_line(9, "Left:    show b-box") ;
		Write_menu_line(10, "Middle: blank b-box") ;
		Write_menu_line(11, "Right:  return") ;

		nogo = 0 ;
		for(;;)
		{
			R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
			ux = ((double)screen_x - D_west) / U_to_D_xconv + U_west ;
			uy = ((double)screen_y - D_south)/ U_to_D_yconv + U_south ;


			switch(button)
			{
				case 1:
				case 2:

					Write_message(3, "Checking lines") ;
					if ( i = point_to_line(ux, uy) )
					{
						if (button ==1)
							R_standard_color( D_translate_color("orange") ) ;
						else
							R_standard_color( D_translate_color("black") ) ;

						box_coors[0] = line[i].W ;	box_coors[1] = line[i].N ;
						box_coors[2] = line[i].E ;	box_coors[3] = line[i].N ;
						box_coors[4] = line[i].E ;	box_coors[5] = line[i].S ;
						box_coors[6] = line[i].W ;	box_coors[7] = line[i].S ;
						box_coors[8] = line[i].W ;	box_coors[9] = line[i].N ;

						plot_all_coors ( 5, box_coors) ;


						Write_line(i, &line[i]) ;
					}

					Write_message(3, " ") ;
					break ;

				case 3:
					return ;
					break ;
				default:
					break ;
			}
		}
	}
}

debug_all_boxs ()
{
	int i ;

	double	box_coors[10] ;
	
	R_standard_color( D_translate_color("orange") ) ;

	for (i=0; i <= tot_lines; i++)
	{

		box_coors[0] = line[i].W ;	box_coors[1] = line[i].N ;
		box_coors[2] = line[i].E ;	box_coors[3] = line[i].N ;
		box_coors[4] = line[i].E ;	box_coors[5] = line[i].S ;
		box_coors[6] = line[i].W ;	box_coors[7] = line[i].S ;
		box_coors[8] = line[i].W ;	box_coors[9] = line[i].N ;

		plot_all_coors ( 5, box_coors) ;

	}

}

debug_show_area() 
{
	int screen_x, screen_y ;
	int cur_screen_x, cur_screen_y ;
	double ux, uy ;
	int area_num ;
	int last_area ;
	char buff[80] ;
	int button ;
	int choice ;
	int nogo ;
	
	Clear_menu() ;
	Write_menu_line(3,"AREAS  are  red") ;
	Write_menu_line(4,"ISLANDS  are  green") ;

	Clear_message() ;
	Write_message(1, "Enter area number ") ;
	Write_message(2, "  (or 0 to use mouse): ") ;
	Get_curses_text(buff) ;
	sscanf(buff,"%d",&area_num) ;
	if (area_num != 0)
	{
		R_standard_color( D_translate_color("red") ) ;
		debug_plot_area(area_num) ;
		R_standard_color( D_translate_color("green") ) ;
		debug_plot_area_islands(area_num) ;
		Clear_message() ;
		return(0) ;
	}

	Clear_message() ;
	nogo = 0 ;

	for(;;)
	{

		Write_menu_line(6,"Use cursor to identify AREA") ;
		Write_menu_line(8, "Buttons:") ;
		Write_menu_line(9, "Left:   where am i") ;
		Write_menu_line(10, "Middle: show this area") ;
		Write_menu_line(11, "Right:  return") ;

		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		screen_to_utm (screen_x, screen_y, &ux, &uy) ;
		switch(button)
		{
			case 1:
				sprintf(buff,"EAST: %12.2lf", ux) ;
				Write_message(1, buff) ;
				sprintf(buff,"NORTH: %12.2lf", uy) ;
				Write_message(2, buff) ;
				break ;
			case 2:
				Write_message(3, "Checking areas") ;
				if ( area_num = point_to_area(ux, uy) )
				{
					if (last_area)
					{
						R_standard_color( D_translate_color("gray") ) ;
						debug_blank_area(last_area) ;
					}

					sprintf(buff,"area #: %d", area_num) ;
					Write_message(2, buff) ;

					R_standard_color( D_translate_color("red") ) ;
					debug_plot_area(area_num) ;

					R_standard_color( D_translate_color("green") ) ;
					debug_plot_area_islands(area_num) ;
					last_area = area_num ;
				}
				Write_message(3, " ") ;
				break ;
			case 3:
				nogo = 1 ;
				break ;
			default:
				break ;
		}
		if (nogo)
			break ;

	}  /*  for()  */

}  /*  debug_show_area()  */


#include "externs.h"
#include <signal.h>

/* plot_area plots lines that area "a" claims
 * are associated with it.
 */
debug_plot_area(a)
	long a ;
{
	int i ;
	int l ;

	Old_tty() ;
	set_signals() ;

	for (i=0; i<area[a].n_lines; i++)
	{
		if (signalflag.interrupt)
			break ;
		l = abs(area[a].lines[i]) ;
		if (! l)
			break ;
		plot_line(l, SOLID) ;
	}

	New_tty() ;
}

debug_plot_area_islands(a)
	long a ;
{
	int i ;
	int l ;

	Old_tty() ;
	set_signals() ;

	for (i=0; i<area[a].n_lines; i++)
	{
		if (signalflag.interrupt)
			break ;
		l = abs(area[a].lines[i]) ;
		if (! l)
			break ;
	}

	for (; i<area[a].n_lines; i++)
	{
		if (signalflag.interrupt)
			break ;
		l = abs(area[a].lines[i]) ;
		if (! l)
			continue ;
		plot_line(l, SOLID) ;
	}

	New_tty() ;
}

debug_blank_area(a)
	long a ;
{
	int i ;
	int l ;

	Old_tty() ;
	set_signals() ;


	for ( i=0; i<area[a].n_lines; i++)
	{
		if (signalflag.interrupt)
			break ;
		l = abs(area[a].lines[i]) ;
		if (! l)
			continue ;
		plot_line(l, SOLID) ;
	}

	New_tty() ;
}

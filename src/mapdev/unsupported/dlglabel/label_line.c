/*  @(#)label_lines.c	1.1  5/4/87  */
#include "dlg.h"

label_lines()
{
	int screen_x, screen_y ;
	double ux, uy ;
	int cat_num ;
	char buff[80] ;
	int	button ;
	int choice ;
	int nogo ;
	int n ;
	
	Clear_menu() ;
	Clear_message() ;

	for(;;)
	{
		Clear_message() ;
		Write_message(1, "Enter category number ") ;
		Write_message(2, "  (or 0 to quit): ") ;
		Get_curses_text(buff) ;
		sscanf(buff,"%d",&cat_num) ;
		if (cat_num == 0)
		{
			Clear_message() ;
			return(0) ;
		}

		Write_menu_line(4,"LINE labeling:") ;
		Write_menu_line(6,"Use cursor to identify LINE") ;
		Write_menu_line(8, "Buttons:") ;
		Write_menu_line(9, "Left:   where am i") ;
		Write_menu_line(10, "Middle: label this LINE") ;
		Write_menu_line(11, "Right:  return") ;

		nogo = 0 ;
		for(;;)
		{
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
					Write_message(3, "Checking lines") ;
					if ( choice = point_to_line(ux, uy) )
					{
						/* First min. att. code */
						if ( ! line[choice].n_atts)
						{
							line[choice].atts = (int *)(calloc(2,sizeof(int))) ;
							if (line[choice].atts == NULL)
							{
								fprintf(stderr,"ERROR: insufficient memory\n") ;
								return(-1) ;
							}
						}

					/* Set the new coordinates and cat num */
						line[choice].atts[0] = 999 ;
						line[choice].atts[1] = cat_num ;
						line[choice].n_atts = 1 ;

						R_standard_color( D_translate_color("violet") ) ;

						if ( (n = plot_line(choice, SOLID)) < 0)
						{
							sprintf(buff, "Error on read (%d)", choice) ;
							Write_message(5, buff) ;
							getchar() ;
						}

						Write_line(choice, &line[choice]) ;
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
		}
	}
}


unlabel_lines()
{
	int screen_x, screen_y ;
	double ux, uy ;
	char buff[80] ;
	int  button ;
	int  choice ;
	int  n ;
	
	Clear_menu() ;
	Clear_message() ;


	Write_menu_line(6,"Use cursor to identify LINE") ;
	Write_menu_line(8, "Buttons:") ;
	Write_menu_line(9, "Left:   where am i") ;
	Write_menu_line(10, "Middle: UN-label this line") ;
	Write_menu_line(11, "Right:  return") ;

	for(;;)
	{
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
				Write_message(3, "Checking lines") ;
				if ( choice = point_to_line(ux, uy) )
				{
					/* First min. att. code */
					if (! line[choice].n_atts)
					{
						sprintf(buff, "Line (%d) has no label", choice) ;
						Write_message(5, buff) ;
						Write_message(3, " ") ;
						continue ;
					}

				/* Set the new coordinates and cat num */
					line[choice].n_atts = 0 ;

					R_standard_color( D_translate_color("gray") ) ;

					if ( (n = plot_line(choice, SOLID)) < 0)
					{
						sprintf(buff, "Error on read (%d)", choice) ;
						Write_message(5, buff) ;
						getchar() ;
					}

					Write_line(choice, &line[choice]) ;
				}
				Write_message(3, " ") ;
				break ;
			case 3:
				return(0) ;
				break ;
			default:
				break ;
		}
	}
}


/*  @(#)make_line.c	2.1  6/26/87  */
#include "dlg.h"

make_line(choice, redraw)
	int choice ;
	int redraw ;
{
	char buffer[128] ;
	int new_node ;
	int new_area ;
	int points ;
	int beg_node, end_node ;
	double *xyptr ;

	Clear_menu() ;
	Clear_message() ;

	if(redraw)
	{
		beg_node = line[choice].start_node ;
		end_node = line[choice].end_node ;
	}

	xyptr = coors ;

/* Draw new line */
	points = 0 ;

	if (redraw)
	{
		xyptr[0] = node[beg_node].x ;	
		xyptr[1] = node[beg_node].y ;	
		assign_first(xyptr[0], xyptr[1]) ;
	}
	else
		get_first(xyptr, xyptr+1) ;

	First(xyptr, xyptr+1) ;

	points = 1 ;
	xyptr += 2 ;

	for(;;)
	{
		if (! get_next(xyptr, xyptr+1) )
			break ;
		R_standard_color( D_translate_color("red") ) ;
		Next(xyptr, xyptr+1) ;
		xyptr += 2 ;
		alloc_coors(++points) ;
	}

	if (redraw)
	{
		*(xyptr) = node[end_node].x ;	
		*(xyptr+1) = node[end_node].y ;	
		Next(xyptr, xyptr+1) ;
		R_flush() ;
		points++ ;
	}

	if (! redraw)
	{
	/* Get associated areas */
		line_areas(choice) ;

	/* Figure out associated nodes */
		line_nodes(choice) ;
	}

/* Make final check */
	Clear_message() ;
	Write_message(3,"Are you sure? (y/n) > ") ;
	Get_curses_text(buffer) ;
	switch(*buffer)
	{
		case 'y':
			fseek(dlg_tmp,0L,2) ;
			line[choice].offset = ftell(dlg_tmp) ;
			line[choice].file = dlg_tmp ;
			line[choice].n_coors = points ;
		/* Save coordinates */
			write_double(dlg_tmp, points*2, coors) ;
			return(1) ;
			break ;
		case 'n':
			/* erase line */
			R_standard_color( D_translate_color("black") ) ;
			xyptr = coors ;
			First(xyptr, xyptr+1) ;
			xyptr += 2 ;
			points-- ;
			while(--points)
			{
				Next(xyptr, xyptr+1) ;
				xyptr += 2 ;
				points++ ;
			}
			if (! redraw)
			{
				/* Zero line info */
				line[choice].start_node = 0 ;
				line[choice].end_node = 0 ;
				line[choice].left_area = 0 ;
				line[choice].right_area = 0 ;
				line[choice].n_coors = 0 ;
				line[choice].n_atts = 0 ;
			}
			return(0) ;
			break ;
		default:
			Write_message(2,"Taking that as NO") ;
			sleep(2) ;
			break ;
	}
}

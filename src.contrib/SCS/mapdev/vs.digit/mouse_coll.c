/*  @(#)coll_pts.c	1.1  5/4/87  */
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "popup.h"

/*  
*   collect_points() - collect a set of points.
*	coll_a_pnt() -  collect a point from digitizer.
*/


/*  these correspond to the cursor keys */
#define		DIG_POINT	1
#define		BACKUP		2
#define		STOP_DIG	3

static mouse_get_point ();
static int show;

mouse_collect_points (mode, type, Points)
	int mode;
	char type ;
	struct line_pnts *Points;
{
    int     Xraw;
    int     Yraw;
    int		Xlast ;
    int		Ylast ;
    int		button ;
    int		loop ;
    int stream_mode ;
    int run_mode ;
    double	*xptr ;
    double	*yptr ;
    char buf[80];
    int *n_points ;
    int cnt;
    int menu_left, menu_top;
    int ret, chr;

    menu_left = Next_l + 1;
    menu_top = Next_t;

    if (Points->alloc_points == 0)
    {
	dig_alloc_points (Points, 500);
    }
    n_points = &(Points->n_points);
    mode = POINT;
    run_mode = mode ;
    xptr = Points->x ;
    yptr = Points->y ;

    {
    Xlast = 0 ;
    Ylast = 0 ;
    *n_points = 0 ;
    stream_mode = 0 ;

    show = 1;
    message[0] = " POINT mode ";
    message[1] = " # Points       Easting     Northing" ;
    sprintf(buf, "   %6d   %12.2lf %13.2lf .", *n_points, 0.0, 0.0);
    message[2] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[2],"%s", buf);
    message[3] = "";
    message[4] = '\0';
    popup_messg("show_stat", 1);
    }

    loop = 1 ;

    Xraw = Yraw = 100;
    buttons[0] = " ";
    buttons[1] = "Buttons:\0";
    buttons[2] = "Left:   Digitize a site";
    buttons[3] = "Middle: Abort/Quit";
    buttons[4] = "Right:  Abort/Quit";
    buttons[5] = "  ";
    buttons[6] = '\0';


    cnt = 0;
    /*  digitizing loop  */
    while (loop)
    {
/*-->*/ if (type == DOT)		/* scs added code */
	{
           Dchoose(MEN.name) ;
           popup_butns( menu_top, menu_left, buttons, "mous_col", 1) ;
           Dchoose(DIG.name) ;

/*	   button = mouse_get_point (&Xraw, &Yraw, *n_points);*/
/*-->*/	   R_get_location_with_pointer ( &Xraw, &Yraw, &button); /*ADDED*/
/*-->*/	   flush_keyboard (); /*ADDED*/
/*-->*/	}
	else
	    button = mouse_get_point (&Xraw, &Yraw, *n_points);

	switch (button) {
	    case STOP_DIG:
		if (type == DOT)
		    {
                    erase_popup("mous_col");
                    erase_popup("show_stat");
		    return (mode);
		    }
		loop = 0 ;
                erase_popup("get_pt");
                erase_popup("show_stat");
		continue ;
		break ;

	    case BACKUP:
		if (type == DOT)	/* abort */
		    {
                    erase_popup("mous_col");
                    erase_popup("show_stat");
		    return (mode);
		    }
		if (*n_points <= 0)
		{
		    BEEP;
		    *n_points = 0; /* just in case */
		    continue;
		}
		
		(--(*n_points));
		xptr--;
		yptr--;
		if (*n_points)
		    utm_to_screen (*(xptr-1), *(yptr-1), &Xraw, &Yraw);
		else
		    utm_to_screen (*(xptr), *(yptr), &Xraw, &Yraw);
		switch (*n_points) {
		    case 0:
			plot_points( type, 1, xptr, yptr, CLR_ERASE, CLR_ERASE);
			break;
		    case 1:
			plot_points( type, 2, xptr-1, yptr-1, CLR_ERASE, 0);
			plot_points( type, 1, xptr-1, yptr-1, CLR_0_NODE, CLR_0_NODE);
			break;
		    default:
			plot_points( type, 2, xptr-1, yptr-1, CLR_ERASE, 0);
			break;
		}
		continue ;
		break ;

	    case DIG_POINT:
	    default:
		if (type == DOT)
                    erase_popup("mous_col");
		break ;

	}		/*  end of switch  */

	if (run_mode == POINT  && button != DIG_POINT )
	continue ;


	/*  digitizer sitting in the same place  */
	if (*n_points)
	{
	    if ( Xlast == Xraw  &&  Ylast == Yraw)
		continue ;
	}

	Xlast = Xraw ;
	Ylast = Yraw ;

	/* 
	** + 2 added for DOT
	*/
	if ((*n_points) + 2 >= Points->alloc_points)
	{
	    dig_alloc_points (Points, Points->alloc_points + 50);

	    xptr = Points->x + *n_points ;
	    yptr = Points->y + *n_points ;
	}

	/*  convert raw coor. to utm coor.  */
	screen_to_utm (Xraw, Yraw, xptr, yptr) ;
	(*n_points)++ ;

	/*  update the monitor for each new point  */
	if (run_mode == POINT)
	{
	    /*  blot the 1st node, but after that its a line  */

	    if (*n_points == 1)
		plot_points( type, 1, xptr, yptr, CLR_HIGHLIGHT, CLR_HIGHLIGHT);
	    else
		plot_points( type, *n_points, Points->x, Points->y, CLR_HIGHLIGHT, 0);
	}



	/*  show user coor. of new point  */
	sprintf(buf, "   %6d   %12.2lf %13.2lf .", *n_points, *xptr, *yptr);
        sprintf(message[2],"%s", buf);
        Dchoose(MEN.name) ;
	popup_messg("show_stat",0);
        Dchoose(DIG.name) ;

	if (type == DOT)
	{
	    loop = 0;
	    Points->x[1] = Points->x[0];
	    Points->y[1] = Points->y[0];
	    (*n_points)++ ;
	}


	/*  advance pointers for the next set of coor. */
	xptr++ ;
	yptr++ ;

    }

    /*  black at the line on the monitor  exactly as it is now  */
    plot_points( type, 1, Points->x, Points->y, CLR_ERASE, CLR_ERASE);
    if (*n_points)
	plot_points( type, *n_points, Points->x, Points->y, CLR_ERASE, CLR_ERASE);

    return(stream_mode) ;

}	/*  coll_pts()  */




/* x = y = 0.0  on abort.  */
static
mouse_get_point (screen_x, screen_y, cnt)
    int    *screen_x, *screen_y;
    int cnt;
{
    int  button;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64];
    int prev;
    int menu_left, menu_top;
    int ret, chr, panel_opt;

    menu_left = Next_l + 1;
    menu_top = Next_t;

    if (cnt == 0) panel_opt = 1;
    else panel_opt = 0;

    sprintf(buffer,"Point number %d\0", cnt);
    buttons[0] = (char *) malloc (strlen (buffer) + 1);
    sprintf(buttons[0],"%s\0", buffer);
    buttons[1] = "Buttons:";
    buttons[2] = "Left:   Mark a point";
    if (cnt)
        buttons[3] = "Middle:  Backup one point";
    else
        buttons[3] = "Middle:(Backup one point)";
    buttons[4] = "Right:  Quit digitizing";
    buttons[5] = "  ";
    buttons[6] = '\0';

    Dchoose(MEN.name) ;
    popup_butns( menu_top, menu_left, buttons, "get_pt", panel_opt) ;
    Dchoose(DIG.name) ;

    if (cnt)
	R_get_location_with_line (*screen_x, *screen_y, screen_x, screen_y, &button);
    else
	R_get_location_with_pointer ( screen_x, screen_y, &button);

    flush_keyboard ();
    return (button);


/*
*   screen_to_utm(screen_x, screen_y, &ux2, &uy2);
*   sprintf(buffer," EAST:  %10.2f", ux1>ux2?ux1:ux2);
*	fprintf(stderr,"%s\n", buffer);
*   sprintf(buffer," NORTH: %10.2f .", uy1>uy2?uy1:uy2);
*	fprintf(stderr,"%s\n", buffer);
*/
}


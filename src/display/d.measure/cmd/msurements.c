#include "gis.h"

measurements(color1,color2)
{
    double *x, *y;
    int npoints, nalloc;
    double area;
    double cur_ux, cur_uy ;
    double D_d_to_u_row(), D_d_to_u_col() ;
    double hypot() ;
    double length ;
    double ux, uy ;
    int button ;
    int cur_screen_x, cur_screen_y ;
    int screen_x, screen_y ;
    struct Cell_head window ;
    int t, b, l, r ;
    double G_area_of_polygon();
    double G_distance();

    nalloc = 128;
    x = (double *)G_calloc (nalloc, sizeof(double));
    y = (double *)G_calloc (nalloc, sizeof(double));


/* Set up area/distance calculations  */
    G_begin_polygon_area_calculations();
    G_begin_distance_calculations();

    G_get_window(&window) ;
    D_get_screen_window(&t, &b, &l, &r) ;
    D_do_conversions(&window, t, b, l, r) ;

    for(;;)
    {
	npoints = 0;
        G_clear_screen() ;
        printf( "\nButtons:\n") ;
        printf( "Left:   where am i\n") ;
        printf( "Middle: set FIRST vertex\n") ;
        printf( "Right:  quit this\n") ;

        screen_y  = (t + b) / 2 ;
        screen_x  = (l + r) / 2 ;

        do
        {
            R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
            cur_uy = D_d_to_u_row((double)screen_y)  ;
            cur_ux = D_d_to_u_col((double)screen_x)  ;
	    if (button == 1)
		print_en(cur_ux, cur_uy);
            if(button == 3)
                return(0) ;
        } while (button != 2) ;

	add_point (&x, &y, &npoints, &nalloc, cur_ux, cur_uy);
        G_clear_screen() ;
        printf( "\nLeft:   where am i\n") ;
        printf( "Middle: set NEXT vertex\n") ;
        printf( "Right:  FINISH\n") ;

        R_move_abs(screen_x, screen_y) ;
        cur_screen_x = screen_x ;
        cur_screen_y = screen_y ;

        length = 0.0 ;

        do
        {
            R_standard_color (color1);
            R_get_location_with_line(cur_screen_x,cur_screen_y,&screen_x, &screen_y, &button) ;
            uy = D_d_to_u_row((double)screen_y)  ;
            ux = D_d_to_u_col((double)screen_x)  ;
            switch (button)
            {
            case 1:
                print_en (ux, uy);
                break ;
            case 2:
                draw_line(screen_x,screen_y,cur_screen_x,cur_screen_y,color1,color2)  ;
		add_point (&x, &y, &npoints, &nalloc, ux, uy);
                length += G_distance(cur_ux, cur_uy, ux, uy) ;
                print_length(length);
                cur_screen_x = screen_x ;
                cur_screen_y = screen_y ;
                cur_ux = ux ;
                cur_uy = uy ;
                break ;
            default:
                break ;
            }
        } while (button != 3) ;
	R_flush();

        G_clear_screen() ;
        printf( "\nButtons:\n") ;
        printf( "Left:   DO ANOTHER\n") ;
        printf( "Middle: \n") ;
        printf( "Right:  quit this\n") ;
/*
 * 10000 is sq meters per hectare
 * 2589988 is sq meters per sq mile
 */
        printf ("\n");
        print_length(length);
	if (npoints > 3)
	{
	    area = G_area_of_polygon (x, y, npoints);
	    printf("AREA:  %10.2f hectares\n", area / 10000 ) ;
	    printf("       %10.4f square miles\n", area / 2589988.11 ) ;
	    printf("       %10.2f square meters\n", area) ;
	}

        R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
        if (button == 3)
            return(0) ;
    }
}

print_en(e,n)
    double e,n;
{
    char buf[100];

    G_format_easting (e, buf, G_projection());
    printf( "EAST:  %s\n", buf);
    G_format_northing (n, buf, G_projection());
    printf( "NORTH: %s\n", buf);
}

print_length(length)
    double length;
{
    printf("LEN:   %10.2f meters\n", length) ;
}

add_point (x, y, npoints, nalloc, ux, uy)
    double **x, **y, ux, uy;
    int *npoints, *nalloc;
{
    double *px, *py;

    px = *x;
    py = *y;

    if (*npoints >= *nalloc)
    {
	*nalloc *= 2;
	*x = px = (double *) G_realloc (px, *nalloc * sizeof(double));
	*y = py = (double *) G_realloc (py, *nalloc * sizeof(double));
    }
    px[*npoints] = ux;
    py[*npoints] = uy;
    *npoints += 1;
}

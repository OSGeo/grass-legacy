#include "gis.h"
#include "infx.h"

int getArea(tp)
  struct Sql *tp;
{
	char buffer[200] ;
	char buf1[1024], buf2[1024];
	char temp[1024];
	double lat, lon ;
	int screen_x, screen_y ;
	int curx, cury ;
	double east, north ;
	double east1, north1 ;
	double east2, north2 ;
	int button ;
	double D_get_d_north(), D_get_d_south() ;
	double D_get_d_east(), D_get_d_west() ;
	double D_d_to_u_row(), D_d_to_u_col() ;
	int projection;

	projection = G_projection();

	screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;



	fprintf(stderr, "\n\nButtons:\n") ;
	fprintf(stderr, "Left:  Select site for DB query.\n") ;
	fprintf(stderr, "Right: Finish. \n\n\n") ;
	R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
	east = D_d_to_u_col((double)screen_x) ;
	north = D_d_to_u_row((double)screen_y) ;
	tp->centX = east;
	tp->centY = north;
	tp->permX = east + tp->distance;
	tp->permY = north + tp->distance;
	tp->maxX = east + tp->distance;
	tp->maxY = north + tp->distance;
	tp->minX = east - tp->distance;
	tp->minY = north - tp->distance;

	tp->rad2 = ((tp->permX - tp->centX) * (tp->permX - tp->centX) +
       			(tp->permY - tp->centY) * (tp->permY - tp->centY) );


	return(button) ;


}

static
show (buf) char *buf;
{
	char *b;

	if (!isatty(1))
		printf ("%s\n", buf);
	for (b = buf; *b; b++)
		fprintf (stderr, "%c", *b);
	for (b = buf; *b; b++)
		fprintf (stderr, "\b");
}

#include <stdio.h>
#include <unistd.h>
#include "raster.h"
#include "display.h"
#include "config.h"
#include "local_proto.h"

#define NL	012
#define TAB	011
#define BACK	0134
#define MTEXT	1024

#define TOP	0
#define CENT	1
#define BOT	2
#define LEFT	0
#define RITE	2
#define YES	1
#define NO	0
static double east ;
static double north ;

int in_window (char *easting, char *northing)
{
	char line[256] ;
	int X, Y ;
	int T, B, L, R ;
	int t, b, l, r ;

	sscanf(easting,"%lf",&east) ;
	sscanf(northing,"%lf",&north) ;
	X = (int)(D_u_to_d_col(east)) ;
	sprintf(line,"X");

	/* Find extent of all text (assume ref point is upper left) */
	T = 999999 ;
	B = 0 ;
	L = 999999 ;
	R = 0 ;

	Y = (int)(D_u_to_d_row(north)) ;
	R_move_abs(X, Y) ;
	R_get_text_box(line, &t, &b, &l, &r) ;

	if (t < T) T = t ;
	if (b > B) B = b ;
	if (l < L) L = l ;
	if (r > R) R = r ;

	/* If the window is outside of current map window, ignore */;
  	if (R < (int)D_get_d_west())
	      {
	      fprintf(stderr,"Label point NOT in current active window\n");
              fprintf(stderr," Label point %d is less than west %d\n",R,(int)D_get_d_west());
	      sleep(2);
	      return(0) ;
	      }
	if (L > (int)D_get_d_east())
	      {
	      fprintf(stderr,"Label point NOT in current active window\n");
              fprintf(stderr," Label point %d is greater than east %d\n",L,(int)D_get_d_east());
	      sleep(2);
	      return(0) ;
	      }
	if (B < (int)D_get_d_north())
	      {
	      fprintf(stderr,"Label point NOT in current active window\n");
              fprintf(stderr," Label point %d is less than north %d\n",B,(int)D_get_d_north());
	      sleep(2);
	      return(0) ;
	      }
	if (T > (int)D_get_d_south())
	      {
	      fprintf(stderr,"Label point NOT in current active window\n");
              fprintf(stderr," Label point %d is greater than south %d\n",T,(int)D_get_d_south());
	      sleep(2);
	      return(0) ;
	      }
	return(1);
}

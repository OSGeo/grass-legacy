#include <stdio.h>
#include "config.h"

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
static int xoffset ;
static int yoffset ;
static int xref ;
static int yref ;
static int color ;
static double size ;
static int width ;
static int background ;
static int border ;
static int opaque ;
static char text[MTEXT] ;
static char dfont[20] ;

in_window(easting, northing)
char *easting, *northing;
{
	char *tmp_fname, *G_tempfile();
        int flag;
        char yn[80];
        int line_num ;
	int n_lines ;
	int n_chars ;
	char line[256] ;
	char *lptr, *tptr ;
	double line_size ;
	int text_size ;
	int X, Y ;
	int T, B, L, R ;
	int scrT, scrB, scrL, scrR ;
	int t, b, l, r ;
	int xarr[5] ;
	int yarr[5] ;
	int Xoffset ;
	int Yoffset ;
	double D_u_to_d_row() ;
	double D_u_to_d_col() ;
	double D_get_d_west() ;
	double D_get_d_east() ;
	double D_get_d_north();
	double D_get_d_south();

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

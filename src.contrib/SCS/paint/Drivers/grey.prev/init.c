#define GLOBAL
#include "P.h"
#include <stdio.h>

Pinit()
{
    char buf[128] ;
    char window_name[64] ;
    int color ;
    int ncolors;
    int offset;
    float red, grn, blu;
    int r,g,b;
    char *getenv(), *background;

	if (R_color_table_float() != 0)
		fprintf(stderr,"could not go into float\n");
    if (D_get_cur_wind(window_name))
    {
	Dclearscreen();
	Dnew ("full", 0, 100, 0, 100);
	Dchoose ("full");
    }
    if (D_get_cur_wind(window_name))
	error ("No current graphics window");

    if (D_set_cur_wind(window_name))
	error("Current graphics window not available") ;

    if (D_get_screen_window(&top, &bottom, &left, &right))
	error("Can't determine screen coordinates") ;

    window_nrows = bottom-top-1;
    window_ncols = right-left-1;
    if (window_nrows < 3 || window_ncols < 3)
	error ("Graphics window is too small");

    if (D_clear_window())
	error("Can't clear graphics window") ;

/* Erase the window */
    if ((background = getenv ("PREVIEW_BACKGROUND")) == NULL)
	background = "grey";
    color = D_translate_color(background) ;
    R_standard_color(color) ;
    R_box_abs (left, top, right, bottom);

/* tell monitor about color offset for this window */
/*    D_offset_is(&offset) ;
    R_color_offset(offset) ;*/

/* load the color table */
    ncolors = Pncolors();
    for (color = 0; color < ncolors; color++)
    {
	Pcolorvalue (color, &red, &grn, &blu);

/* masscomp compiler bugs force me to do this nonsense */
	red = red * 255.0;
	r = (int)red;

	grn = grn * 255.0;
	g = (int)grn;

	blu = blu * 255.0;
	b = (int)blu;


	if ( r > 255) r = 255;
	if ( g > 255) g = 255;
	if ( b > 255) b = 255;
/*fprintf(stderr,"reset_ %d_ %d %d %d\n",color,r,g,b);*/
	R_reset_color ((char)r, (char)g, (char)b, color);
    }

/* set RLE to DATA conversion buffer to NULL */
    data = NULL;
}

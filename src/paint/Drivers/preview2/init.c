#define GLOBAL
#include "P.h"

Pinit()
{
    char *malloc();
    char buf[128] ;
    char window_name[64] ;
    int color ;
    int ncolors;
    int offset;
    float red, grn, blu;
    int r,g,b;

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


/* tell monitor about color offset for this window */
    D_offset_is(&offset) ;
    R_color_offset(offset) ;

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

	R_reset_color ((char)r, (char)g, (char)b, color);
    }

/* allocate RLE to DATA conversion buffer */
    data = (int *) malloc (window_ncols * sizeof(int));

/* Erase the window */
    erase();
}

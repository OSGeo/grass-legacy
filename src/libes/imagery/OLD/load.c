/****************************************************************
* I_load_colors (set_color)
*   int (*set_color)();
*
* down loads a color table which corresponds to the current
* color levels as set by last call to I_set_color_levels()
* thru a specified routine (set_color) which must be
* defined as
*
*   set_color (n, fr, fg, fb)
*        int n;             color number to set
*        float fr;          percentage red
*        float fg;          percentage green
*        float fb;          percentage blue
****************************************************************/
I_load_colors (set_color)
    int (*set_color)();
{
    int red, grn, blu;
    float xr,xg,xb;
    float fr,fg,fb;
    int r,g,b;

    I_get_color_levels (&red, &grn, &blu);

    xr = 1.0 / (red - 1);
    xg = 1.0 / (grn - 1);
    xb = 1.0 / (blu - 1);

    for (fr=0.0, r=0; r < red; r++, fr += xr)
	for (fg=0.0, g=0; g < grn; g++, fg += xg)
	    for (fb=0.0, b=0; b < blu; b++, fb += xb)
		(*set_color) (I_color_n(r,g,b),fr,fg,fb);
}

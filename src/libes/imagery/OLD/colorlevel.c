static int red = 8;
static int grn = 8;
static int blu = 8;

/*****************************************************************
* I_set_color_levels (r, g, b)
*
* sets the red, green, blue color levels
*****************************************************************/
I_set_color_levels (r, g, b)
{
    if ((red = r) < 2) red = 2;
    if ((grn = g) < 2) grn = 2;
    if ((blu = b) < 2) blu = 2;
}

/*****************************************************************
* I_get_color_levels (r, g, b)
*   int *r, *g, *b;
*
* gets the red, green, blue color levels
*****************************************************************/
I_get_color_levels (r, g, b)
    int *r, *g, *b;
{
    *r = red;
    *g = grn;
    *b = blu;
}

/*****************************************************************
* I_color_n (r, g, b)
*
* converts red, green, blue color levels into a single color number
*****************************************************************/
I_color_n (r, g, b)
{
    r = r < 0 ? 0 : r >= red ? red - 1 : r ;
    g = g < 0 ? 0 : g >= grn ? grn - 1 : g ;
    b = b < 0 ? 0 : b >= blu ? blu - 1 : b ;

    return b + blu * (g + grn * r) ;
}

/*****************************************************************
* I_color_f (fr,fg,fb)
*   float fr,fg,fb;
*
* converts float red, green, blue color percentages
* into a single color number
*****************************************************************/
I_color_f (fr, fg, fb)
    float fr, fg, fb;
{
    int r,g,b;

    r = fr * (red-1) + .5;
    g = fg * (grn-1) + .5;
    b = fb * (blu-1) + .5;

    return I_color_n (r,g,b);
}

/**********************************************************
* I_init_color_levels (ncolors)
*
* given the number of colors available, sets the rgb
* color levels to the integer cube root
**********************************************************/
I_init_color_levels (ncolors)
    register int ncolors;
{
    register int i;
    int r,g,b;
    for (i = 2; i*i*i <= ncolors; i++)
	    ;
    r = g = b = i;

    if (r*g*b > ncolors)
    {
	b--;
	if (r*g*b > ncolors)
	{
	    g--;
	    if (r*g*b > ncolors)
		r--;
	}
    }

    I_set_color_levels (r,g,b);
}

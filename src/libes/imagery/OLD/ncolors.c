/*************************************************************
* I_ncolors()
*
* returns the total number of colors based on the
* current color levels
*************************************************************/
I_ncolors()
{
    int r,g,b;

    I_get_color_levels (&r,&g,&b);
    return r*g*b;
}

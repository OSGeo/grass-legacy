/* this is for a 125 color system */
Pcolornum (red, grn, blu)
    float red, grn, blu;
{
    int r,g,b;

    r = red * 5;
    if (r < 0) r = 0;
    if (r > 4) r = 4;

    g = grn * 5;
    if (g < 0) g = 0;
    if (g > 4) g = 4;

    b = blu * 5;
    if (b < 0) b = 0;
    if (b > 4) b = 4;

    return 25*r + 5*g + b;
}


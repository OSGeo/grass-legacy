/* %W% %G% */
/* this is for a 216 color system */
Pcolornum (red, grn, blu)
    float red, grn, blu;
{
    int r,g,b;

    r = red * 6;
    if (r < 0) r = 0;
    if (r > 5) r = 5;

    g = grn * 6;
    if (g < 0) g = 0;
    if (g > 5) g = 5;

    b = blu * 6;
    if (b < 0) b = 0;
    if (b > 5) b = 5;

    return 36*r + 6*g + b;
}


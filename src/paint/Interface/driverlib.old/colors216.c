/* for 216 colors */

Pcolorlevels (red, grn, blu)
    int *red, *grn, *blu;
{
    *red = 6;
    *grn = 6;
    *blu = 6;
}

Pcolormultipliers (red, grn, blu)
    int *red, *grn, *blu;
{
    *red = 36;
    *grn = 6;
    *blu = 1;
}

Pcolornum (red, green, blue)
    float red, green, blue ;
{
    register int r;
    register int g;
    register int b;

    r = red * 6.;
    g = green * 6.;
    b = blue * 6.;

    if (r < 0) r = 0;
    if (r > 5) r = 5;

    if (g < 0) g = 0;
    if (g > 5) g = 5;

    if (b < 0) b = 0;
    if (b > 5) b = 5;

    return (r*36 + g*6 + b);
}

Pcolorvalue (n, red, grn, blu)
    float *red, *grn, *blu;
{
    int r,g,b;

    if (n < 0) n = 0;
    if (n > 215) n = 215;

    r = n / 36 ;
    g = (n % 36) / 6;
    b = n % 6 ;

    *red = r / 5.0 ;
    *grn = g / 5.0 ;
    *blu = b / 5.0 ;
}

Pncolors()
{
    return 216;
}

/* for 125 colors */

Pcolorlevels (red, grn, blu)
    int *red, *grn, *blu;
{
    *red = 5;
    *grn = 5;
    *blu = 5;
}

Pcolormultipliers (red, grn, blu)
    int *red, *grn, *blu;
{
    *red = 25;
    *grn = 5;
    *blu = 1;
}

Pcolornum (red, green, blue)
    float red, green, blue ;
{
    register int r;
    register int g;
    register int b;

    r = red * 5.;
    g = green * 5.;
    b = blue * 5.;

    if (r < 0) r = 0;
    if (r > 4) r = 4;

    if (g < 0) g = 0;
    if (g > 4) g = 4;

    if (b < 0) b = 0;
    if (b > 4) b = 4;

    return (r*25 + g*5 + b);
}

Pcolorvalue (n, red, grn, blu)
    float *red, *grn, *blu;
{
    int r,g,b;

    if (n < 0) n = 0;
    if (n > 124) n = 124;

    r = n / 25 ;
    g = (n % 25) / 5;
    b = n % 5 ;

    *red = r / 4.0 ;
    *grn = g / 4.0 ;
    *blu = b / 4.0 ;
}

Pncolors()
{
    return 125;
}
